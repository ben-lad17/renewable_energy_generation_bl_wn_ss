### Run cross validation and save output as dataframe for shiny app ###

# Clean the environment
rm(list=ls())

# load libraries
library(tidyverse)
library(tidymodels)
library(tidyverse)
library(here)
library(openxlsx)
library(readxl)
library(broom)




# Load data 
solar_2023 = read.csv(here("Output", "eia_solar_2016-2023.csv")) |>
  filter(year==2023)
wind_2023 = read.csv(here("Output", "eia_wind_2016-2023.csv")) |>
  filter(year==2023)


# Combine datasets and add energy_type column
energy_data <- bind_rows(
  solar_2023 |> mutate(energy_type = "solar"),
  wind_2023 |> mutate(energy_type = "wind")
)

year_range <- range(energy_data$operating_year, na.rm = TRUE)


# Prepare data for modeling
model_data = energy_data |>
  # Convert energy_type to a factor for classification
  mutate(energy_type = factor(energy_type)) |>
  # Select only the variables we need for modeling
  select(energy_type, state, operating_year, sector_name, nameplate_capacity_mw) |>
  # Remove any rows with NA values
  drop_na()

# Set seed for reproducibility
set.seed(42)

# Create 10-fold cross-validation splits, stratified by energy_type
folds = vfold_cv(model_data, v = 10, strata = energy_type)

# Define model specifications
log_spec = logistic_reg() |>
  set_engine("glm")

# Define recipes (model formulations)
recipe_1 = recipe(energy_type ~ state + operating_year + sector_name + nameplate_capacity_mw, 
                  data = model_data)

recipe_2 = recipe(energy_type ~ state + operating_year + nameplate_capacity_mw, 
                  data = model_data)

recipe_3 <- recipe(energy_type ~ state + nameplate_capacity_mw, 
                   data = model_data)

# Create workflows (recipe + model)
workflow_1 = workflow() |>
  add_recipe(recipe_1) |>
  add_model(log_spec)

workflow_2 = workflow() |>
  add_recipe(recipe_2) |>
  add_model(log_spec)

workflow_3 = workflow() |>
  add_recipe(recipe_3) |>
  add_model(log_spec)

# Fit models with resampling
results_1 = workflow_1 |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(accuracy, roc_auc, sens, spec)
  )

results_2 = workflow_2 |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(accuracy, roc_auc, sens, spec)
  )

results_3 = workflow_3 |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(accuracy, roc_auc, sens, spec)
  )



# Collect metrics for all models
metrics_1 <- collect_metrics(results_1) %>% mutate(model = "Model 1: All variables")
metrics_2 <- collect_metrics(results_2) %>% mutate(model = "Model 2: No sector")
metrics_3 <- collect_metrics(results_3) %>% mutate(model = "Model 3: No sector or year")

all_metrics <- bind_rows(metrics_1, metrics_2, metrics_3)

# Determine the best model based on accuracy
best_accuracy <- all_metrics %>%
  filter(.metric == "accuracy") %>%
  arrange(desc(mean)) %>%
  slice(1)

best_model_num <- case_when(
  best_accuracy$model == "Model 1: All variables" ~ 1,
  best_accuracy$model == "Model 2: No sector" ~ 2,
  best_accuracy$model == "Model 3: No sector or year" ~ 3
)

# Fit final model on all data
final_workflows <- list(workflow_1, workflow_2, workflow_3)
final_model_fit_1 <- final_workflows[[1]] |>
  fit(data = model_data)
final_model_fit_2 <- final_workflows[[2]] |>
  fit(data = model_data)
final_model_fit_3 <- final_workflows[[3]] |>
  fit(data = model_data)




# Generate predictions for each model
predictions_1 <- augment(final_model_fit_1, new_data = model_data)
predictions_2 <- augment(final_model_fit_2, new_data = model_data)
predictions_3 <- augment(final_model_fit_3, new_data = model_data)


library(yardstick)

# Compute confusion matrices
cm_1 <- conf_mat(predictions_1, truth = energy_type, estimate = .pred_class)
cm_2 <- conf_mat(predictions_2, truth = energy_type, estimate = .pred_class)
cm_3 <- conf_mat(predictions_3, truth = energy_type, estimate = .pred_class)


extract_metrics <- function(cm) {
  tbl <- as_tibble(cm$table)
  total_obs <- sum(tbl$n)
  correct_obs <- sum(tbl %>% filter(Prediction == Truth) %>% pull(n))
  incorrect_obs <- total_obs - correct_obs
  percent_correct <- (correct_obs / total_obs) * 100
  list(total_obs = total_obs, correct_obs = correct_obs,
       incorrect_obs = incorrect_obs, percent_correct = percent_correct)
}

cm_metrics_1 <- extract_metrics(cm_1)
cm_metrics_2 <- extract_metrics(cm_2)
cm_metrics_3 <- extract_metrics(cm_3)


cv_accuracy_df = as.data.frame(tibble(
  Model = c("Model 1: All variables", "Model 2: No sector", "Model 3: No sector or year"),
  Total_Observations = c(cm_metrics_1$total_obs, cm_metrics_2$total_obs, cm_metrics_3$total_obs),
  Correctly_Classified = c(cm_metrics_1$correct_obs, cm_metrics_2$correct_obs, cm_metrics_3$correct_obs),
  Incorrectly_Classified = c(cm_metrics_1$incorrect_obs, cm_metrics_2$incorrect_obs, cm_metrics_3$incorrect_obs),
  Percent_Correctly = c(cm_metrics_1$percent_correct, cm_metrics_2$percent_correct, cm_metrics_3$percent_correct)
))


write_csv(cv_accuracy_df, here("Output", "cv_accuracy_df.csv"))
