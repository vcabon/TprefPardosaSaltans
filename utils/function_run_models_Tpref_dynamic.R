# Define function to run models on Tpref dynamic

run_models_dynamic_Tpref <- function(x) {
  model_dynamic <- glmer(value ~ L1 + (1|V2), data = x)
  results_model_dynamic <-Anova(model_dynamic, type = 3)
  print(results_model_dynamic$`Pr(>Chisq)`[2]) # Only print the p-value
}