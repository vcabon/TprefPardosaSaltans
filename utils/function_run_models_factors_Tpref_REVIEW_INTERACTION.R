
run_models_factors_Tpref_interaction <- function(data, resp, dev) {
  
    model_factors_Tpref <- glmer(resp ~ (Length_mm + 
                                   Length_legs_mm + 
                                   Sex +
                                   UHI_03_04_pixel.x) * 
                                   dev + 
                                   (1|Session.x), 
                                 data,
                                 family=gaussian(link=identity),
                                 na.action = "na.fail")
    
    summary_model_factors_Tpref <- summary(model_factors_Tpref) 
    
    results_model_factors_Tpref <-Anova(model_factors_Tpref, type = 3)
    
    r2_model_factors_Tpref <- r2(model_factors_Tpref)
    
    selected_model <- dredge(model_factors_Tpref)
    
    print(cbind(summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3]))
    print(r2_model_factors_Tpref $R2_conditional)
    print(r2_model_factors_Tpref$R2_marginal)
    print(selected_model)
    
    list_outputs <- list()
    list_outputs[[1]] <- as.data.frame(print(cbind(row.names(summary_model_factors_Tpref$coefficients), summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3])))
    list_outputs[[2]] <- as.data.frame(print(r2_model_factors_Tpref$R2_conditional))
    list_outputs[[3]] <- as.data.frame(print(r2_model_factors_Tpref$R2_marginal))
    list_outputs[[4]] <- as.data.frame(print(selected_model))
    
    return(list_outputs)  
  }

