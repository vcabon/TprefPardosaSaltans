
compare_models_HUI_Tpref <- function(data, resp, dev) {
  
  if(missing(dev)) {
    
    # run model with UHI_03_04_pixel
    model_factors_Tpref <- glmer(resp ~ Length_mm + 
                                   Length_legs_mm + 
                                   UHI_03_04_pixel.x * Sex + 
                                   (1|Session.x), 
                                 data,
                                 family=gaussian(link=identity))
    
    summary_model_factors_Tpref <- summary(model_factors_Tpref) 
    
    results_model_factors_Tpref <-Anova(model_factors_Tpref, type = 3)
    
    r2_model_factors_Tpref <- r2(model_factors_Tpref)
    
    print("UHI : March to April 2022")
    print(cbind(summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3]))
    print(r2_model_factors_Tpref $R2_conditional)
    
    
    # run model with UHI_06_06_pixel
    model_factors_Tpref <- glmer(resp ~ Length_mm + 
                                   Length_legs_mm + 
                                   UHI_09_04_pixel.x * Sex + 
                                   (1|Session.x), 
                                 data,
                                 family=gaussian(link=identity))
    
    summary_model_factors_Tpref <- summary(model_factors_Tpref) 
    
    results_model_factors_Tpref <-Anova(model_factors_Tpref, type = 3)
    
    r2_model_factors_Tpref <- r2(model_factors_Tpref)
    
    print("UHI : September 2021 to April 2022")
    print(cbind(summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3]))
    print(r2_model_factors_Tpref $R2_conditional)
    
    
    # run model with UHI_12_3_pixel.x
    model_factors_Tpref <- glmer(resp ~ Length_mm + 
                                   Length_legs_mm + 
                                   UHI_12_02_pixel.x * Sex + 
                                   (1|Session.x), 
                                 data,
                                 family=gaussian(link=identity))
    
    summary_model_factors_Tpref <- summary(model_factors_Tpref) 
    
    results_model_factors_Tpref <-Anova(model_factors_Tpref, type = 3)
    
    r2_model_factors_Tpref <- r2(model_factors_Tpref)
    
    print("UHI : December 2021 to February 2022")
    print(cbind(summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3]))
    print(r2_model_factors_Tpref $R2_conditional)
  }
  
  else {
    # run model with UHI_03_04_pixel
    model_factors_Tpref <- glmer(resp ~ Length_mm + 
                                   Length_legs_mm + 
                                   dev +
                                   UHI_03_04_pixel.x * Sex + 
                                   (1|Session.x), 
                                 data,
                                 family=gaussian(link=identity))
    
    summary_model_factors_Tpref <- summary(model_factors_Tpref) 
    
    results_model_factors_Tpref <-Anova(model_factors_Tpref, type = 3)
    
    r2_model_factors_Tpref <- r2(model_factors_Tpref)
    
    print("UHI : March to April 2022")
    print(cbind(summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3]))
    print(r2_model_factors_Tpref $R2_conditional)
    
    
    # run model with UHI_06_06_pixel
    model_factors_Tpref <- glmer(resp ~ Length_mm + 
                                   Length_legs_mm + 
                                   dev +
                                   UHI_09_04_pixel.x * Sex + 
                                   (1|Session.x), 
                                 data,
                                 family=gaussian(link=identity))
    
    summary_model_factors_Tpref <- summary(model_factors_Tpref) 
    
    results_model_factors_Tpref <-Anova(model_factors_Tpref, type = 3)
    
    r2_model_factors_Tpref <- r2(model_factors_Tpref)
    
    print("UHI : September 2021 to April 2022")
    print(cbind(summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3]))
    print(r2_model_factors_Tpref $R2_conditional)
    
    
    # run model with UHI_12_3_pixel.x
    model_factors_Tpref <- glmer(resp ~ Length_mm + 
                                   Length_legs_mm + 
                                   dev +
                                   UHI_12_02_pixel.x * Sex + 
                                   (1|Session.x), 
                                 data,
                                 family=gaussian(link=identity))
    
    summary_model_factors_Tpref <- summary(model_factors_Tpref) 
    
    results_model_factors_Tpref <-Anova(model_factors_Tpref, type = 3)
    
    r2_model_factors_Tpref <- r2(model_factors_Tpref)
    
    print("UHI : December 2021 to February 2022")
    print(cbind(summary_model_factors_Tpref$coefficients[,-3], results_model_factors_Tpref[3]))
    print(r2_model_factors_Tpref $R2_conditional)
  }
  
}
