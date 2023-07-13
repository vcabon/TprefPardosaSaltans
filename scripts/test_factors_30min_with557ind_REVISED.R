# format Tpref data set to keep only data from the last 60 minutes 
# and transpose individual Tpref data to columns
d_temp_30min =as.data.frame(t(test1_ID95[,c(1, 39:(ncol(test1_ID95)))]))

names(d_temp_30min) <- d_temp_30min[1,]
d_temp_30min <- d_temp_30min[-1,]
d_temp_30min <- apply( d_temp_30min,2, as.numeric )
d_temp_30min <- as.data.frame(d_temp_30min)

# create empty data frame to compile mean and SD values 
# from fitted gaussian distributions  
sortie2=data.frame('Identifiant'=rep(0,1), 'mean'=rep(0,1), 'SD'=rep(0,1))

# Fit gaussian distribution to each individual (column) 
# and save results (mean and SD) in the table "sortie2"
for (i in 1:length(d_temp_30min)){
  fn2 <- fitdist(c(na.exclude(d_temp_30min[,i])), "norm", method=c("mme")) 
  Me2=summary(fn2)$estimate[1] # mean
  Sd2=summary(fn2)$estimate[2] # sd
  
  sortie2[i,1]=names(d_temp_30min[i])
  sortie2[i,2]=Me2
  sortie2[i,3]=Sd2
}

# combine all individuals' information in a single table 
DATA_complet_30min_test <- test1_ID95[, -c(10:(ncol(test1_ID95)))] # set base table and remove single Tpref measurements 

DATA_complet_30min_test <- inner_join(DATA_complet_30min_test, 
                                      sortie2, 
                                      by="Identifiant") # merge mean and SD values  

DATA_PARDOSES_30min_test <- inner_join(DATA_complet_30min_test, 
                                       d_indiv, 
                                       by = "Identifiant") # merge individuals information

DATA_PARDOSES_30min_test <- DATA_PARDOSES_30min_test[,-20]
DATA_PARDOSES_30min_test$Session <- as.factor(DATA_PARDOSES_30min_test$Session) 
DATA_PARDOSES_30min_test$Text <- as.numeric(DATA_PARDOSES_30min_test$Text)
DATA_PARDOSES_30min_test$Poids <- as.numeric(DATA_PARDOSES_30min_test$Poids)

DATA_PARDOSES_30min_test <- inner_join(DATA_PARDOSES_30min_test, 
                                       length_cephalothorax, 
                                       by = "Identifiant") # Merge morphological data (body size)

DATA_PARDOSES_30min_test <- inner_join(DATA_PARDOSES_30min_test, 
                                       length_legs, 
                                       by = "Identifiant") # Merge morphological data (legs' length)

DATA_PARDOSES_30min_test <- DATA_PARDOSES_30min_test[
  !duplicated(DATA_PARDOSES_30min_test$Identifiant), ] # remove duplicates 


# Load functions to run models
source("utils/function_run_models_factors_Tpref_REVIEW_INTERACTION.R")
source("utils/function_compare_models_HUI_Tpref.R")

## seect models on Tpref range by dredging
res_Tpref_range_all <- run_models_factors_Tpref_interaction(DATA_PARDOSES_30min_test, 
                                                DATA_PARDOSES_30min_test$SD, 
                                                DATA_PARDOSES_30min_test$Dev_stage.x) # all individuals
write.csv(rbind.fill(res_Tpref_range_all), "./outputs/res_Tpref_range_all.csv")

## seect models on mean Tpref by dredging
res_Tpref_mean_all <- run_models_factors_Tpref_interaction(DATA_PARDOSES_30min_test, 
                                               DATA_PARDOSES_30min_test$mean, 
                                               DATA_PARDOSES_30min_test$Dev_stage.x) # all individuals
write.csv(rbind.fill(res_Tpref_mean_all), "./outputs/res_Tpref_mean_all.csv")

# Test R2 for individual variables 
r2(lm(SD ~ Length_mm + 
        Length_legs_mm + 
        UHI_03_04_pixel.x + 
        Sex, 
      data = DATA_PARDOSES_30min_test_juveniles))

r2(lm(SD ~ Length_mm, data = DATA_PARDOSES_30min_test_juveniles))
r2(lm(SD ~ Length_legs_mm, data = DATA_PARDOSES_30min_test_juveniles))
r2(lm(SD ~ UHI_03_04_pixel.x, data = DATA_PARDOSES_30min_test_juveniles))
r2(lm(SD ~ Sex, data = DATA_PARDOSES_30min_test_juveniles))

#Run dredging-selected models on range
model_Tpref_range <- glmer(SD ~ Dev_stage.x +
                             Sex +
                             UHI_03_04_pixel.x + 
                             (Dev_stage.x*Sex)+
                             (Dev_stage.x*UHI_03_04_pixel.x)+
                             (1|Session.x), 
                           DATA_PARDOSES_30min_test,
                           family=gaussian(link=identity))

summary(model_Tpref_range) 
r2(model_Tpref_range)

# Post-hoc interactions dev_stage:sex
emmeans_mean<- emmeans(model_Tpref_range, list(~ Dev_stage.x * Sex), adjust = "tukey")
summary(pairs(lsm_range), type = "response")

# Post-hoc interactions UHI
summary(lm(SD~ UHI_03_04_pixel.x, data = DATA_PARDOSES_30min_test_adults))
r2(lm(SD~ UHI_03_04_pixel.x, data = DATA_PARDOSES_30min_test_adults))
summary(lm(SD~ UHI_03_04_pixel.x, data = DATA_PARDOSES_30min_test_juveniles))
r2(lm(SD~ UHI_03_04_pixel.x, data = DATA_PARDOSES_30min_test_juveniles))

#Run dredging-selected models on mean
model_Tpref_mean <- glmer((mean) ~ Dev_stage.x +
                            Length_mm +
                            Sex +
                            (Dev_stage.x*Sex)+
                            (1|Session.x), 
                           DATA_PARDOSES_30min_test,
                           family=gaussian(link=identity))

summary(model_Tpref_mean) 
r2(model_Tpref_mean)
# Post-hoc interactions dev_stage:sex
emmeans_mean<- emmeans(model_Tpref_mean, list(~ Dev_stage.x * Sex), adjust = "tukey")
summary(pairs(emmeans_mean), type = "response")
