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

### create separate data sets for adults and juveniles
DATA_PARDOSES_30min_test_adults <- filter(DATA_PARDOSES_30min_test, 
                                          DATA_PARDOSES_30min_test$Dev_stage=="Adult")

DATA_PARDOSES_30min_test_juveniles <- filter(DATA_PARDOSES_30min_test, 
                                             DATA_PARDOSES_30min_test$Dev_stage=="Juvenile")

### create separate data sets for males and females
DATA_PARDOSES_30min_test_males <- filter(DATA_PARDOSES_30min_test, 
                                         DATA_PARDOSES_30min_test$Sex=="M")

DATA_PARDOSES_30min_test_females <- filter(DATA_PARDOSES_30min_test, 
                                           DATA_PARDOSES_30min_test$Sex=="F")

# ### create separate data sets for small and larges
# size_median = median(DATA_PARDOSES_30min_test$Length_mm)
# DATA_PARDOSES_30min_test_small <- filter(DATA_PARDOSES_30min_test, DATA_PARDOSES_30min_test$Length_mm<=size_median)
# DATA_PARDOSES_30min_test_large <- filter(DATA_PARDOSES_30min_test, DATA_PARDOSES_30min_test$Length_mm>size_median)


# Load functions to run models
source("utils/function_run_models_factors_Tpref.R")
source("utils/function_compare_models_HUI_Tpref.R")

## run models on Tpref range and print results 
res_Tpref_range_all <- run_models_factors_Tpref(DATA_PARDOSES_30min_test, 
                                                      DATA_PARDOSES_30min_test$SD, 
                                                      DATA_PARDOSES_30min_test$Dev_stage.x) # all individuals
write.csv(rbind.fill(res_Tpref_range_all), "./outputs/res_Tpref_range_all.csv")

res_Tpref_range_adults <- run_models_factors_Tpref(DATA_PARDOSES_30min_test_adults, 
                                                DATA_PARDOSES_30min_test_adults$SD) # only for adults
write.csv(rbind.fill(res_Tpref_range_adults), "./outputs/res_Tpref_range_adults.csv")

res_Tpref_range_juveniles <- run_models_factors_Tpref(DATA_PARDOSES_30min_test_juveniles, 
                                                      DATA_PARDOSES_30min_test_juveniles$SD) # only for juveniles
write.csv(rbind.fill(res_Tpref_range_juveniles), "./outputs/res_Tpref_range_juveniles.csv")
# compare_models_HUI_Tpref(DATA_PARDOSES_30min_test_juveniles, DATA_PARDOSES_30min_test_juveniles$SD) # only for juveniles -> compare effects of multiple UHI metrics

# run_models_factors_Tpref(DATA_PARDOSES_30min_test_small, DATA_PARDOSES_30min_test_small$SD, DATA_PARDOSES_30min_test_small$Dev_stage.x) # only for small individuals
# run_models_factors_Tpref(DATA_PARDOSES_30min_test_large, DATA_PARDOSES_30min_test_large$SD, DATA_PARDOSES_30min_test_large$Dev_stage.x) # only for large individuals

## run models on mean Tpref
res_Tpref_mean_all <- run_models_factors_Tpref(DATA_PARDOSES_30min_test, 
                                                DATA_PARDOSES_30min_test$mean, 
                                                DATA_PARDOSES_30min_test$Dev_stage.x) # all individuals
write.csv(rbind.fill(res_Tpref_mean_all), "./outputs/res_Tpref_mean_all.csv")

res_Tpref_mean_adults <- run_models_factors_Tpref(DATA_PARDOSES_30min_test_adults, 
                                                 DATA_PARDOSES_30min_test_adults$mean) # only for adults
write.csv(rbind.fill(res_Tpref_median_adults), "./outputs/res_Tpref_mean_adults.csv")

res_Tpref_mean_juveniles <- run_models_factors_Tpref(DATA_PARDOSES_30min_test_juveniles, 
                                                       DATA_PARDOSES_30min_test_juveniles$mean) # only for juveniles
write.csv(rbind.fill(res_Tpref_mean_juveniles), "./outputs/res_Tpref_mean_juveniles.csv")

# run_models_factors_Tpref(DATA_PARDOSES_30min_test_small, DATA_PARDOSES_30min_test_small$median, DATA_PARDOSES_30min_test_small$Dev_stage.x) # only for small individuals
# run_models_factors_Tpref(DATA_PARDOSES_30min_test_large, DATA_PARDOSES_30min_test_large$median, DATA_PARDOSES_30min_test_large$Dev_stage.x) # only for large individuals
