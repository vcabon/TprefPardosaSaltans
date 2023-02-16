# calculate median Tpref on 90min experiment
test1$median=apply(test1[,10:99], 1, median, na.rm=TRUE) 

### Fit gaussian distribution to median Tpref and calculate 95% dispersion interval 
fn <- (fitdist(c(na.exclude(test1$median)), "norm", method=c("mme")))
m=summary(fn)$estimate[1] # mean
s=summary(fn)$estimate[2] # SD
N=nrow(test1)
p=0.95
t95=qt((1+p)/2,df=N-1)
ID95=c(m-t95*s,m+t95*s)

### filter median Tpref data distributed in the 95% dispertion interval 
test1_ID95 <-  test1 %>% filter(between(test1$median, ID95[1], ID95[2]))
test1_ID95 <- test1_ID95[!duplicated(test1_ID95$Identifiant), ]

# Create a list containing averaged Tpref values by 10 minutes slots 
Dynamique_list_test <- list(test1_ID95[,c(10:19)], test1_ID95[,c(20:29)], test1_ID95[,c(30:39)], test1_ID95[,c(40:49)], test1_ID95[,c(50:59)], test1_ID95[,c(60:69)], test1_ID95[,c(70:79)], test1_ID95[,c(80:89)], test1_ID95[,c(90:99)])

means_tpref_list_test <- list()
for (i in 1:9) { 
  colnames(Dynamique_list_test[[i]]) = colnames(Dynamique_list_test[[1]])
  means_tpref_list_test[[i]] <- rowMeans(Dynamique_list_test[[i]])
  means_tpref_list_test[[i]] <- as.data.frame(means_tpref_list_test[[i]])
  means_tpref_list_test[[i]][,2] <- test1_ID95$Identifiant
  }

# Format data frames to test effect of time
means_tpref_melt_test <- melt.list(means_tpref_list_test)
means_tpref_melt_test$L1 <- as.factor(means_tpref_melt_test$L1)

time_sequences <- list()
time_sequences$means_tpref_melt_test_0min <- means_tpref_melt_test
time_sequences$means_tpref_melt_test_10min <- filter(means_tpref_melt_test, L1 != 1)
time_sequences$means_tpref_melt_test_20min <- filter(time_sequences$means_tpref_melt_test_10min, L1 != 2)
time_sequences$means_tpref_melt_test_30min <- filter(time_sequences$means_tpref_melt_test_20min, L1 != 3)
time_sequences$means_tpref_melt_test_40min <- filter(time_sequences$means_tpref_melt_test_30min, L1 != 4)


# test effect of time (GLMM) on multiple time intervals to indentify the breakpoint from which Tpref is stabilized

source("utils/function_run_models_Tpref_dynamic.R")
models_dynamic_Tpref <- lapply(time_sequences, run_models_dynamic_Tpref)

