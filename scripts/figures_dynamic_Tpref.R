# Plot results dynamic Tpref

## Plot boxplot 90min
means_tpref_melt_test <- mutate(means_tpref_melt_test, L2 =
                                  case_when(means_tpref_melt_test$L1 == 1 ~ "0-10", 
                                            means_tpref_melt_test$L1 == 2 ~ "11-20",
                                            means_tpref_melt_test$L1 == 3 ~ "21-30",
                                            means_tpref_melt_test$L1 == 4 ~ "31-40",
                                            means_tpref_melt_test$L1 == 5 ~ "41-50",
                                            means_tpref_melt_test$L1 == 6 ~ "51-60",
                                            means_tpref_melt_test$L1 == 7 ~ "61-70",
                                            means_tpref_melt_test$L1 == 8 ~ "71-80",
                                            means_tpref_melt_test$L1 == 9 ~ "81-90"))

p_tpref_time_test <- ggplot(means_tpref_melt_test, aes(factor(L2), value)) +
  geom_boxplot(alpha=0.5, outlier.shape = NA) + 
  geom_vline(xintercept="31-40", linetype="dashed", size = 1) +
  stat_summary(fun=median, geom="line", aes(group=1), col="black", size = 1.5, alpha = 1) + 
  geom_sina(size=2, alpha=1/15) + scale_color_manual(values =  c("grey", "grey")) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x = "Time interval (min)",y =  "Mean Tpref (°C)") 
p_tpref_time_test

## Plot histogram of Tpref distribution over the last 60 minutes 
test_ID95_30min <- test1_ID95[,c(40:100)]

hist_test_ID95_30min <- ggplot(test_ID95_30min, aes(x=median)) + 
  geom_histogram(binwidth=1) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Tpref (°C)",y =  "Count")

## Merge boxplot and histogram  
fig1 <-  p_tpref_time_test + hist_test_ID95_30min + plot_annotation(tag_levels = 'A')
fig1

# Export as jpeg file
jpeg("outputs/fig1.jpg", width = 850, height = 500)
fig1
dev.off()

