# plot results 

source("utils/function_regression_plot.R")
source("utils/function_box_plot.R")

# Mean Tpref - Sex

Plot_mean_sex <- box_plot(DATA_PARDOSES_30min_test, 
                          DATA_PARDOSES_30min_test$Sex, 
                          "Sex", 
                          DATA_PARDOSES_30min_test$mean, 
                          "Mean Tpref (°C)")
Plot_mean_sex <- Plot_mean_sex + ggtitle('A')

# Tpref range - Sex

Plot_SD_sex <- box_plot(DATA_PARDOSES_30min_test[-125,], 
                        DATA_PARDOSES_30min_test[-125,]$Sex, 
                        "Sex", 
                        DATA_PARDOSES_30min_test[-125,]$SD, 
                        "Tpref range (°C)")

Plot_SD_sex <- Plot_SD_sex + ggtitle('B')

# mean Tpref - sex : development stage interaction 

plot_mean_devStage <- ggplot(DATA_PARDOSES_30min_test[-125,], 
                           aes(x = DATA_PARDOSES_30min_test[-125,]$Dev_stage.x, 
                               y = DATA_PARDOSES_30min_test[-125,]$mean)) +
  geom_boxplot(fill="grey", 
               alpha=0.8, 
               outlier.shape = NA) + 
  geom_sina(size=2.5, 
            alpha=1, 
            aes(shape=DATA_PARDOSES_30min_test[-125,]$Sex)) + 
  scale_shape_manual(values=c(1, 16)) +
  scale_color_manual(values =  c("grey", "grey")) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),   
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  labs(x = "Developmental stage",y =  "mean Tpref (°C)")

plot_mean_devStage <- plot_mean_devStage + ggtitle('C')

# Tpref range - sex : development stage interaction 

plot_SD_devStage <- ggplot(DATA_PARDOSES_30min_test[-125,], 
                           aes(x = DATA_PARDOSES_30min_test[-125,]$Dev_stage.x, 
                               y = DATA_PARDOSES_30min_test[-125,]$SD)) +
  geom_boxplot(fill="grey", 
               alpha=0.8, 
               outlier.shape = NA) + 
  geom_sina(size=2.5, 
            alpha=1, 
            aes(shape=DATA_PARDOSES_30min_test[-125,]$Sex)) + 
  scale_shape_manual(values=c(1, 16)) +
  scale_color_manual(values =  c("grey", "grey")) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),   
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  labs(x = "Developmental stage",y =  "Tpref range (°C)")

plot_SD_devStage <- plot_SD_devStage + ggtitle('D')

# Mean Tpref - Prosoma length

Plot_mean_body <- regression_plot(DATA_PARDOSES_30min_test, 
                                  DATA_PARDOSES_30min_test$Length_mm, 
                                  "Prosoma length (mm)", 
                                  DATA_PARDOSES_30min_test$mean, 
                                  "mean Tpref (°C)", 
                                  DATA_PARDOSES_30min_test$Sex, 
                                  "Sex")
Plot_mean_body <- Plot_mean_body + ggtitle('E')

# Tpref range - UHI

Plot_range_UHI <- ggplot(DATA_PARDOSES_30min_test[-125,]) +
  aes(x=UHI_03_04_pixel.x, y=SD) +
  geom_point(aes(shape = Sex), size = 2.5) +
  scale_shape_manual(values=c(1, 16)) +
  geom_smooth(method = "glm", 
              data = DATA_PARDOSES_30min_test_adults, 
              color = "black") +
  geom_smooth(method = "glm", 
              data = DATA_PARDOSES_30min_test_juveniles,
              color = "black", linetype = "dashed") +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),   
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  # theme(panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank(), 
  #       panel.background = element_blank(), 
  #       axis.line = element_line(colour = "black")) +
  labs(x="UHI intensity (°C)", y= "Tpref range (°C) ", color = "Legend") +
  ggtitle('F')

# Empty plot
plot_blank <- ggplot() + theme_void()

# Create figure

fig2 <- (Plot_mean_sex | Plot_SD_sex) / 
  (plot_mean_devStage | plot_SD_devStage) / 
  (Plot_mean_body | Plot_range_UHI) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = list(c('', '',
                                      '', '',
                                      '', ''))) +
  theme(plot.tag = element_text(size = 18))
fig2

# Export as jpeg file
jpeg("outputs/fig2_revised.jpg", width = 2000, height = 2400, res = 300)
fig2
dev.off()
# Export as svg file
svg("outputs/fig2_revised.svg", width = 2000, height = 2400)
fig2
dev.off()

# Figure revisions
fig_rev <- Plot_Mean_Text | Plot_Mean_Text_adults
