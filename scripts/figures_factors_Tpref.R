# plot results 

source("utils/function_regression_plot.R")
source("utils/function_box_plot.R")

# development stage effect on Tpref range 

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

plot_SD_devStage <- plot_SD_devStage + ggtitle('C')

# Body size

Plot_mean_body <- regression_plot(DATA_PARDOSES_30min_test, 
                                  DATA_PARDOSES_30min_test$Length_mm, 
                                  "Prosoma length (mm)", 
                                  DATA_PARDOSES_30min_test$mean, 
                                  "mean Tpref (°C)", 
                                  DATA_PARDOSES_30min_test$Sex, 
                                  "Sex")

Plot_mean_body <- Plot_mean_body + ggtitle('A')

Plot_mean_body_adults <- regression_plot(DATA_PARDOSES_30min_test_adults, 
                                         DATA_PARDOSES_30min_test_adults$Length_mm, 
                                         "Prosoma length (mm)", 
                                         DATA_PARDOSES_30min_test_adults$mean, 
                                         "mean Tpref (°C)", 
                                         DATA_PARDOSES_30min_test_adults$Sex, "Sex")

Plot_mean_body_adults <- Plot_mean_body_adults + ggtitle('D')

Plot_SD_body <- regression_plot(DATA_PARDOSES_30min_test[-125,], 
                                DATA_PARDOSES_30min_test[-125,]$Length_mm, 
                                "Prosoma length (mm)", 
                                DATA_PARDOSES_30min_test[-125,]$SD, 
                                "Tpref range (°C)", 
                                DATA_PARDOSES_30min_test[-125,]$Sex, 
                                "Sex")

Plot_SD_body <- Plot_SD_body + ggtitle('B')

# Sex

Plot_mean_sex <- box_plot(DATA_PARDOSES_30min_test_adults, 
                          DATA_PARDOSES_30min_test_adults$Sex, 
                          "Sex", 
                          DATA_PARDOSES_30min_test_adults$mean, 
                          "Mean Tpref (°C)")

Plot_mean_sex <- Plot_mean_sex + ggtitle('E')

Plot_SD_sex <- box_plot(DATA_PARDOSES_30min_test_adults, 
                        DATA_PARDOSES_30min_test_adults$Sex, 
                        "Sex", 
                        DATA_PARDOSES_30min_test_adults$SD, 
                        "Tpref range (°C)")

Plot_SD_sex <- Plot_SD_sex + ggtitle('F')

# UHI

Plot_SD_UHI <- regression_plot(DATA_PARDOSES_30min_test_juveniles[-49,], 
                               DATA_PARDOSES_30min_test_juveniles[-49,]$UHI_03_04_pixel.x, 
                               "UHI intensity (°C)", 
                               DATA_PARDOSES_30min_test_juveniles[-49,]$SD, 
                               "Tpref range (°C)", 
                               DATA_PARDOSES_30min_test_juveniles[-49,]$Sex, 
                               "Sex")

Plot_SD_UHI <- Plot_SD_UHI + ggtitle('G')

# Empty plot
plot_blank <- ggplot() + theme_void()

# Create figure

fig2 <- (Plot_mean_body | Plot_SD_body | plot_SD_devStage ) / 
  (Plot_mean_body_adults | Plot_mean_sex | Plot_SD_sex ) / 
  (Plot_SD_UHI | plot_blank | plot_blank) + 
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = list(c('                All individuals', '', '', 
                                      '     Adults', '', '', 
                                      '         Juveniles', '', ''))) & 
  theme(plot.tag = element_text(size = 18))
fig2

# Export as jpeg file
jpeg("outputs/fig2.jpg", width = 3200, height = 2400, res = 300)
fig2
dev.off()
