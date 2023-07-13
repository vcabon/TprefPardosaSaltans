# Appendix 

source("utils/function_regression_plot.R")
source("utils/function_box_plot.R")

# development stage effect on Tpref range 

plot_SD_devStage <- box_plot(DATA_PARDOSES_30min_test[-125,], DATA_PARDOSES_30min_test[-125,]$Dev_stage.x, "Development stage", DATA_PARDOSES_30min_test[-125,]$SD, "Tpref range (°C)")

# UHI effect on body size

shapiro.test(mod_body_size$residuals)
cor.test(DATA_PARDOSES_30min_test$Length_mm,DATA_PARDOSES_30min_test$UHI_03_04_pixel, method="spearman")

Plot_UHI_body <-regression_plot(DATA_PARDOSES_30min_test, DATA_PARDOSES_30min_test$UHI_03_04_pixel.x, "UHI intensity (°C)", DATA_PARDOSES_30min_test$Length_mm, "body size (mm)", DATA_PARDOSES_30min_test$Sex, "Sex")

# sex effect on body size
shapiro.test(DATA_PARDOSES_30min_test_females$Length_mm)
shapiro.test(DATA_PARDOSES_30min_test_males$Length_mm)
wilcox.test(DATA_PARDOSES_40min_ID95_males$Length_mm, DATA_PARDOSES_40min_ID95_females$Length_mm)

plot_sex_size <- box_plot(DATA_PARDOSES_30min_test, DATA_PARDOSES_30min_test$Sex, "Sex", DATA_PARDOSES_30min_test$Length_mm, "Body size (mm)")

# comparison of UHI metrics  

colors <- c("UHI (September 2021 - April 2022)" = "red", "UHI (March 2022 - April 2022)" = "green", "UHI (December 2021 - February 2022)" = "blue")
Plot_UHI <- ggplot(DATA_PARDOSES_30min_test_juveniles, aes_string(x = DATA_PARDOSES_30min_test_juveniles$UHI_09_04_pixel , y = DATA_PARDOSES_30min_test_juveniles$SD )) +
  geom_ribbon(aes(x = DATA_PARDOSES_30min_test_juveniles$UHI_09_04_pixel , y = DATA_PARDOSES_30min_test_juveniles$SD), inherit.aes = FALSE,stat = "smooth", method = "glm", fill="lightpink", alpha=0.3) +
  stat_smooth(aes(x = DATA_PARDOSES_30min_test_juveniles$UHI_09_04_pixel , y = DATA_PARDOSES_30min_test_juveniles$SD, color = "UHI (September 2021 - April 2022)"),method="glm",se=FALSE, lwd= 1.2) +
  geom_ribbon(aes(x = DATA_PARDOSES_30min_test_juveniles$UHI_03_04_pixel , y = DATA_PARDOSES_30min_test_juveniles$SD), inherit.aes = TRUE,stat = "smooth", method = "glm", fill="lightgreen", alpha=0.3) +
  stat_smooth(aes(x = DATA_PARDOSES_30min_test_juveniles$UHI_03_04_pixel , y = DATA_PARDOSES_30min_test_juveniles$SD, color = "UHI (March 2022 - April 2022)"),method="glm",se=FALSE, lwd= 1.2) +
  geom_ribbon(aes(x = DATA_PARDOSES_30min_test_juveniles$UHI_12_02_pixel , y = DATA_PARDOSES_30min_test_juveniles$SD), inherit.aes = TRUE,stat = "smooth", method = "glm", fill="lightblue", alpha=0.3) +
  stat_smooth(aes(x = DATA_PARDOSES_30min_test_juveniles$UHI_12_02_pixel , y = DATA_PARDOSES_30min_test_juveniles$SD, color = "UHI (December 2021 - February 2022)"),method="glm",se=FALSE, lwd= 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="UHI intensity (°C)", y= "Tpref range (°C) ", color = "Legend") +
  scale_color_manual(values = colors) + ggtitle('Juveniles')

# comparison of UHI metrics  - models

model_Tpref_range_UHI_09_02 <- glmer((SD) ~ Dev_stage.x +
                             Sex +
                             UHI_09_04_pixel.x + 
                             (Dev_stage.x*Sex)+
                             (Dev_stage.x*UHI_09_04_pixel.x)+
                             (1|Session.x), 
                           DATA_PARDOSES_30min_test,
                           family=gaussian(link=identity))

summary(model_Tpref_range_UHI_09_02) 
r2(model_Tpref_range_UHI_09_02)

model_Tpref_range_UHI_12_02 <- glmer((SD) ~ Dev_stage.x +
                                       Sex +
                                       UHI_12_02_pixel.x + 
                                       (Dev_stage.x*Sex)+
                                       (Dev_stage.x*UHI_12_02_pixel.x)+
                                       (1|Session.x), 
                                     DATA_PARDOSES_30min_test,
                                     family=gaussian(link=identity))

summary(model_Tpref_range_UHI_12_02) 
r2(model_Tpref_range_UHI_12_02)

