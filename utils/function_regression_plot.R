
regression_plot <- function(data, xabs, xabs_name, yord, yord_name, Sex, group_var_name) {
  regression_plot <- ggplot(data, aes(x = xabs , y = yord, group= Sex)) +
  geom_point(aes(shape = Sex), size = 2.5) +
  scale_shape_manual(values=c(1, 16)) +
  geom_ribbon(stat = "smooth", method = "glm", fill="grey", alpha = 0.8, aes(group=1)) +
  stat_smooth(method = "glm", se= FALSE, color="black", lwd= 1, aes(group=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.key=element_blank()) 
  regression_plot <- regression_plot +labs(x= xabs_name, y= yord_name) + guides(fill=guide_legend(title= group_var_name))
  regression_plot <- regression_plot + labs(fill = group_var_name)
  regression_plot
}


