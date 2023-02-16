box_plot <- function(data, xabs, xabs_name, yord, yord_name) {

  box_plot <- ggplot(data, aes(x = xabs, y = yord)) +
    geom_boxplot(fill="grey", alpha=0.8, outlier.shape = NA) + 
    geom_sina(size=2.5, alpha=1, aes(shape=xabs)) + 
    scale_shape_manual(values=c(1, 16)) +
    scale_color_manual(values =  c("grey", "grey")) + 
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),   panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    labs(x = xabs_name,y =  yord_name)

  box_plot
}



