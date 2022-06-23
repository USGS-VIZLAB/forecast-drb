merge_plot_legend <- function(main_plot, legend, out_file, show_all_predicted){
  
  # Add labels and annotation to legend before combining
  legend_updated <- legend +
    theme(strip.text = element_text(face = "bold", color = NA),
          panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.line = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
  
  # Draw a blank spot "NULL" to put legend
  plot_grid(main_plot)+
    # then add legend on top
    draw_plot(legend_updated, x = 0.75, y = 0.0, width = 0.093, height = 0.5)+
    # and annotate temperature threshold
    draw_label(expression('75'~degree*'F'), colour = "orangered", x = .865, y = .37, size = 8)+
    annotate("segment", xend = 0.82, x = 0.835, yend = 0.30, y = 0.34, 
             arrow = arrow(length = unit(0.1, "cm")), colour = "orangered")+
    draw_label("threshold", x = .873, y = .338, size = 6, colour = "orangered")+
    # and annotate mean
    draw_label("Mean", colour = "black", x = 0.864, y = 0.258 , size = 7)+
    annotate("segment", xend = 0.824, x = 0.837, yend = 0.258 , y = 0.258 , 
             arrow = arrow(length = unit(0.1, "cm")), colour = "black")+
    # and 90% CI
    draw_label("{", x = 0.82, y = 0.208, size = 19, angle = 180, color = "black")+
    draw_label("\u00B1 90% CI", colour = "black", x = 0.878, y = 0.225, size = 7)+
    # and complete values
    {if(show_all_predicted == TRUE){
      draw_label("     Predicted \nvalues", x = 0.93, y = 0.3, 
                 size = 6, color = "cornsilk3")
    } else {
      draw_label("     Predicted \nvalues", x = 0.93, y = 0.3, 
                 size = 6, color = "white")
    }
    }
    
  
  
  ggsave(filename = out_file,
         width = 1600, height = 900, dpi = 300, units = "px", bg = "white")
}
