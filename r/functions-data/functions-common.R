
library(ggplot2)

save_plots_png <- function(plots, prefix, outputdir = "../output/plots/", size_preset = "small") {
  plot_filename_prefix <- paste0(prefix, "_", Sys.Date(), "_")
  
  if (size_preset == "small") {
    preset_width <- 6
    preset_height <- 4
    preset_dpi <- 300
  } else {
    preset_width <- 12
    preset_height <- 8
    preset_dpi <- 150
  }
  
    
  for (item in 1:length(plots)) {
    filename <- paste0(outputdir,
                       plot_filename_prefix,
                       names(plots)[item],
                       ".png")
    ggsave(filename, plots[[item]],
           width = preset_width,
           height = preset_height,
           dpi = preset_dpi)  
  }
}
