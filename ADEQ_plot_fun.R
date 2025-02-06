# Plotting function from lake_profiles_graphing project
# modified to work with app

site_plottingAR <- function(site_data) {
  
  site_data <- site_data %>% 
    mutate(across(Date:SiteID, as.character),
           across(Temp_Inst:Depth, as.numeric))
  
  site_id <- unique(site_data$SiteID)
  
  # Plotting:
  
  # DO:
  do_p <- ggplot(site_data) +
    aes(x = DO_Inst, y = Depth, color = Date) +
    geom_point() +
    geom_line(orientation = "y") +
    scale_y_reverse() +
    scale_x_continuous(position = "top")  +
    labs(x = "DO (mg/L)", y = "Depth (m)") +
    theme_classic(12)
  
  # pH:
  ph_p <- ggplot(site_data) + 
    aes(x=pH_Inst, y=Depth, color = Date) +
    geom_point() + 
    geom_line(orientation = "y") +
    scale_y_reverse() + 
    scale_x_continuous(position = "top")  +
    labs(x="pH", y="Depth (m)") +
    theme_classic(12)
  
  # Temp:
  temp_p <- ggplot(site_data) + 
    aes(x=Temp_Inst, y=Depth, color = Date) +
    geom_point() + 
    geom_line(orientation = "y") +
    scale_y_reverse() + 
    scale_x_continuous(position = "top")  +
    labs(x="Temp (*C)", y="Depth (m)") +
    theme_classic(12)
  
  # Display plots in a grid:
  grid.arrange(do_p, ph_p, temp_p, ncol = 2)
  
}
