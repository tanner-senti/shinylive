# Tanner Senti
# 2024-12-13

# Original profile plotting code from wqTools by Utah DEQ
#This version removes logger line and custom units

# TESTING:
# test_data <- read.csv("modified_app/data/long_prof_test.csv")
# test_data <- test_data %>%
#   filter(ActivityIdentifier == "LARK014A-2023-04-17")
# test_data$Time <- as.factor(test_data$Time)
# test_data <- test_data %>% 
#   rename(test = DataLoggerLine, DataLoggerLine = Time)
# 
# data = test_data
# parameter = "Parameter"
# depth = "Depth (m)"
# do = "DO (mg/L)"
# temp = "Temp (*C)"
# pH = "pH"
# value_var = "IR_Value"
# line_no = "DataLoggerLine"



profilePlotAR <- function(data,
                          parameter = "CharacteristicName",
                          #units = "ResultMeasure.MeasureUnitCode",
                          depth = "Depth, data-logger (ported)",
                          do = "Dissolved oxygen (DO)",
                          temp = "Temperature, water",
                          pH = "pH",
                          value_var = "ResultMeasureValue",
                          line_no = "Time",
                          do_crit,
                          temp_crit,
                          pH_crit)
{
  data = data[!is.na(data[, line_no]),]
  data = droplevels(data[data[, parameter] %in% c(depth, do,
                                                  temp, pH), ])
  if (all(data[, parameter] != depth)) {
    stop("No depth values associated with this profile")
  }
  param_names = data.frame(rbind(depth, do, temp,  pH))
  colnames(param_names)[1] = paste(parameter)
  param_names$param_name = row.names(param_names)
  data = merge(data, param_names, all.x = T)
  
  # if (any(length(unique(data[, units][data[, parameter] ==
  #                                     do])) > 1,
  #         length(unique(data[, units][data[, parameter] ==
  #                                     depth])) > 1,
  #         length(unique(data[, units][data[, parameter] ==
  #                                     temp])) > 1,
  #         length(unique(data[, units][data[, parameter] ==
  #                                     pH])) > 1)) {
  #   stop("Mixed units for one or more parameter present. Convert units as appropriate.")
  #   print(table(data[, parameter], data[, units]))
  # }
  
  # unit_table = unique(data[, c("param_name", units)])
  
  prof_matrix = reshape2::dcast(
    SiteID +
      ActivityIdentifier + Date
     + get(line_no) ~
      param_name,
    value.var = paste(value_var),
    fun.aggregate = mean,
    data = data
  )
  prof_matrix = prof_matrix[order(prof_matrix$depth),]
  tc_data = aggregate(temp ~ depth, data = prof_matrix, FUN = "mean")
  tc_depth = rLakeAnalyzer::thermo.depth(tc_data$temp, tc_data$depth)
  par(mar = c(7.1, 5.1, 3.1, 2.1))
  plot(
    depth ~ temp,
    prof_matrix,
    ylim = rev(range(depth)),
    xlim = c(min(data[data$param_name != "depth", value_var],
                 na.rm = T), max(data[data$param_name != "depth",
                                      value_var], na.rm = T)),
    pch = NA,
    xlab = "",
    ylab = "Depth (m)",
    # ylab = paste0("Depth (",
    #               unit_table[unit_table$param_name == "depth", 2],
    #               ")"),
    xaxt = "n",
    cex.axis = 1.25,
    cex.lab = 1.5
  )
  axis(3, cex.axis = 1.25)
  # abline(
  #   h = tc_depth,
  #   col = "purple",
  #   lwd = 2,
  #   lty = 2
  # )
  abline(h = 0, lwd = 1, lty = 3)
  if (!missing(do_crit)) {
    abline(
      v = do_crit,
      col = "deepskyblue3",
      lty = 3,
      lwd = 3
    )
  }
  if (!missing(temp_crit)) {
    abline(
      v = temp_crit,
      col = "orange",
      lty = 3,
      lwd = 3
    )
  }
  if (!missing(pH_crit)) {
    abline(
      v = pH_crit[1],
      col = "green",
      lty = 3,
      lwd = 3
    )
  }
  if (!missing(pH_crit)) {
    abline(
      v = pH_crit[2],
      col = "green",
      lty = 3,
      lwd = 3
    )
  }
  if (any(!is.na(prof_matrix$do))) {
    points(
      depth ~ do,
      prof_matrix,
      type = "b",
      bg = "deepskyblue3",
      pch = 24,
      cex = 1.5
    )
  }
  if (any(!is.na(prof_matrix$temp))) {
    points(
      depth ~ temp,
      prof_matrix,
      type = "b",
      bg = "orange",
      pch = 21,
      cex = 1.5
    )
  }
  if (any(!is.na(prof_matrix$pH))) {
    points(
      depth ~ pH,
      prof_matrix,
      type = "b",
      bg = "green",
      pch = 22,
      cex = 1.5
    )
  }
  par(xpd = TRUE)
  legend(
    "bottomleft",
    inset = c(0.05,-0.3),
    bty = "n",
    lty = c(NA,
            NA, NA, 3),
    lwd = c(NA, NA, NA, 3),
    col = c("black",
            "black", "black", "purple"),
    legend = c(
      "Dissolved oxygen (mg/L",
      "Temperature (*C)",
      # paste0("Dissolved oxygen (",
      #        unit_table[unit_table$param_name == "do", 2], ")"),
      # paste0("Temperature (",
      #        unit_table[unit_table$param_name == "temp", 2], ")"),
      "pH"#,
     # "Thermocline"
    ),
    pch = c(24, 21, 22, NA),
    pt.bg = c("deepskyblue3",
              "orange", "green", NA),
    cex = 1.5
  )
  par(xpd = FALSE)
}
