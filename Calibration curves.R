# --------------------------------- #
# Microprobe calibration curves
# --------------------------------- #
# Copyright Lowell R. Moore 2023 MIT license

# TODO:
#   -

# --------------------------------- #
# Data and settings
# --------------------------------- #

# Tidy CSV file with standards and unknowns labeled
Import <- read.csv("Input to R.csv"
                   , stringsAsFactors = FALSE
                   )

# Text labels in CSV file used to identify standards and unknowns
stds_flag <- "Standard"
unk_flag <- "WDS"

# number of replicates for standards analyzed as unknowns
n_replicates <- 3

# Confidence level used for uncertainty envelope
pred_level <- 0.95

# Flag standard rows and element columns
standards <- which(Import$flag == stds_flag)
standards <- Import$Comment[flag_stds]
elements <- colnames(Import)[4:12]


# --------------------------------- #
# Functions
# --------------------------------- #

ele_plot <- function(element){
  xs <- NA # Reference concentration of element
  ys <- NA # Analyzed concentration of element
  ref_comment <- NA
  
  for(std_i in standards){
    # Debug : std_i <- standards[1]; element <- elements[1]
    
    # identify plot xs as standard rows
    std_rows <- which(Import$Comment == std_i
                      & Import$flag == stds_flag)
    xs <- c(xs, rep(Import[std_rows,element], n_replicates))
    
    # identify plot ys as unknown rows 
    unk_rows <- which(Import$Comment == std_i
                      & Import$flag == unk_flag)
    ys <- c(ys, Import[unk_rows,element])
    
    # identify comment column for output data table
    ref_comment <- c(ref_comment, rep(Import[std_rows,"Comment"], n_replicates))
    
    # Absolute errors = ys - xs
    elem_error <- ys-xs
  }
  
  # Only output data if there were more than one compositions
  if(sum(!is.na(xs)) > 1){
    
    # Scatter plot for calibration curve
    plot(xs, ys
         , xlab = paste(element, ", (standard)", sep = "")
         , ylab = paste(element, ", (unknown)", sep = "")
         , main = element
    )
    
    # one-to-one line
    abline(0, 1, col = "gray", lwd = 2)
    
    # R-squared label
    not_NA <- which(!is.na(xs) & !is.na(ys))
    text(min(xs[not_NA]), max(ys[not_NA]), adj = c(0, 1)
         , labels = paste("R =", round(cor(xs[not_NA], ys[not_NA]), 5)))
    
    # linear least-squares regression model
    train_rows <- which(!is.na(xs) & !is.na(ys))
    train_data <- data.frame(wds = ys[train_rows], ref = xs[train_rows])
    linear_model <- lm(ref~wds, data = train_data)
    summary(linear_model)
    
    # Prediction envelope and regression line
    new_data <- data.frame(wds = seq(min(train_data$wds), max(train_data$wds)
                                     , length.out = 10))
    pred_model <- as.data.frame(predict(object = linear_model, newdata = new_data, interval = "p"
                          , level = pred_level))
    delta_lwr <- pred_model$lwr - pred_model$fit
    delta_upr <- pred_model$upr - pred_model$fit
    pred_model <- cbind(pred_model, delta_lwr, delta_upr)
    
    # Add regression line and error envelope to plot
    line_ys <- new_data$wds
    lines(pred_model[,1], line_ys, col = "red", lty = 1)
    lines(pred_model[,2], line_ys, col = "red", lty = 2)
    lines(pred_model[,3], line_ys, col = "red", lty = 2)
    
    # Return some output data
    # output_data = the actual points being plotted and used for the linear
    #                  regression model
    # pred_model = the prediction uncertainty envelope and concentrations predicted
    #                 by the linear regression model
    output_data <- cbind.data.frame(ref_comment, xs, ys, elem_error)
    output_data <- output_data[-1,]
    return(list(output_data, pred_model, summary(linear_model)))
    
  } else {
    warning(paste("No values for element", element))
  }
}


# --------------------------------- #
# Calculations
# --------------------------------- #

# Summary table to hold mean error values for each element
#   based on prediction envelope
elements_error_summary <- data.frame(Element = elements, mean_error = rep(NA, length(elements)))

# PDF to hold calibration curve figures
pdf("WDS comparison plots.pdf", useDingbats = FALSE, width = 8, height = 14)
par(mfrow = c(5, 3))
for(i in 1:length(elements)){
  print(elements[i])
  output_i <- ele_plot(elements[i])
  
  # Save output data for each element in text files
  write.csv(file = paste("output_data_", elements[i], ".csv", sep = "")
            , x = output_i[[1]], row.names = FALSE, quote = FALSE)
  write.csv(file = paste("pred_model_", elements[i], ".csv", sep = "")
            , x = output_i[[2]], row.names = FALSE, quote = FALSE)
  sink(file = paste("linear_model_", elements[i], ".txt", sep = ""))
  print(output_i[[3]])
  sink()
  
  # Fill out error values in the summary table for each element
  elements_error_summary$mean_error[i] <- mean(output_i[[2]]$delta_upr)
}
dev.off()
par(mfrow = c(1, 1))

write.csv(file = "elements_error_summary.csv", x = elements_error_summary
          , quote = FALSE, row.names = FALSE)

# Test output for just one element
#test_output <- ele_plot("MgO.Mass..")

