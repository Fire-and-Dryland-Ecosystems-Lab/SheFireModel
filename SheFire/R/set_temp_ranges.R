#' Set Temperature Ranges
#' 
#' Calculates time spent in different temp ranges (bins) for a specified soil depth(s). The user sets the lower bounds of each bin (bins don't have to be equal sizes). The final bin includes all temps over the last input temp (no upper bound to temps included in last bin).
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' 
#' @param x The soil depth in cm. Either a single number or a vector of numbers.
#' @param bins A vector of the lower bounds of all the temperature bins.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the shefire function containing the equations and data that comprise the model.
#' @return The function returns a data.frame with bin boundary temps and times in each bin for each depth.
#' @author MaryKBrady
#' @seealso \code{\link{shefire}}, \code{\link{time_temp_ranges}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- shefire(input = inputFile )  #create model with all default settings
#' 
#' SetBins2cm <- set_temp_ranges(x = 2, bins = c(10,15,20,25,30,35,40), model = model_example) #default res is 1 min
#' 
#' #example to compare diff depths with same temp bins - reformat table output from set_temp_ranges function to plot
#' CompareDepths <- set_temp_ranges(x = c(2,3,4,5), bins = seq(from = 10, to = 45, by = 5), model = model_example)
#' CompareDepths #view table
#' ##to plot
#' timevalues <- as.numeric(c(CompareDepths[,3], CompareDepths[,4], CompareDepths[,5], CompareDepths[,6]))
#' type <- c(rep("Depth2cm", nrow(CompareDepths)), rep("Depth3cm", nrow(CompareDepths)), rep("Depth4cm", nrow(CompareDepths)), rep("Depth5cm", nrow(CompareDepths)))
#' binnames <- rep(c("10-14.9","15-19.9","20-24.9","25-29.9","30-34.9","35-39.9","40-44.9","45+"), 4)
#' TableToPlot <- data.frame(binnames, timevalues, type)
#' ggplot(TableToPlot, aes(binnames, timevalues)) +
#'   geom_col(position = "dodge", aes(fill = type)) +
#'   labs(x = "Temp bins", y = "Time (min)", title = "Time in temp ranges for different soil depths")
#' @export

set_temp_ranges <- function(x, bins=c(0, 10, 20, 30, 40, 50, 60), res = 1, model){
  #x checked in temp_over_time function
  if(mode(bins) != "numeric" | length(bins) < 2){
    stop("bins must be a numeric vector with length greater than 1")
  }
  table <- matrix(nrow = length(bins), ncol = length(x)+2)
  columns <- c("low.temp", "high.temp")
  for (a in seq(length(x))){
    equation <- temp_over_time(x = x[a], res = res, model = model)
    columns <- append(columns, paste0("Time ",x[a],"cm"))
    for (b in seq(length(bins))){
      table[b,1:2] <- c(bins[b], bins[b+1]- 0.00001)
      time <- (length(which(equation >= bins[b] & equation < bins[b+1]))) * res #adjust time for variable temporal resolution
      if (b == (length(bins))){
        time <- (length(which(equation >= bins[b]))) * res 
        table[b,1] <- bins[b]
        table[b,2] <- paste0(tail(bins, n=1), "+")
      }
      table[b,(2+a)] <- time 
    }
  }
  colnames(table) <- columns
  return(as.data.frame(table))
}
