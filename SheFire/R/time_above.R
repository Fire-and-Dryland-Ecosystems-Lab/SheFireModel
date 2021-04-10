#' Time Above Threshold
#' 
#' Calculates the time (min) a specified soil depth spends at or above a specified temperature threshold.
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' 
#' @param x The soil depth in cm.
#' @param t The time (min) for when to cut off the calculation (use a single number) or the time range (min) (use a list of two numbers) across which to calculate, defaults to the full model time period.
#' @param temp The temperature threshold.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the shefire function containing the equations and data that comprise the model.
#' @return Function returns the length of time in minutes that the soil depth spent at or above the threshold. The function also prints a text string with the same information.
#' @author MaryKBrady
#' @seealso \code{\link{shefire}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- shefire(input = inputFile )  #create model with all default settings
#' 
#' TimeAbove30 <- time_above(x = 5, temp = 30, res = 1, model = model_example) #t defaults to FullTime - full time range of model
#' TimeAbove20 <- time_above(x = 8, temp = 17, t = 10:1000, res = 0.5, model = model_example) #subset the time period used
#' 
#' #example with multiple depths
#' depths <- c(4, 6, 8)
#' Above25multiple <- sapply(X=depths, FUN = time_above, temp = 25, t = 1100, model = model_example) #shortened time range
#' @export

time_above <- function (x, temp, t= NULL, res = 1, model){
  #x and t and res are checked in temp_over_time function
  if (mode(temp) != "numeric" | length(temp) != 1){  #is temp valid input
    stop("The temp threshold must be a single number")
  }
  equation <- temp_over_time(x = x, t = t, res = res, model = model)  #function defined in package
  minutes <- (length(which(equation >= temp))) * res #pulls index for all points when temp is above threshold then counts how many and adjusts for temporal resolution
  print(paste0("Depth ", x, "cm was above ", temp, "C for ", minutes, " minutes in this time range"))
  return(minutes)
}
