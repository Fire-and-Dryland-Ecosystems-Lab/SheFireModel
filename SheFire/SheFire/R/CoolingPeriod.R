#' Cooling Period
#' 
#' Isolates the period of time when the soil depth of interest is cooling and returns the temperature values for that period.
#' 
#' Example data from US Forest Service FBAT: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#'
#' @param x The soil depth in cm.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the SheFire function containing the equations and data that comprise the model.
#' @return The function returns the temperatures at the specified time interval (res) for the soil depth (x) from the point that the soil depth begins to cool through the end of the model time range.  It also prints a text string summarizing some information about the cooling.
#' @author MaryKBrady
#' @seealso \code{\link{SheFire}}, \code{\link{HeatingPeriod}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- SheFire(input = inputFile )  #create model with all default settings
#' 
#' CoolingDown <- CoolingPeriod(x = 5, res = 1, model = model_example)
#' @export

CoolingPeriod <- function(x, res = 1, model){  #is x a valid depth
  #x and res checked in TempOverTime function
  equation <- TempOverTime(x = x, res = res, model = model) #t defaults to FullTime
  maxInd <- which.max(equation) #when does the temp peak
  percentcool <- ((equation[maxInd]-equation[model$FullTime])/(equation[maxInd]-equation[1]))*100  #temp fall/temp rise * 100
  timemin <- length(equation[maxInd:model$FullTime]) * res #convert length of time points to minutes (if res is not 1 min)
  print(paste0("The recorded cool down period for ", x, "cm was ", timemin, " min long and covered ", round(percentcool, digits = 1), " percent of the cooling"))
  return(equation[(maxInd+1):model$FullTime]) #returns after temp peak through the end
}
