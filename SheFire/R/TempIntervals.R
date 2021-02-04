#' Temperature Intervals
#' 
#' Calculates total time spent in different temperature ranges (bins) for a specified soil depth. The bin boundary temperatures are set by the function depending on the temps reached at that depth and the bin size set by the user.
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' 
#' @param x The soil depth in cm.
#' @param bin The size, in degrees, of each temperature bin (ie bins of 5 degrees: 15-19.9, 20-24.9). Defaults is 5.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the SheFire function containing the equations and data that comprise the model.
#' @return This function returns a data.frame of the bin temperature boundaries and the time (min) spent in each temp range.
#' @author MaryKBrady
#' @seealso \code{\link{SheFire}}, \code{\link{TempIntervalsSet}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- SheFire(input = inputFile )  #create model with all default settings
#' 
#' binned5cm <- TempIntervals(x = 5, bin = 5, model = model_example) #res defaults to 1 minute
#' binned5cm.res <- TempIntervals(x = 5, bin = 5, res = 0.5, model = model_example) #changed temporal resolution to .5 min
#' @export

TempIntervals <- function(x, bin = 5, res = 1, model){
  #x and res checked in TempOverTime function
  if(mode(bin) != "numeric" | length(bin) != 1){  #is bin valid
    stop("increment must be a single number")
  }
  equation <- TempOverTime(x = x, res = res, model = model)
  rangetemp <- seq(from = floor(min(equation)), to = ceiling(max(equation)), by = bin) #bin boundaries
  if ((ceiling(max(equation))-floor(min(equation))) %% bin != 0){ #if doesn't divide nicely, adjust final bin
    rangetemp <- c(rangetemp, (tail(rangetemp, n=1)+bin))
  }
  table <- matrix(nrow = length(rangetemp)-1, ncol = 3, dimnames = list(c(), c("low temp", "high temp", "time")))
  for (a in 1:(length(rangetemp)-1)){
    table[a,1:2] <- c(rangetemp[a], rangetemp[a+1]- 0.00001)
    time <- (length(which(equation >= rangetemp[a] & equation < rangetemp[a+1]))) * res #adjust times for variable temporal resolution
    if (a == (length(rangetemp)-1)){
      time <- (length(which(equation >= rangetemp[a] & equation <= rangetemp[a+1]))) * res
      table[a,2] <- rangetemp[a+1]
    }
    table[a,3] <- time
  } 
  return(as.data.frame(table))
}
