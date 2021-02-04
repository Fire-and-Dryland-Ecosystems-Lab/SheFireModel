#' Summarize a Depth Range
#' 
#' Calculates the mean temp, standard deviation, median temp, and max temp at each time point for a range of soil depths. The user can enter the shallow and deep bounds for the depth range or enter a single number and the range will default to that depth up to the shallowest depth calculable. NOTE: the deepest depth calculated will be the depth closest to the user defined max depth without exceeding it that can be reached by incrementing from the shallowest depth (example: if your shallow depth is 2.3cm and the increment is 0.5cm, the closest to a max depth of 4 would be 3.8cm). Once you have the values calculated at each time point, you can then run further statistics or functions on those temperatures.
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' 
#' @param x The soil depth in cm. Either one number or a vector of two numbers.
#' @param inc The increment (in cm) used to calculate which depths within the depth range to calculate.
#' @param t The time range, in min, (vector of two numbers or a single number if starting from the beginning) over which to calculate. Default is t = NULL, the full model time range.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the SheFire function containing the equations and data that comprise the model.
#' @return The function returns a data.frame of the mean, standard deviation, median, and maximum temperature for the depth range at each time point.
#' @author MaryKBrady
#' @seealso \code{\link{SheFire}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- SheFire(input = inputFile )  #create model with all default settings
#' 
#' Depth5to10cm <- SummDepthRange(x = c(5, 10), inc = 1, model = model_example) #t defaults to FullTime - full time range of model, res defaults to 1
#' 
#' #plot mean +/- stand dev over time
#' Depth5to10cm$time <- Depth5to10cm$time - 0.00001 #just to make it look a bit cleaner (model time starts at 0.00001, not 0, for math reasons)
#' ggplot(Depth5to10cm, (aes(x = time))) +   
#'  geom_line(aes(y = mean), color = "blue", size = 1) +
#'  geom_line(aes(y = mean - stnddev), color = "red", linetype = "dashed") +
#'  geom_line(aes(y = mean + stnddev), color = "red", linetype = "dashed") +
#'  labs(x = "Time (min)", y = "Temp (C)", title = "Mean temp for depths 5-10cm +/- standard deviation")
#' @export

SummDepthRange <- function(x, inc = 0.5, t = NULL, res = 1, model){
  #x and t  and res checked in TempOverTime function
  if(is.null(t)){
    t <- model$FullTime
  }
  if(length(inc) != 1){ #is b just one number
    stop("Increment must be a single number")
  }
  if (length(x) == 1){  #if only 1 depth entered, set shallowest as start
    depths <- seq(from = model$Shallowest, to = x, by = inc)
    if(tail(depths, n=1) != x){  #if the increment does add evenly up to the max depth entered, print warning
      print(paste("Warning: the max depth calulated (based on shallowest depth possible and the increment chosen, is", round(tail(depths, n=1), digits = 3), "and not", x))
    }
  }else if (length(x) == 2) {  #if two depths entered
    if(x[2] <= x[1]){  #are depths entered in order
      stop("depths must be listed shallowest first")
    }
    depths <- seq(from = x[1], to = x[2], by = inc) 
    if(tail(depths, n=1) != x[2]){  #if increment does not add evenly up to max depth entered, print warning
      print(paste("Warning: the max depth calulated (based on the shallowest depth and increment chosen, is", round(tail(depths, n=1), digits = 3), "and not", x[2]))
    }
  } else {stop("Depth must be either one or two numbers")}  #if more than two depths entered, stop
  allTemps <- sapply(depths, FUN = TempOverTime, t = t, res = res, model = model) #create equation for each depth
  mean <- apply(allTemps, 1, FUN = mean) #take the average temp across all depth at each time point
  stnddev <- apply(allTemps, 1, FUN = sd)  #take standard deviation at each time point
  median <- apply(allTemps, 1, FUN = median)  #median  at each time point
  max <- apply(allTemps, 1, FUN = max)  #max temp at each time point
  time <- seq(from = .00001, to = t, by = res) #marks time points 
  table <- data.frame(time, mean, stnddev, median, max) #gather all of that into a data frame
  return(table)
}
