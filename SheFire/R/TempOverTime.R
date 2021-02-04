#' Temperature Over Time
#' 
#' This function takes the model created in the SheFire function and uses it to calculate the temperature over time for the user input depth. Until more validation experiments are run, it is not recommended that you use this model for soil depths deeper than 5 centimeters beyond the deepest sensor used to create the model. The function will print a warning if a deeper depth is entered.  The shallowest depth that can be calculated is set by the model and can accessed by model_object_name$Shallowest. This function will print an error message if input depth is too shallow. Note that in some cases, the shallowest depth that can mathematically be calculated by the model may be shallower than the shallowest depth that produces a reasonable prediction because near the soil surface, the temperatures become more extreme and difficult to model. This is a soil depth model, not a soil surface model. Plotting the temperature over time for a shallow depth of interest alongside a deeper depth (2 or 3 cm perhaps) is typically sufficient to visually determine if the shallow depth output is reasonable. A warning message will be printed for depths at or less than 0.5cm.
#'
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf At least at the time of writing, all application functions in this package are based, in part, on this function.
#'   
#' @param x The soil depth (cm) for which to calculate temperature over time.
#' @param t To enter a time (min) for when to cut off the calculation (after starting at t=0.0001), use a single number for the cutoff time. Or, to set a time range (min) across which to calculate, use a vector of two numbers to designate the start and stop times. Default is t = NULL which will set the time range as the full model time period starting at time = 0.00001 (cannot have time = 0).
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the SheFire function containing the equations and data that comprise the model.
#' @return Returns a list of the soil temperatures at each time interval (res) for the specified depth(x) over the specified time range (t).
#' @author MaryKBrady
#' @seealso \code{\link{SheFire}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' #standard use:
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- SheFire(input = inputFile )  #create model with all default settings
#' 
#' Depth4cm <- TempOverTime(x = 4, res = 1, model = model_example) #run function on model, t defaults to FullTime - full time range of model
#' 
#' #add model time to temp data
#' time <- seq(from = 0.00001, to = model_example$FullTime, by = 1) #model time technically starts at 0.00001 not 0 (because math reasons, sorry) you can probably ignore the 1/100000 of a minute if you wanted to, put temporal resolution (res) in "by" parameter
#' Depth4cmWithTime <- cbind(time, Depth4cm)
#' colnames(Depth4cmWithTime) <- c("Time", "Temperature")
#' 
#' #connect temp data to real time:
#' StartTime.formatted <- parse_date_time(model_example$StartTime, "dmy HM") #lubridate pkg. parse StartTime into time format, "dmy HM" is the input format
#' Time <- StartTime.formatted  #initialize Time at beginning of time for Depth4cm
#' for (a in seq(length(1:model_example$FullTime)-1)){  #for each row (time point) in table of temps (1 per minute, res = 1), -1 because starts with Time already in next line
#'   Time <- c(Time, StartTime.formatted + 60*a)  #add one minute (60 sec) to StartTime 
#' }
#' Depth4WithRealTime <- cbind(as.character(Time), Depth4cm) #table of times and corresponding temps
#' colnames(Depth4WithRealTime) <- c("Time", "Temperature")
#' head(Depth4WithRealTime)
#' 
#' #example of applying function to multiple depths at once and plotting:
#' ##make a table with temps for all depths:
#' depths <- c(1, 2, 3, 4, 5)
#' multiple.depths <- sapply(X=depths, FUN=TempOverTime, model = model_example) #default t and res
#' colnames(multiple.depths) <- lapply(X=depths, FUN=function(X)paste0("Depth", X, "cm"))
#' ##for plotting
#' AllTemps <- c(multiple.depths[,1], multiple.depths[,2], multiple.depths[,3],multiple.depths[,4],multiple.depths[,5])
#' Type <- c(rep("Depth1cm", nrow(multiple.depths)), rep("Depth2cm", nrow(multiple.depths)), rep("Depth3cm", nrow(multiple.depths)), rep("Depth4cm", nrow(multiple.depths)), rep("Depth5cm", nrow(multiple.depths))) #grouping variable for plot
#' TimeCount <- seq(nrow(multiple.depths))  #don't forget to multiply by res if temporal resolution is not 1 min
#' ToPlot <- data.frame(TimeCount, AllTemps, Type)
#' ggplot(ToPlot, aes(x=TimeCount, y=AllTemps)) +
#'   geom_line(aes(color = Type), size = 1) +
#'   labs(x = "Time (min)", y = "Temp (C)", title = "Temperature over time")
#' @export

TempOverTime <- function(x, t = NULL, res = 1, model){
  if (length(x) != 1 | mode(x) != "numeric"){  #is x a valid depth
    stop("Input depth must be a single number")
  }
  if (x < model$Shallowest){  #is x too shallow
    stop(paste0("The shallowest you can extrapolate is ", round(model$Shallowest, digits = 3), "cm below the surface"))
  }
  if (x < 0.5){ #can I write a function to check for some of these irregularities?
    print("Warning: as you get very shallow, the uncertainty increases as the temperatures become more extreme. The depth at which the predictions no longer make logical sense varies with the burn but generally, looking at plot of the temps over time is enough to determine when that is the case (plot shallow depths alongside deeper depths like 2 and 3cm to compare). The shallowest reasonable depth may in fact be a little deeper than the shallowest depth that it is possible for the model to calculate. Generally, 0.5cm and deeper is reasonable.")
  }
  if (x > (model$SensorDepths[3]+5)){ #warning if trying to use deeper then deepest sensor + 5cm
    print("Warning: No deepest depth that can be predicted by this model has been set. Until someone is able to conduct further validation experiments, use caution predicting more than about 5cm beyond your deepest sensor. Maybe plot the temperature over time for the deep depth alongside a few shallower depths and see if the results makes sense")
  }
  if(is.null(t)){ #if user did not specify time
    t <- model$FullTime
  }
  if (mode(t) != "numeric" | t[1] < 0.00001){  #is time valid
    stop("The time range must be a positive number or range of numbers, earliest possible time is 0.0001 min")
  }
  if (length(t) == 1){  #if only one time input, make time start at beginning - really close to 0
    t <- c(0.00001, t)
  }
  if(t[2] > model$FullTime){  #is time within bounds
    stop(paste("That time range extends beyond the time limit. You cannot go beyond", FullTime))
  }
  if (t[1] > t[2]){  #is time entered in order
    stop("Time range must go in chronological order (earlier time first)")
  }
  if(mode(res) != "numeric" | length(res) != 1){  #is res a valid temporal resolution
    stop("res must be a single number")
  }
  t.res <- seq(from = t[1], to = t[2], by = res)
  maxtemp <- model$MaxTemp.reg(x, MaxTemp.coeffs = model$MaxTemp.coeffs)  #set the parameters using the regressions
  timeatmax <- model$TimeAtMax.reg(x, TimeAtMax.coeffs = model$TimeAtMax.coeffs)
  inittemp <- model$InitTemp.reg(x, InitTemp.coeffs = model$InitTemp.coeffs, InitTemp.byDepth = model$InitTemp.byDepth, SensorDepths = model$SensorDepths)
  shape <- model$Shape.reg(x, Shape.coeffs = model$Shape.coeffs)
  equation <- model$BFDEquation(t.res, inittemp, maxtemp, timeatmax, shape)  #calculate the equation
  return(equation)
}  
