#' Depth for Temperature
#' 
#' Calculates the deepest depth (cm) that is reached by the specified temperature. When the exact depth is not tested, the function will choose the shallower of the two bounding depths (example: if 60C actually reaches 5.4cm but the function is set up such that it checks 5cm and 5.5cm, it will return 5cm). Using a smaller depth increment (inc) will increase precision.
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' 
#' @param temp The temperature of interest.
#' @param inc The final increment (cm) used to calculate which depths to check (ie every 0.5 cm). Default is 0.5.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the shefire function containing the equations and data that comprise the model.
#' @return This function returns the deepest depth (in cm) that reached the specified temperature. It also print a text string that contains the same information or reasons for a NA or NULL return.
#' @author MaryKBrady
#' @seealso \code{\link{shefire}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- shefire(input = inputFile )  #create model with all default settings
#' 
#' Deepest30 <- depth_for_temp(temp = 30, inc = 0.1, model = model_example)  #default temporal resolution res = 1
#' @export

depth_for_temp <- function(temp, inc = 0.5, res = 1, model){
  #res checked in temp_over_time functions
  if(mode(temp)!= "numeric" | length(temp) != 1){  #is temp valid
    stop("temp must be a single number")
  }
  if(mode(inc) != "numeric" | length(inc) != 1){  #is inc valid
    stop("increment must be a single number")
  }
  depths <- c(model$SensorDepths, model$SensorDepths[3]+5)  #starting depths to begin narrowing process
  firstcheck.table <- sapply(depths, FUN = temp_over_time, res = res, model = model) #get temps for each depth, t defaults to FullTime
  firstmaximums <- apply(firstcheck.table, 2, FUN = max) #max temp for each depth
  firsts <- which(firstmaximums > temp) #which depths got over the threshold
  index <- tail(firsts, n=1) #deepest depth that got over threshold
  x <- depths[index] #set x as that depth
  if (length(firsts) == 0){ #if no depth reached threshold, set x as the shallowest depth possible
    x <- model$Shallowest
  }
  if (x == tail(depths, n=1)){ #if x is deepest + 5, end function (can we trust extrapolations beyond that?)
    print(paste(temp, "C reached at least as deep as", model$SensorDepths[3] + 5, "cm"))
    return(NA)
  } else if (x == model$Shallowest){ #if x is the shallowest 
    depths <- c(x, ceiling(x):5) #new depths start at shallowest then goes from nearest larger whole number
  }else {
    depths <- x:(x+5) #new depths go from x to x+5 by ones
  }
  secondcheck.table <- sapply(depths, FUN = temp_over_time, res = res, model = model)  #get temps for each depth
  secondmaximums <- apply(secondcheck.table, 2, FUN = max)  #max temp for each depth
  seconds <- which(secondmaximums > temp) #which depths got over the threshold
  index <- tail(seconds, n=1) #deepest depth that got over threshold
  x <- depths[index]  #set x as that depth
  if(length(seconds) ==0){ #if not depth reached threshold
    print(paste("No depths tested reached", temp, "C. The shallowest depth that could be tested was", model$Shallowest,"cm which reached", round(secondmaximums[1], digits = 3), "C"))
    return(NULL)
  }
  depths <- seq(from = x, to = x+1, by = inc)  #new depths go from x to x+1 by the increment
  thirdcheck.table <- sapply(depths, FUN = temp_over_time, res = res, model = model)  #get temps for each depth
  thirdmaximums <- apply(thirdcheck.table, 2, FUN = max)  #max temps for each depth
  thirds <- which(thirdmaximums > temp)   #which depths got over the threshold
  index <- tail(thirds, n=1) #deepest depth that got over threshold
  x <- depths[index]  #set x as that depth
  print(paste(x, "cm is the deepest soil depth that experienced", temp, "C"))
  return(x)
}
