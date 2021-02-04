#' Heating Period
#' 
#' Isolates the portion of time when the soil depth of interest is heating and returns the temperature values for that period. 
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' For discussion on reasons to isolate just the heating limb of the temperature curve see: McGranahan, D. A. (2020). An inconvenient truth about temperature-time data from thermocouples. Plant Ecology, 221(11), 1091-1104.
#' 
#' 
#' @param x The soil depth in cm.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the SheFire function containing the equations and data that comprise the model.
#' @return The function returns the temperatures at the set time interval (res) for the specified soil depth (x) from the point when it begins to warm through the temperature peak. It also prints a text string summarizing some information about the heating.
#' @author MaryKBrady
#' @seealso \code{\link{SheFire}}, \code{\link{CoolingPeriod}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- SheFire(input = inputFile )  #create model with all default settings
#' 
#' heating_portion <- HeatingPeriod(x = 4, model = model_example)  #res defaults to 1 minute
#' @export

HeatingPeriod <- function(x, res = 1, model){
  #x and res checked in TempOverTime function
  equation <- TempOverTime(x = x, res = res, model = model)  #function defined in package
  maxInd <- which.max(equation) #where is the max temp
  NumRows <- length(equation[1:maxInd])  #how long before (and including) the max temp                    
  TempRate <- matrix(0, nrow= NumRows, ncol= 1) #initialize matrix                            
  for (a in 1:NumRows-1) {  #in first row up to the row before the max temp                                                
    TempRate[a,1] <- equation[a+1] - equation[a]   #Temp change = next.temp-temp
  }
  MaxTRInd <- which.max(TempRate) #Max Temp Rate Index - row at max rate
  startInd <- 1 #initialize StartInd, if not adjusted in the next for loop, remains at earliest time possible
  for (b in 1:MaxTRInd) {   #Searches prior to MaxTRInd for last 0 rate of change
    if(TempRate[b] <= 0.0001){ #if temp rate is really really close to 0 or negative (cooling), act as if it is 0 
      startInd <- b #set the start point
    }
  }
  timemin <- length(equation[startInd:maxInd]) * res #convert length of time points to time in min (in case res is not 1 min)
  print(paste0("Depth ", x, "cm was heating for ", timemin, " minutes and increased ", round(max(equation)-equation[1], digits = 3), " degrees in that time"))
  return(equation[startInd:maxInd]) #returns start of temp rise through the peak
}
