#' Survival Depth
#' 
#' Calculates the depth that had the specified survival (ie at what depth was the cassia seed survival 80 percent?). If the exact depth that had the exact specified survival is not checked, the function will return the next shallowest depth that was tested and print a text string with the details (example: 50 percent survival is at 5.67cm but the function is set up to check 5.5cm and 6cm, it will return 5.5cm). Using a smaller depth increment (inc) will increase precision. The function does not check soil depths deeper than 5cm below the deepest sensor that was used to create the model. The function requires three thermal tolerance parameters, deltaS, deltaH, and a lower temperature limit, which are specific to the organism/tissue/etc of interest. These parameters are specific to the kind of impact the user is looking at because different biological systems (seeds, roots, microorganisms, etc) have varying heat tolerances.
#'
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' Papers cited in this function: DICKINSON, M. B., & Johnson, E. A. (2004). Temperature-dependent rate models of vascular cambium cell mortality. Canadian Journal of Forest Research, 34(3), 546-559. ; MARTIN, R. E., Cushwa, C. T., & Miller, R. L. (1969). Fire as a physical factor in wildland management. In In: Proceedings Annual [9th] Tall Timbers Fire Ecology Conference. Tallahassee, FL. Tall Timbers Research, Inc., Tallahassee, FL. 271-288. (pp. 271-288). ; MIESEL, J. R., Dickinson, M. B., Kolka, R. K., Kern, C. C., Donner, D. M., Quigley, K. M., & Bushman, M. M. (2020). Manipulating soil heating patterns to optimize barrens restoration. FireScience.gov. ; ROSENGERG, B., Kemeny, G., Switzer, R. C., & Hamilton, T. C. (1971). Quantitative evidence for protein denaturation as the cause of thermal death. Nature, 232(5311), 471-473.
#'
#' @param survivalValue The survival threshold of interest in decimal form (60 percent survival -> 0.6).
#' @param deltaS The activation entropy (units of J*mol-1*K-1) for the organism/tissue/etc of interest.
#' @param deltaH The activation enthalpy (units of J*mol-1) for the organism/tissue/etc of interest. 
#' @param temp The threshold temperature when survival will begin to be impacted. Default is 48 for physiologically active tissue based on Miesel et al (2020) and personal communication with Matthew B Dickinson.
#' @param inc The final increment (in cm) used to calculate what depths to check (ie every 0.5 cm). Default is 0.5.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute. Res greater than 1 is not recommended for this function in particular because of how strongly it can impact results.
#' @param model The model object (a list) from the SheFire function containing the equations and data that comprise the model.
#' @return This function returns the depth that exactly reached the survival threshold or, if no depth tested was exact, the depth tested that was closest to and less than the survival threshold (higher mortality). The function also prints a text string explaining if the returned depth was at the exact threshold or not. If all depths tested have a lower survival, returns NA. If all depths tested have a higher survival, returns NULL.
#' @author MaryKBrady, based on work by Matthew B Dickinson
#' @seealso \code{\link{SheFire}}, \code{\link{SurvivalPercent}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- SheFire(input = inputFile )  #create model with all default settings
#' 
#' CassiaSeeds <- SurvivalDepth(survivalValue = 0.8, deltaS = 112, deltaH = 147741, inc = 0.1, model = model_example) #default temporal resolution, increment, and temp. Cassia Seed survival, for parameters see Martin et al (1969)
#' Proteins <- SurvivalDepth(survivalValue = 0.5, deltaS = 482, deltaH = 248410, inc = 0.5, model = model_example) #default temporal resolution and temp. Protein thermal denaturation parameters, See Dickinson and Johnson (2004) and Rosenberg et al (1971)
#' @export

SurvivalDepth <- function(survivalValue, deltaS, deltaH, temp = 48, inc = 0.5, res = 1, model){
  #res checked in TempOverTime functions
  if(mode(survivalValue)!= "numeric" | length(survivalValue) != 1 | survivalValue > 1 | survivalValue < 0){  #is survivalValue valid
    stop("survivalValue must be a single number from 0 to 1")
  }
  if(mode(inc) != "numeric" | length(inc) != 1){  #is inc valid
    stop("increment must be a single number")
  }
  if(mode(deltaS) != "numeric" | length(deltaS) != 1){
    stop("deltaS must be a single number")
  }
  if(mode(deltaH) != "numeric" | length(deltaH) != 1){
    stop("deltaH must be a single number")
  }
  RGas <- 8.31  #Universal gas constant, units of J K-1 mol-1
  kBoltz <- 1.38*10^(-23)  #Boltzman constant, J·K-1
  hPlanck <- 6.63*10^(-34)  #Planck's constant J·s
  #round 1
  depths <- c(model$SensorDepths, model$SensorDepths[3]+5)  #starting depths to begin narrowing process
  survival.list <- c() #initialize list of survival values
  for(a in depths){
    survival <- 1 #survival starts at 1 - 100 percent
    equation <- TempOverTime(x = a, res = res, model = model) #for each time point
    for(b in seq(equation)){ #for every time point
      if(equation[b] >= temp){  #if the temp is equal to or over set threshold
        TempK <- equation[b]+273.15 #Convert temp to Kelvin 
        timepoint.mortality <- (((kBoltz*TempK)/hPlanck)*exp(deltaS/RGas)*exp(-deltaH/(RGas*TempK))) * (res * 60) #mortality per second * # of seconds in each time step (res)
        survival <- survival - (timepoint.mortality * survival)  #total survival thus far = total survival previous - this time point effect on the previous survival
        if (survival < 0){ #if no survival, set to 0
          survival <- 0
        }
      }
    }
    survival.list <- append(survival.list, survival) #add survival to list
  }
  exact <- which(survival.list == survivalValue) #test for exact value
  if(length(exact) != 0){ #if any are exact
    index <- exact[1] #just in case more than one is exact? pick the shallowest
    finaldepth <- depths[index]
    print(paste("Depth", finaldepth, "cm had a survival of", survivalValue))
    return(finaldepth)
  }
  exceeds <- which(survival.list > survivalValue) #which depths got over the threshold (survival.list index same as depths index)
  if(length(exceeds) == 0){  #if no depths exceed survival threshold
    deepestsurvival <- tail(survival.list, n=1)
    print(paste("All depths tested had lower survival than", survivalValue, ". The model does not test deeper than 5cm beyond the deepest sensor where, in this case, the survival was", model$SensorDepths[3]+5 ,"cm with", round(deepestsurvival, digits = 3), "survival."))
    return(NA)
  }
  index <- exceeds[1] #shallowest depth that exceeded survival threshold
  x <- depths[index] #set x as that depth
  if(index == 1){ #if x is the shallowest sensor (all exceed threshold survival) need to check for model shallowest depth
    depths <- c(model$Shallowest, ceiling(model$Shallowest):x) #shallowest, nearest whole number up to shallow sensor depth (x)
  }else{
    depths <- depths[index - 1]:x #from the next shallowest sensor to the sensor that exceeded survival threshold
  }
  #round 2
  survival.list <- c() #initialize list of survival values
  for(a in depths){
    survival <- 1 #survival starts at 1 - 100 percent
    equation <- TempOverTime(x = a, res = res, model = model) #for each time point
    for(b in seq(equation)){ #for every time point
      if(equation[b] >= temp){  #if the temp is equal to or over set threshold
        TempK <- equation[b]+273.15 #Convert temp to Kelvin 
        timepoint.mortality <- (((kBoltz*TempK)/hPlanck)*exp(deltaS/RGas)*exp(-deltaH/(RGas*TempK))) * (res * 60) #mortality per second * # of seconds in each time step (res)
        survival <- survival - (timepoint.mortality * survival)  #total survival thus far = total survival previous - this time point effect on the previous survival
        if (survival < 0){ #if no survival, set to 0
          survival <- 0
        }
      }
    }
    survival.list <- append(survival.list, survival) #add survival to list
  }
  exact <- which(survival.list == survivalValue) #test for exact value
  if(length(exact) != 0){ #if any are exact
    index <- exact[1] #just in case more than one is exact? pick the shallowest
    finaldepth <- depths[index]
    print(paste("Depth", finaldepth, "cm had a survival of", survivalValue))
    return(finaldepth) #return depth and end function
  }
  exceeds <- which(survival.list > survivalValue)
  if(length(exceeds)==length(survival.list)){ #all could exceed survival if starting from model$Shallowest in depths
    print(paste("No depths tested had a survival at or below", survivalValue, ". The shallowest depth that could be tested was", model$Shallowest,"cm which had a survival of", round(survival.list[1], digits = 3)))
    return(NULL)
  }
  index <- exceeds[1] #length of exceeds will be at least one
  x <- depths[index]  #x will not be the first value in depths 
  depths <- seq(from = depths[index - 1], to = x, by = inc)
  if(length(depths) ==1){ #use at least two depths so that final print statement can show what the next deepest depth survival
    depths <- c(depths, x)
  }
  #round 3
  survival.list <- c() #initialize list of survival values
  for(a in depths){
    survival <- 1 #survival starts at 1 - 100 percent
    equation <- TempOverTime(x = a, res = res, model = model) #for each time point
    for(b in seq(equation)){ #for every time point
      if(equation[b] >= temp){  #if the temp is equal to or over set threshold
        TempK <- equation[b]+273.15 #Convert temp to Kelvin 
        timepoint.mortality <- (((kBoltz*TempK)/hPlanck)*exp(deltaS/RGas)*exp(-deltaH/(RGas*TempK))) * (res * 60) #mortality per second * # of seconds in each time step (res)
        survival <- survival - (timepoint.mortality * survival)  #total survival thus far = total survival previous - this time point effect on the previous survival
        if (survival < 0){ #if no survival, set to 0
          survival <- 0
        }
      }
    }
    survival.list <- append(survival.list, survival) #add survival to list
  }
  exact <- which(survival.list == survivalValue) #test for exact value
  if(length(exact) != 0){ #if any are exact
    index <- exact[1] #just in case more than one is exact? pick the shallowest
    finaldepth <- depths[index]
    print(paste("Depth", finaldepth, "cm had a survival of", survivalValue))
    return(finaldepth) #return depth and end function
  }
  notexceeds <- which(survival.list < survivalValue) #which depths are lower survival than threshold
  index <- tail(notexceeds, n = 1) #the index for deepest depth lower than threshold - should be the depth closest to and lower than threshold
  x <- depths[index] #the depth for survival closest to and not exceeding threshold
  print(paste("The depth returned is", round(x, digits = 3), "cm with a survival of", round(survival.list[index], digits = 3),". The next deepest depth tested was", round(depths[index+1], digits = 3), "cm with survival of", round(survival.list[index+1], digits = 3),". If you need more precision, decrease depth increment (inc) or possibly temporal resolution (res) if it is greater than one."))
  return(x)
}
