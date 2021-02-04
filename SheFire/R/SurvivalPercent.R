#' Survival Percent
#' 
#' Models survival of the specified biological organism/tissue/protein/etc at the specified soil depth over time. The function requires two thermal tolerance parameters, deltaS and deltaH, specific to the organism/tissue/etc of interest. These parameters are specific to the kind of impact the user is looking at because different biological systems (seeds, roots, microorganisms, etc) have varying heat tolerances.
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' Other papers cited in this function: DICKINSON, M. B., & Johnson, E. A. (2004). Temperature-dependent rate models of vascular cambium cell mortality. Canadian Journal of Forest Research, 34(3), 546-559. ; MARTIN, R. E., Cushwa, C. T., & Miller, R. L. (1969). Fire as a physical factor in wildland management. In In: Proceedings Annual [9th] Tall Timbers Fire Ecology Conference. Tallahassee, FL. Tall Timbers Research, Inc., Tallahassee, FL. 271-288. (pp. 271-288). ; MIESEL, J. R., Dickinson, M. B., Kolka, R. K., Kern, C. C., Donner, D. M., Quigley, K. M., & Bushman, M. M. (2020). Manipulating soil heating patterns to optimize barrens restoration. FireScience.gov. ; ROSENGERG, B., Kemeny, G., Switzer, R. C., & Hamilton, T. C. (1971). Quantitative evidence for protein denaturation as the cause of thermal death. Nature, 232(5311), 471-473.
#' 
#' @param x The soil depth in cm.
#' @param deltaS The activation entropy (units of J*mol-1*K-1) for the organism/tissue/etc of interest.
#' @param deltaH The activation enthalpy (units of J*mol-1) for the organism/tissue/etc of interest. 
#' @param temp The threshold temperature when survival will begin to be impacted. Default is 48 for physiologically active tissue based on Miesel et al (2020) and personal communication with Matthew Dickinson.
#' @param res The temporal resolution (in min) at which to calculate the temperatures. Default is one minute.
#' @param model The model object (a list) from the SheFire function containing the equations and data that comprise the model.
#' @return This function returns a list containing the final survival (1 - 100 percent, 0 - no survival) and a data.frame of survival through time (each row is a time point). It also prints a text string with survival information.
#' @author MaryKBrady, based on work by Matthew B Dickinson
#' @seealso \code{\link{SheFire}}, \code{\link{SurvivalDepth}}
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- SheFire(input = inputFile )  #create model with all default settings
#' 
#' CassiaSeeds2cm <- SurvivalPercent(x = 2, deltaS = 112, deltaH = 147741, model = model_example) #default temporal resolution and temp. Cassia Seed survival, for parameters see Martin et al (1969)
#' Proteins1.79cm <- SurvivalPercent(x = 1.79, deltaS = 482, deltaH = 248410, res = 0.5, model = model_example) #default temp. Protein thermal denaturation parameters, See Dickinson and Johnson (2004) and Rosenberg et al (1971)
#' 
#' #plot survival
#' PlotSurvival <- data.frame(Proteins1.79cm$table.Time, Proteins1.79cm$table.Survival)
#' colnames(PlotSurvival) <- c("Time", "Survival")
#' ggplot(PlotSurvival, aes (x = Time, y = Survival)) +
#'   geom_line() +
#'   ylim(0,1) +
#'   labs(x = "Time (min)", y = "Survival", title = "Protein Survival Over Time")
#' @export

SurvivalPercent <- function(x, deltaS, deltaH, res = 1, temp = 48, model){
  #x and res checked in TempOverTime function
  if(mode(deltaS) != "numeric" | length(deltaS) != 1){
    stop("deltaS must be a single number")
  }
  if(mode(deltaH) != "numeric" | length(deltaH) != 1){
    stop("deltaH must be a single number")
  }
  RGas <- 8.31  #Universal gas constant, units of J K-1 mol-1
  kBoltz <- 1.38*10^(-23)  #Boltzman constant, J·K-1
  hPlanck <- 6.63*10^(-34)  #Planck's constant J·s
  equation <- TempOverTime(x = x, res = res, model = model)
  #browser()
  survival <- 1   #survival starts at 100 percent
  table <- matrix(nrow = length(equation), ncol = 2)  #creates output table
  for (a in seq(equation)){  #for each time point
    #browser()
    #print(a)
    if(equation[a] >= temp){  #if the temp is equal to or over set threshold
      TempK <- equation[a]+273.15 #Convert temp to Kelvin 
      timepoint.mortality <- (((kBoltz*TempK)/hPlanck)*exp(deltaS/RGas)*exp(-deltaH/(RGas*TempK))) * (res * 60) #mortality per second * # of seconds in each time step (res)
      survival <- survival - (timepoint.mortality * survival)  #total survival thus far = total survival previous - this time point effect on the previous survival
      if (survival < 0){ #if no survival, set to 0
        survival <- 0
      }
    }
    time <- seq(from = 0.00001, to = length(equation), by = res)
    table[a,] <- c(time[a], survival)  #add time point and survival up through this time point to the table
  }
  print(paste0("The survival at ", x, " cm deep was ", round(survival, digits = 3), " (", round(survival, digits = 3)*100, "%)"))
  table.df <- as.data.frame(table)
  colnames(table.df) <- c("Time", "Survival")
  returnlist <- c("table" = table.df, "survival" = survival)
  return(returnlist)
}
