#' SheFire Model
#' 
#' This function takes data from three co-located soil temperature sensors at different depths during a fire and fits an equation that closely matches the typical rise and fall of temperatures in soils heated by wildland fires. The equation originated in the building fire literature and is called the BFD equation (Barnett 2002). With parameter regression set to T, the model will also calculate regression equations for each of the BFD equation parameters. Those regressions enable temperature over time during the fire to be modeled at unmeasured soil depths. 
#' 
#' Example data from US Forest Service Fire Behavior Assessment Team: https://www.fs.fed.us/adaptivemanagement/reports/fbat/2019_FBATReport_WalkerFire_10112019_Final.pdf
#' Papers Cited in this function: Barnett, C.R., 2002. BFD curve: a new empirical model for fire compartment temperatures. Fire Safety Journal, 37(5), pp.437-463.
#' 
#' @param input Data.frame of data organized with columns: Date.Time, TimeCounter, Temp_S, Temp_M, Temp_D (columns must be in this order) where S is the shallow sensor, M is the middle sensor, and D is the deep sensor. Rows: each time point. TimeCounter must start at 1*TimeStep (data logging rate), not 0.
#' @param sensor.depths List of the temperature sensor depths in centimeters.
#' @param cutOff The time (min) after the shallow sensor's maximum temperature that the code will cut off the data set for fitting. It will use this cut off time unless the temps rise again (ie diurnal heating) or the data set ends first in which cases, those will be set as the end points. The default is 24 hours (1440 min) but look at your data and do what makes sense (Some hot, smoldering fires will be cooling for more than 24 hrs).
#' @param override.clip This parameter will prevent the function from clipping the data set. Default is False. Only change this if you have manually clipped the input data to exactly how you want it analyzed.
#' @param moving.window This option smooths the data when locating the starting point for heating in the shallow sensor. This can be useful in low temperature fires in particular, when the fire heating may not be faster than diurnal heating on a point to point basis. The default is False.
#' @param window.size The size (in time points) of the window that is used in smoothing with moving.window. Default is 3.
#' @param regression This will have the function calculate the parameter regressions which can be used to extrapolate or interpolate to other soil depths. Default is True. Set to False if you are only interested in the equations for the sensor depths.
#' @param res Only needed if you are not calculating the regressions. This sets the temporal resolution (in min) for the output temperatures from the equations fit to the sensor data. The temp values (at every time point based on the temporal resolution) are returned in the return list at the end of the function (ie temperature at every minute, or every half minute, or every 5 minutes, etc). Default is 1 minute.
#' @param corr.threshold Threshold for accepted level of correlation between the input data and the fitted BFD equations as well as between the calculated regression equations and the BFD parameters from the three sensor depths. Correlations less than the threshold will cause the function to throw an error message. Default is 0.8.
#' @param time.buffer The time (min) added before the temperature begins to rise for the shallow sensor. If you do need extrapolations to depths shallower than your shallow sensor or if you are not calculating the regressions, you can set time.buffer to 0. The buffer is needed to be able to extrapolate to shallower depths with the parameter regressions. You want to keep the time.buffer as short as possible because longer buffers can worsen the fits for equations and regressions. The default is set to 30 minutes.
#' @param print.plots.table Print the standard plots of data (plotted at the temporal resolution of the input data, not "res") and equations as well as the two summary tables containing equation details and fit information. Default is False.
#' @param save.plots.table Save the standard plots of data (plotted at the temporal resolution of the input data, not "res") and equations as jpeg images and the summary tables containing equation details and fit information as CSV files. Default is False.
#' @param save.name The name that will be used for saving the plots and tables. Default is "SheFire".
#' @param save.directory File path for where to save the plots and tables, if different from current working directory.
#' @return If not calculating the regressions, the function returns a list of the equation values at time intervals set in res, the time resolution (res), the summary data.frame containing the equation parameters, and information about the time at the beginning and end of the clipped data set used to calculate those equations. If calculating the regressions, the function returns a list of all the equations and data necessary calculate temperature over time for a range of soil depths and run the application functions in this package. The list contains: BFDEquation - a function for calculating the temp over time given the BFD parameters; MaxTemp.reg, TimeAtMax.reg, Shape.reg, InitTemp.reg - functions to calculate the BFD parameters for a given soil depth; MaxTemp.coeffs, TimeAtMax.coeffs, Shape.coeffs, InitTemp.coeffs - the coefficients calculated for their respective parameter functions; InitTemp.byDepth - additional parameter needed for InitTemp.reg function, list of InitTemps calculated for the sensor depths; sensor.depths - additional parameter needed for InitTemp.reg function, list of sensor depths; Shallowest - shallowest depth (in cm) that it is mathematically possible for the model to calculate (note: shallowest depth for which the model is reasonable may be slightly deeper, see notes elsewhere in package about checking shallow depth for reasonability); FullTime - time in minutes that the model covers, StartTime - time at the beginning of the model time range, EndTime - time at the end of the model time range.
#' @author MaryKBrady, based on work by Matthew B Dickinson
#' @keywords Fire, Soil, Temperature
#' @examples 
#' inputFile <- read.csv(file.path(system.file("external", package="SheFire"), "WalkerPlot4NE.csv")) #read in example data
#' model_example <- shefire(input = inputFile )  #create model with all default settings
#' @export

shefire <- function(input, sensor.depths = c(5,10, 15), cutOff = 1440, override.clip = F, moving.window = F, window.size = 3, 
                    regression = T, res = 1, corr.threshold = 0.80, time.buffer = 30, print.plots.table = F, save.plots.table = F, save.name = "SheFire", 
                    save.directory = getwd()){
  if(colnames(input)[1] != "Date.Time"| colnames(input)[2] != "TimeCounter"| colnames(input[3]) != "Temp_S"| colnames(input)[4] != "Temp_M"| colnames(input)[5] != "Temp_D"){
    stop("input must have columns: Date.Time, TimeCounter, Temp_S, Temp_M, and Temp_D in that order. See help page for more details")
  }
  TimeStep <- input[1,2]  #TimeStep (data logging rate in sensors) based on first row of input
  if (TimeStep == 0) {
    stop("TimeCounter should start at one TimeStep, not zero. Please fix input")
  }
  if(length(sensor.depths) != 3 | mode(sensor.depths) != "numeric"){
    stop("You need to have three numeric sensor depths to run this model")
  }
  if(length(cutOff) != 1 | mode(cutOff) != "numeric"){
    stop("cutOff must be a single number")
  }
  if(override.clip != T & override.clip != F){
    stop("override.clip must be a boolean T or F")
  }
  if(moving.window != T & moving.window != F){
    stop("moving.window must be a boolean T or F")
  }
  if(length(window.size) != 1 | mode(window.size) != "numeric"){
    stop("window.size must be a single number")
  }
  if(regression != T & regression != F){
    stop("regression must be a boolean T or F")
  }
  if(length(time.buffer) != 1 | mode(time.buffer) != "numeric"){
    stop("time.buffer must be a single numer")
  }
  if(print.plots.table != T & print.plots.table != F){
    stop("print.plots.table must be a boolean T or F")
  }
  if(save.plots.table != T & save.plots.table != F){
    stop("save.plots.table must be a boolean T or F")
  }
  #packages
  if(moving.window == T){
    require(evobiR)
  }
  if(print.plots.table == T | save.plots.table ==T){
    require(ggplot2)
    require(ggpubr)
    PlotTheme <- theme(panel.background = element_rect(fill = "white"),
                       panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                       plot.title = element_text(colour = "black", face = "bold", size = rel(1.7), hjust = 0.5),
                       axis.line = element_line(size = 1, colour = "black"),
                       axis.title = element_text(colour="black", size = rel(1.2)),
                       axis.text = element_text(colour = "black", size = rel(1)))
    PlotThemeMulti <- theme(panel.background = element_rect(fill = "white"),
                            panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                            plot.title = element_text(colour = "black", face = "bold", size = rel(1.3)),
                            axis.line = element_line(size = 1, colour = "black"),
                            axis.text = element_text(colour = "black", size = rel(1)))
  }
  if(save.plots.table == T){ #set working directory for saving
    og_dir <- getwd() #keep this to reset at end of function
    setwd(save.directory)
  }
  #This is the BFD equation:
  BFDEquation <- function(x, InitTemp.value, MaxTemp.value, TimeAtMax.value, Shape.value)
  {InitTemp.value + (MaxTemp.value-InitTemp.value)*exp(-(((log(x))-log(TimeAtMax.value))^2)/Shape.value)}
  #power model - works well for Shape regression, sometimes works well for Initial Temp regression
  Power.equation <- function(x, a, b) {a*(x^-b)}
  #Setting StartInd 
  Data_S <- input[,3] #pull out shallow sensor data
  NumRows <- length(Data_S)                                            
  if (override.clip == F) {
    TempRate <- matrix(0, nrow= NumRows, ncol= 1) #initialize matrix                            
    for (a in 1:NumRows-1) {                                                  
      TempRate[a,1] <- (Data_S[a+1] - Data_S[a])/TimeStep #Temp change = next.temp-temp/timestep
    }
  }
  if (moving.window == T & override.clip == F) {
    TempRate.mw <- SlidingWindow("mean", TempRate, window.size, 1) #calculates the mean rate for each window
    prespike <- TempRate.mw[1:(which.max(TempRate.mw))] #mean rates up to max mean rate (occurs early in heating)
    indices <- which(prespike == 0) #all times before max mean rate when rate is zero
    StartInd <- tail(indices, n=1) - round(time.buffer/TimeStep) #StartInd is the time of last 0 mean rate moved 'time.buffer' minutes (rounded to nearest whole number) earlier
  }
  if(moving.window == F & override.clip == F) {
    prespike <- TempRate[1:(which.max(TempRate))] #all rates up to the max rate (occurs early in heating)
    indices <- which(prespike == 0) #all times before max rate when rate is zero
    StartInd <- tail(indices, n=1) - round(time.buffer/TimeStep)#StartInd is the time of last 0 rate moved 'time.buffer' minutes (rounded to nearest whole number) earlier
    if(!(length(indices)>=1)){ #if there is never a change of 0 
      StartInd <- 1*TimeStep #then set the StartInd to the beginning
      print("Warning: StartInd was set to first time point because there was no time with 0 temp change before the peak. You may want to look at your data and set the StartInd manually (adjust the code or clip the input data to a logical start point)")
    }
    if(StartInd <= 0){  #if time.buffer sets StartInd to be negative
      StartInd <- 1*TimeStep #then set set StartInd to the beginning
      print("Warning: StartInd was set to the first time point because time.buffer was set too large for this input data (it would have set the StartInd prior to the start of the data)")
    }
  } 
  #Setting EndInd
  if (override.clip == F) {
    MaxInd <- which.max(Data_S) #when is the temp peak
    postpeak <- TempRate[(which.max(Data_S)):NumRows] #rates of temp change after temp peak
    EndInd.1 <- NULL
    for (w in seq(postpeak)){
      if (w == (length(postpeak)-2)){ #if too close to end of data to check for sustained temp rise
        break
      }
      if (postpeak[w] > 0 & Data_S[MaxInd + w] < Data_S[MaxInd + w+2]){ #if temp increases and the temp stays elevated
        EndInd.1 <- MaxInd + w #first time point heating began again post peak
        break
      }
    }
    if (is.null(EndInd.1) ){ #if no heating post peak, set possible end point to end of data
      EndInd.1 <- NumRows
    }
    EndInd.2 <- MaxInd + (cutOff/TimeStep)#CutOff time after the shallow temp peak as a possible end point
    if (EndInd.1 < EndInd.2 & EndInd.1 < NumRows){ #find if temp rise post peak, CutOff time, or end of the data set happens first then set that as the EndInd
      EndInd <- EndInd.1                              
      print("EndInd (end of the data used for fitting) was set when the shallow sensor began to heat again after the peak")
    } else if (EndInd.2 < EndInd.1 & EndInd.2 < NumRows) {   
      EndInd <- EndInd.2
      print("EndInd (end of the data used for fitting) was set by the specified CutOff time after the shallow temperature peak")
    } else {
      EndInd <- NumRows
      print("EndInd (end of the data used for fitting) was set as the end of the input data set")
    }
  }
  #clip data
  if(override.clip == F){
    input.clipped <- input[c(StartInd:EndInd),]  #subset original data to the ranged used for fitting
    input.clipped$TimeCounter <- input.clipped$TimeCounter - (StartInd*TimeStep)  #restart the timecounter in subset
    rownames(input.clipped) = seq(length=nrow(input.clipped)) #restart row names
  }else {
    input.clipped <- input  #if no clipping, rename input so it matches the clipped data name for rest of the code
  }
  #Fit BFD equations
  Results <- matrix(ncol = 6, dimnames = list(c(), c("RMSE", "Pearson","InitTemp", "MaxTemp", "TimeAtMax", "Shape")))    
  timecounter <- input.clipped$TimeCounter  #time needs to be in this format to go into equation
  sensorlist <- list("Temp_S", "Temp_M", "Temp_D")  #Column names for temperature data
  for(c in names(input.clipped)){   #names() pulls up all the column names
    if(c %in% sensorlist){     #only acts on the Temp columns
      InitTemp.fit <- input.clipped[1,c]        #sets parameter starting values for nls
      MaxTemp.fit <- max(input.clipped[[c]])
      MaxInd.fit <- which.max(input.clipped[[c]])
      TimeAtMax.fit <- input.clipped$TimeCounter[MaxInd.fit]
      BFD.fit <- nls(input.clipped[[c]]~BFDEquation(timecounter, InitTemp.value, MaxTemp.value, TimeAtMax.value, Shape.value), start = list(InitTemp.value = InitTemp.fit, MaxTemp.value = MaxTemp.fit, TimeAtMax.value = TimeAtMax.fit, Shape.value = 0.1))                    #call nls function
      RMSE <- summary(BFD.fit)$sigma  #root mean square error
      Parameters.fit <- summary(BFD.fit)$parameters[,1] #equation parameters
      InitTemp.fit <- Parameters.fit[1]   #isolate parameter values
      MaxTemp.fit <- Parameters.fit[2]
      TimeAtMax.fit <- Parameters.fit[3]
      ShapeConstant.fit <- Parameters.fit[4]
      Temporary.fit <- BFDEquation(timecounter, InitTemp.fit, MaxTemp.fit, TimeAtMax.fit, ShapeConstant.fit) #Make temporary equation for calculating Pearson coefficient
      Pearson <- cor.test(Temporary.fit, input.clipped[[c]], method = "pearson") #pearson correlation coefficient
      Results <- rbind(Results, c(RMSE, Pearson$estimate, Parameters.fit))       #add to results
    }
  }
  Results.df <- data.frame(na.omit(Results)) #data frame is easier to work with. remove first row of NAs (product of using rbind)
  Results.df$Depth <- sensor.depths #add depth
  #Recreate the equations
  timecounter.res <- seq(from = 1, to = max(timecounter), by = res) #time at interval set by res parameter
  #Shallow
  InitTemp.S <- Results.df[1,3]
  MaxTemp.S <- Results.df[1,4]
  TimeAtMax.S <- Results.df[1,5]
  ShapeConstant.S <- Results.df[1,6]
  RMSE.S <- Results.df[1,1]
  Pearson.S <- Results.df[1,2]
  BFDEquation.S <- BFDEquation(timecounter.res, InitTemp.S, MaxTemp.S, TimeAtMax.S, ShapeConstant.S)
  #Middle
  InitTemp.M <- Results.df[2,3]
  MaxTemp.M <- Results.df[2,4]
  TimeAtMax.M <- Results.df[2,5]
  ShapeConstant.M <- Results.df[2,6]
  RMSE.M <- Results.df[2,1]
  Pearson.M <- Results.df[2,2]
  BFDEquation.M <- BFDEquation(timecounter.res, InitTemp.M, MaxTemp.M, TimeAtMax.M, ShapeConstant.M)
  #Deep
  InitTemp.D <- Results.df[3,3]
  MaxTemp.D <- Results.df[3,4]
  TimeAtMax.D <- Results.df[3,5]
  ShapeConstant.D <- Results.df[3,6]
  RMSE.D <- Results.df[3,1]
  Pearson.D <- Results.df[3,2]
  BFDEquation.D <- BFDEquation(timecounter.res, InitTemp.D, MaxTemp.D, TimeAtMax.D, ShapeConstant.D)
  #Make summary table from Results.df 
  if(print.plots.table == T | save.plots.table == T | regression == F){
    Results.df <- Results.df[,c("Depth", "InitTemp", "MaxTemp", "TimeAtMax", "Shape", "RMSE", "Pearson")] #reorder
    summary.table <- c("", "Shallow", "Middle", "Deep", "Formula", "StartInd to real time conversion", "StartInd", "End of data set used")
    summary.table.df <- data.frame(summary.table)
    summary.table.df[2:4, 2:8] <- Results.df
    summary.table.df[1,] <- c("", "Depth", "InitTemp", "MaxTemp", "TimeAtMax", "Shape", "RMSE", "Pearson")
    summary.table.df[,2] <- c("Depth", sensor.depths, "", "", "", "")
    summary.table.df[5,2:3] <- c("Temp = InitTemp + (MaxTemp - InitTemp)e^(-z)", "z = (log(time) - log(TimeAtMax))^2 / ShapeConstant")
    summary.table.df[7,2] <- as.character(input.clipped$Date.Time[1])
    summary.table.df[8,2:3] <- c(as.character(input.clipped$Date.Time[nrow(input.clipped)]), paste0("Time elapsed (min): ", length(BFDEquation.S)*TimeStep))
    colnames(summary.table.df) <- c("", "", "", "", "", "", "", "")
    summary.table.df <- rbind(c("Summary Table", "", "", "","", "", "", "") , summary.table.df)
  }
  #Make plots
  if(print.plots.table == T | save.plots.table == T){
    #need to plot on same temporal resolution as input data
    BFDEquation.S.plot <- BFDEquation(timecounter, InitTemp.S, MaxTemp.S, TimeAtMax.S, ShapeConstant.S)
    BFDEquation.M.plot <- BFDEquation(timecounter, InitTemp.M, MaxTemp.M, TimeAtMax.M, ShapeConstant.M)
    BFDEquation.D.plot <- BFDEquation(timecounter, InitTemp.D, MaxTemp.D, TimeAtMax.D, ShapeConstant.D)
    #Plot Clipped Data
    A <- ggplot(input.clipped, aes(x = TimeCounter)) + #raw data
      geom_point(aes(y = Temp_S), color = "purple") +
      geom_point(aes(y = Temp_M), color = "blue") +
      geom_point(aes(y = Temp_D), color = "green") +
      labs(y = NULL, x = NULL) +
      PlotThemeMulti
    #Plot Shallow Sensor
    B <- ggplot(input.clipped, aes(x = TimeCounter)) +
      geom_point(aes(y = Temp_S), color = "black") +
      geom_line(aes(y = BFDEquation.S.plot), color = "purple", size = 1) +
      labs(y = NULL, x = NULL) + 
      PlotThemeMulti
    #Plot Middle Sensor
    C <- ggplot(input.clipped, aes(x = TimeCounter)) +
      geom_point(aes(y = Temp_M), color = "black") +
      geom_line(aes(y = BFDEquation.M.plot), color = "blue", size = 1) +
      labs(y = NULL, x = NULL) +
      PlotThemeMulti
    #Plot Deep Sensor
    D <- ggplot(input.clipped, aes(x = TimeCounter)) +
      geom_point(aes(y = Temp_D), color = "black") +
      geom_line(aes(y = BFDEquation.D.plot), color = "green", size = 1) +
      labs(y = NULL, x = NULL) +
      PlotThemeMulti
    Figure <- ggarrange(A,B,C,D, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
    Figure_an <- annotate_figure(Figure, bottom = text_grob(paste0("Time (min) since StartInd \nA. Clipped Data, B. Shallow Sensor (RMSE: ", round(RMSE.S, digits = 3), ", Pearson: ", round(Pearson.S, digits = 3), "), \nC. Middle Sensor (RMSE: ", round(RMSE.M, digits = 3), ", Pearson: ", round(Pearson.M, digits = 3), "), D. Deep Sensor (RMSE: ", round(RMSE.D, digits = 3), ", Pearson: ", round(Pearson.D, digits = 3), ")")), left = text_grob("Temperature (C)", rot = 90))
    
    All3 <- ggplot(input.clipped, aes(x = TimeCounter)) +  
      geom_line(aes(y = BFDEquation.S.plot), color = "purple", size = 1) +
      geom_line(aes(y = BFDEquation.M.plot), color = "blue", size = 1) +
      geom_line(aes(y = BFDEquation.D.plot), color = "green", size = 1) +
      ggtitle("All three equations") +
      labs(y="Temperature (C)", x = "Time (min) since StartInd") + 
      PlotTheme +
      annotate("text", x = (max(timecounter)-(max(timecounter)/10)), y = MaxTemp.S, label = paste0("\n\nShallow - Purple \nMiddle - Blue \nDeep - Green"), hjust = 1)
    
    input_plot <- ggplot(input, aes(x = TimeCounter)) + 
      geom_point(aes(y = Temp_S), color = "purple") +
      geom_point(aes(y = Temp_M), color = "blue") +
      geom_point(aes(y = Temp_D), color = "green") +
      ggtitle("Input Data") + 
      labs(y="Temperature (C)", x = "Time (min)") + 
      PlotTheme
  }
  #printing and saving
  if(print.plots.table == T & regression == F){  #print all plots and tables together at the end if continuing to regressions
    print(input_plot)
    print(Figure_an)
    print(All3)
    print(summary.table.df)
  }
  if(save.plots.table == T){
    ggsave(paste0(save.name,"_input.jpeg"), plot = input_plot)
    ggsave(paste0(save.name, "_dataWithEqua.jpeg"), plot = Figure_an)
    ggsave(paste0(save.name, "_equations.jpeg"), plot = All3)
    write.csv(summary.table.df, paste0(save.name, "_summarytable.csv"))
  }
  #Time info
  FullTime <- input.clipped$TimeCounter[nrow(input.clipped)]  #length of time covered by model and equations
  StartTime <- as.character(input.clipped$Date.Time[1])  #just a character string, not a formal date formatted object b/c input might vary
  EndTime <- as.character(input.clipped$Date.Time[nrow(input.clipped)])
  #end function if not calculating regressions
  if(regression == F){
    half.list <- list("BFDEquation.S" = BFDEquation.S, "BFDEquation.M" = BFDEquation.M, "BFDEquation.D" = BFDEquation.D, "Time.Resolution" = res, "Summary.Data.Frame" = summary.table.df, "EndTime" = EndTime, "StartTime" = StartTime, "FullTime" = FullTime)
    print("Not calculating regressions for full model, returning the three equations for temperature over time at the set time resolution (BFDEquation.S - shallow sensor, .M - middle sensor, .D - deep sensor), Time.Resolution (in min), Summary Table, StartTime, EndTime, and FullTime (length of time covered in minutes)")
    return(half.list)
  }
  #calculate regressions for each parameter
  #linear models - works well for Max Temp and Time at Max
  MaxTemp.lm <- lm(log(Results.df$MaxTemp) ~ log(Results.df$Depth))   #linear model (log, log transformed)
  MaxTemp.lm.r <- summary(MaxTemp.lm)$r.squared                       #r squared
  MaxTemp.coeffs <- c(MaxTemp.lm$coefficients[1], MaxTemp.lm$coefficients[2]) #coefficients for regression
  MaxTemp.reg <- function(x, MaxTemp.coeffs) {return(exp(MaxTemp.coeffs[1] + (MaxTemp.coeffs[2])*log(x)))} #regression 
  if (MaxTemp.lm.r < corr.threshold) {
    stop(paste("Calculating regessions failed because the r squared for max temp regression is below the threshold with a value of:", MaxTemp.lm.r, " The correlation threshold can be adjusted but this parameter typically has a high r2"))
  }
  TimeAtMax.lm <- lm(Results.df$TimeAtMax ~ Results.df$Depth)     #linear model (no log transformations)
  TimeAtMax.lm.r <- summary(TimeAtMax.lm)$r.squared               #r squared
  TimeAtMax.coeffs <- c(TimeAtMax.lm$coefficients[1], TimeAtMax.lm$coefficients[2]) #coefficients for regression
  TimeAtMax.reg <- function(x, TimeAtMax.coeffs) {return(TimeAtMax.coeffs[1] + TimeAtMax.coeffs[2]*x)} #regression
  if (MaxTemp.lm.r < corr.threshold) {
    stop(paste("Calculating regeressions failed because the r squared for time at max regression is below the threshold with a value of:", TimeAtMax.lm.r, " The correlation threshold can be adjusted but this parameter typically has a high r2"))
  }
  #Not linear models - works well for Shape and InitTemp
  Shapesfornls <- Results.df$Shape  #change data format because cannot put Results.df$... into nls
  Shape.nls <- nls(Shapesfornls ~ Power.equation(sensor.depths, a, b), start = list(a = 10, b = 1)) 
  Shape.coeffs <- c(summary(Shape.nls)$parameters[1,1], summary(Shape.nls)$parameters[2,1]) #parameters from nls
  Shape.reg <- function(x, Shape.coeffs) {return(Shape.coeffs[1]*(x^-Shape.coeffs[2]))} #function
  predicted.shapes <- Shape.reg(sensor.depths, Shape.coeffs = Shape.coeffs)   #calculate values for pearson correlation
  Shape.pearson <- cor.test(Shapesfornls, predicted.shapes, method = "pearson")
  Initfornls <- Results.df$InitTemp  #change data format because cannot put Results.df$... into nls
  InitTemp.nls <- nls(Initfornls ~ Power.equation(sensor.depths, a, b), start = list(a = 30, b = 0.05))
  InitTemp.coeffs <- c(summary(InitTemp.nls)$parameters[1,1], summary(InitTemp.nls)$parameters[2,1]) #parameters from nls
  InitTemp.byDepth <- c(InitTemp.S, InitTemp.M, InitTemp.D)
  InitTemp.reg <- function(x, InitTemp.coeffs, InitTemp.byDepth, sensor.depths) {return(InitTemp.coeffs[1]*(x^-InitTemp.coeffs[2]))} #function
  predicted.inits <- InitTemp.reg(sensor.depths, InitTemp.coeffs = InitTemp.coeffs)  #calculate values for pearson correlation
  InitTemp.pearson <- cor.test(Initfornls, predicted.inits, method = "pearson")
  #Check shallowest depth and shape pearson
  Shallowest <- (1-TimeAtMax.coeffs[1])/TimeAtMax.coeffs[2] #calculates depth when TimeAtMax is 1 - shallowest that can be extrapolated
  if (Shallowest <= 0){ #if shallowest not ok
    print(paste("The shallowest you can extrapolate was set to 0.001 cm because it was calculated at", Shallowest, "but this at or above the soil surface. Conceptually, depths above the surface don't makes sense in this frame work and mathematically, you cannot have a depth less than or equal to 0 in the BFD equation. You can probably reduce time.buffer a bit to improve fit without losing any ability to extrapolate to all shallow depths"))
    Shallowest <- 0.001
  }else if (Shallowest > 0 & Shape.pearson$estimate >= corr.threshold){ #if shallowest ok and pearson ok
    print(paste("The shallowest depth the model can mathematically extrapolate to is", Shallowest, "cm. If you need a shallower depth, try increasing time.buffer. The model can become unreasonable at particularly shallow depths in some fires so the shalowest reasonable extrapolation may be a little deeper than that (plot shallowest depth temperature over time along side a deeper depth like 1 or 2 cm to visually assess reasonability if you need specific data at or near the shallowest depth)"))
  } else {#if shallowest ok but pearson not ok
    stop(paste("Calculating regressions failed because the pearson correlation for Shape parameter was lower than the threshold with a value of:", Shape.pearson$estimate, ". Decreasing time.buffer should improve the fit. Or adjust the pearson correlation threshold parameter"))
  }
  #Check InitTemp pearson
  if(InitTemp.pearson$estimate < corr.threshold){ #if InitTemp pearson correlation is below threshold, use closest sensor for InitTemp
    InitTemp.reg = function(x, InitTemp.coeffs, InitTemp.byDepth, sensor.depths) { #same parameters as previous function to standardize function calls
      if (x <= (sensor.depths[1] + ((sensor.depths[2]-sensor.depths[1])/2))){#if x  closest to  shallow sensor
        return(InitTemp.byDepth[1]) #shallow sensor InitTemp
      } else if (x > (sensor.depths[1] + ((sensor.depths[2]-sensor.depths[1])/2)) & x<= (sensor.depths[2] + ((sensor.depths[3]-sensor.depths[2])/2))){ #if x closest to middle sensor
        return(InitTemp.byDepth[2]) #middle sensor InitTemp
      }else if (x > (sensor.depths[2] + ((sensor.depths[3]-sensor.depths[2])/2))){ #if x closest to deep sensor
        return(InitTemp.byDepth[3]) #deep sensor InitTemp
      } else {
        print("error in InitTemp regression option 'closest.sensor'")
      }
    }
    ForTable <- "closest.sensor"
    print(paste("InitTemp pearson correlation was below the threshold with a value of:", InitTemp.pearson$estimate, "Therefore, the parameter regression will use the initial temperature from the nearest sensor as the initial temperature parameter instead of the calculated equation for InitTemp. The correlation threshold can be adjusted"))  
  } else {(ForTable <- "This should not be printed")} #set ForTable if Not using closest.sensor
  #regression summary table
  if(print.plots.table == T | save.plots.table ==T){
    regressionSummary <- c("MaxTemp", "InitTemp", "TimeAtMax", "Shape", "Shallowest", "StartTime ", "EndTime")
    regressionSummary.df <- as.data.frame(regressionSummary)
    regressionSummary.df[,2] <- c("r squared", "pearson r", "r squared", "pearson r", paste(Shallowest, "cm"), StartTime, EndTime)
    regressionSummary.df[,3] <- c(MaxTemp.lm.r, "", TimeAtMax.lm.r, Shape.pearson$estimate, "cannot extrapolate shallower", "beginning of model time range", "end of model time range ")
    if(ForTable == "closest.sensor") { #if change InitTemp
      regressionSummary.df[2,2:3] <- c("NA", "using initial temp from nearest sensor")
    }else { #if using initTemp equation
      regressionSummary.df[2,2:3] <- c("pearson r", InitTemp.pearson$estimate)
    }
    regressionSummary.df[1,4] <- paste0("e^(",round(MaxTemp.coeffs[1], digits = 4),"+(",round(MaxTemp.coeffs[2], digits = 4),"*log(x)))")
    if (ForTable ==  "closest.sensor") {
      regressionSummary.df[2,4] <- "nearest sensor InitTemp"
    }else {
      regressionSummary.df[2,4] <- paste0(round(InitTemp.coeffs[1], digits = 4),"*(x^-",round(InitTemp.coeffs[2], digits = 4),")")
    }
    regressionSummary.df[3,4] <- paste0(round(TimeAtMax.coeffs[1], digits = 4), "+", round(TimeAtMax.coeffs[2], digits = 4),"*x")
    regressionSummary.df[4,4] <- paste0(round(Shape.coeffs[1], digits = 4),"*(x^-",round(Shape.coeffs[2], digits = 4),")")
    regressionSummary.df[7,4] <- paste("FullTime: ", FullTime, "min") 
    regressionSummary.df <- rbind(c("Parameter", "Statistic", "Value", "Equation"), regressionSummary.df)
    colnames(regressionSummary.df) <- c("", "", "", "")
    regressionSummary.df <- rbind(c("regression Table", "", "", "") , regressionSummary.df)
  }
  #printing and saving 
  if(print.plots.table == T){
    print(input_plot)
    print(Figure_an)
    print(All3)
    print(summary.table.df)
    print(regressionSummary.df)
  }
  if(save.plots.table == T){
    write.csv(regressionSummary.df, paste0(save.name, "_regtable.csv"))
    setwd(og_dir)  #return working directory to what it was before function was called, nothing more to be saved
  }
  #end of function return values
  model.list <- list("BFDEquation" = BFDEquation, "MaxTemp.reg" = MaxTemp.reg, "MaxTemp.coeffs" = MaxTemp.coeffs, "TimeAtMax.reg" = TimeAtMax.reg, "TimeAtMax.coeffs" = TimeAtMax.coeffs, 
                     "Shape.reg" = Shape.reg, "Shape.coeffs" = Shape.coeffs, "InitTemp.reg" = InitTemp.reg, "InitTemp.byDepth" = InitTemp.byDepth, "InitTemp.coeffs" = InitTemp.coeffs, 
                     "SensorDepths" = sensor.depths, "Shallowest" = Shallowest, "FullTime" = FullTime, "StartTime" = StartTime, "EndTime" = EndTime)
  return(model.list)
}
