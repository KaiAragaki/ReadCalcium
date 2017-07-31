readCalcium <- function(dateArg, ...) {
  
  # Notes:
  # This takes in an xls file, which in actuality is a tab-delimited text file with an xls extension. Saving things as xls simply will not do. They will not be able to be opened by this program.
  # This function takes two kinds of arguments - one, the date of which the experiment was performed, two, the names of the conditions, not inluding ionomycin.
  # This program requires a fairly strict file structure. If the file is moved, this program will cease to work until either file location or path to file changes to match.
  
  # TODO:
  # General Polish
  # Maybe have Ionomycin not default?
  
  conditionNames <- c(..., "Ionomycin")

  # Sets baseline and standard condition time intervals
  workingInterval <- c(0:4)
  additionalConditionTime <- c(1, 1.5, 2, 3, 4, 6)
  conditionAddTimes <- c(5)
  
  # Loops for each condition and appends the time - not including Ionomycin time, which has its own interval.
  for (i in 1:(length(conditionNames) - 1)){
    workingAdditionalCondition <- additionalConditionTime + tail(workingInterval, n=1)
    workingInterval <- c(workingInterval, workingAdditionalCondition)
    conditionAddTimes <- c(conditionAddTimes, tail(workingInterval + 1, n=1))
  }
  
  # Tacks on ionomycin time
  ionomycinInterval <- tail(workingInterval, n=1) + c(1, 1.5, 2, 3, 4)
  finalIntervalTimes <- c(workingInterval, ionomycinInterval)
  
  # Reads and wrangles tab delimited file. If Lab folder position changes, this will break!
  defaultPath <- file.path("~", "Desktop", "Lab", "Experiments", "Calcium", "By Subject", "INS 832:3 Glucose Response")
  variablePath <- file.path(dateArg, paste0(dateArg,"Results.xls"))
  totalPath <- file.path(defaultPath, variablePath)
  setwd(file.path(defaultPath, dateArg))
  calciumFile <- read.delim(totalPath)
  calciumFile <- calciumFile[,-1]
  colnames(calciumFile) <- c("Background", paste("Cell", 1:(ncol(calciumFile) - 1), sep = " "))
  
  # Creates empty matrices of appropriate dimensions and labelling
  calcium340 = calcium380 = data.frame(matrix(nrow = nrow(calciumFile)/2, ncol = ncol(calciumFile) - 1))
  baseline = data.frame(matrix = nrow(calcium340), ncol = 1)
  dimnames(calcium380) = dimnames(calcium340) <- list(finalIntervalTimes, paste("Cell", 1:(ncol(calciumFile) - 1), sep = " "))
  
  # Variable for background. Assumes first ROI is background.
  background <- calciumFile[,1]
  
  # Subtracts background from each cell
  minusBG <- calciumFile
  for (i in 2:ncol(calciumFile)){
    minusBG[,(i-1)] <- calciumFile[,i] - background
  }
  
  # Creates vectors of odd and even rows. Assumes 340 was first
  calcium340 <- minusBG[seq(1, nrow(minusBG), 2),]
  calcium380 <- minusBG[seq(2, nrow(minusBG), 2),]
  
  # Stores 340/380
  calciumRatio <- calcium340/calcium380
  rownames(calciumRatio) <- finalIntervalTimes
  
  # Represents as % dynamic range
  baseline <- colMeans(calciumRatio[1:5,])
  finalVal <- tail(calciumRatio, 1)
  dynamicRange <- finalVal - baseline
  dynamicCalcium <- sweep(calciumRatio, 2, baseline, "-")
  dynamicR <- sweep(as.matrix(dynamicCalcium), 2, as.matrix(dynamicRange), "/")
  dynamicR <- as.data.frame(dynamicR)
  
  ## Unfinished
  
  # Produces data frames, then saves formated plot to the folder from which the data came.
  calcium380 <- data.frame(x = finalIntervalTimes, y = stack(calcium380))
  calcium340 <- data.frame(x = finalIntervalTimes, y = stack(calcium340))
  calciumRatio <- data.frame(x = finalIntervalTimes, y = stack(calciumRatio))
  dynamicR <- data.frame(x = finalIntervalTimes, y = stack(dynamicR))
  
  # 380 Plot
  ggplot(data = calcium380, aes(x=calcium380$x, y=calcium380$y.values, color = calcium380$y.ind)) + 
    geom_point() + 
    geom_line() + 
    labs(x = "Time (min)", y = "", title = "380") + 
    theme(plot.title = element_text(hjust = 0.5), legend.position ="none") + 
    geom_vline(xintercept = (conditionAddTimes), color = "red") +
    annotate("text", x=conditionAddTimes, y=max(calcium380[2]), label = conditionNames, size=10)
  ggsave("380.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340 Plot
  ggplot(data = calcium340, aes(x=calcium340$x, y=calcium340$y.values, color = calcium340$y.ind)) +
    geom_point() + 
    geom_line() + 
    labs(x = "Time (min)", y = "", title = "340") + 
    theme(plot.title = element_text(hjust = 0.5), legend.position ="none") + 
    geom_vline(xintercept = (conditionAddTimes), color = "red") +
    annotate("text", x=conditionAddTimes, y=max(calcium340[2]), label = conditionNames, size=10)
  ggsave("340.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 Plot
  ggplot(data = calciumRatio, aes(x=calciumRatio$x, y=calciumRatio$y.values, color = calciumRatio$y.ind)) + 
    geom_point() + 
    geom_line() + 
    labs(x = "Time (min)", y = "", title = "340/380") + 
    theme(plot.title = element_text(hjust = 0.5), legend.position ="none") + 
    geom_vline(xintercept = (conditionAddTimes), color = "red") +
    annotate("text", x=conditionAddTimes, y=max(calciumRatio[2]), label = conditionNames, size=10)
  ggsave(paste("340\\380.pdf"), device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 as % Dynamic Range Plot
  ggplot(data = dynamicR, aes(x=dynamicR$x, y=dynamicR$y.values, color = dynamicR$y.ind)) + 
    geom_point() + 
    geom_line() + 
    labs(x = "Time (min)", y = "", title = "340/380 as % Dynamic Ratio") + 
    theme(plot.title = element_text(hjust = 0.5), legend.position ="none") + 
    geom_vline(xintercept = (conditionAddTimes), color = "red") +
    annotate("text", x=conditionAddTimes, y=max(dynamicR[2]), label = conditionNames, size=10)
  ggsave(paste("340\\380 As %% Dynamic Ratio.pdf"), device = "pdf", width = 16, height = 9, units = "in")
}