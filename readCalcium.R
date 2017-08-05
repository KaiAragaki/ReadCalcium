readCalciumNewMethod <- function(dateArg, ...) {
  
  # Notes:
  # This takes in an xls file, which in actuality is a tab-delimited text file with an xls extension. Saving things as xls simply will not do. They will not be able to be opened by this program.
  # This function takes the date of which the experiment was performed and a list of cond names, not including ionomycin
  # This program requires a strict file structure. If the file is moved, this program will cease to work until either file location or path to file changes to match.
  
  # TO DO
  # Maybe have Ionomycin not default?
  # Read CSV (Or check if CSV and read accordingly?)
  # Calculate max avg for each cond?
  # Port to Shiny?
  
  # Initiate Variables
  conds <- c(..., "Ionomycin")
  divergeStart <- divergeEnd <- c()
  cal340 <- cal380 <- baseline <- matrix()
  
  # Sets baseline and standard cond time intervals
  interval <- c(0:4)
  condInterval <- c(1.5, 2, 3, 4, 6)
  addTimes <- c(5) # Once I change my protocol to match Craig's, I can likely get rid of this variable
  
  # Loops for each cond and appends the time - not including Ionomycin time, which has its own interval.
  for (i in 1:(length(conds) - 1)){
    addtlCond <- condInterval + tail(interval, n=1)
    interval <- c(interval, addtlCond)
    addTimes <- c(addTimes, tail(interval + 1, n=1))
  }
  
  # Tacks on ionomycin time
  ionomycinInterval <- tail(interval, n=1) + c(1.5, 2, 3, 4)
  condTimes <- c(interval, ionomycinInterval)
  
  # Reads and wrangles tab delimited file. If Lab folder position changes, this will break!
  path <- file.path("~", "Desktop", "Lab", "Experiments", "Calcium", "By Subject", "INS 832:3 Glucose Response", dateArg)
  setwd(path)
  
  calFile <- read.delim(paste0(dateArg,"Results.xls"))
  calFile <- calFile[,-1]
  
  # Subtracts background from each cell
  minusBG <- mutate_all(calFile, function(x) {x - calFile[,1]})
  minusBG <- minusBG[,-1]
  
  # Creates vectors of odd and even rows. Assumes 340 was first
  cal340 <- minusBG[seq(1, nrow(minusBG), 2),]
  cal380 <- minusBG[seq(2, nrow(minusBG), 2),]
  
  # Stores 340/380
  calRatio <- cal340/cal380
  rownames(calRatio) <- condTimes
  
  # Represents as % dynamic range
  baseline <- colMeans(calRatio[1:5,])
  maxIono <- apply(tail(calRatio), 2, function(x) max(x))
  dynamicR <- sweep(sweep(calRatio, 2, baseline, "-"), 2, (maxIono - baseline), "/")
  
  # Row means for finding slope means
  rowMeans380 <- rowMeans(cal380)
  rowMeans340 <- rowMeans(cal340)

  # Produces data frames, then saves formated plot to the folder whence the data came.
  cal380 <- data.frame(x = condTimes, y = stack(cal380))
  cal340 <- data.frame(x = condTimes, y = stack(cal340))
  calRatio <- data.frame(x = condTimes, y = stack(calRatio))
  dynamicR <- data.frame(x = condTimes, y = stack(dynamicR))
  
  # Loops through row means and checks to see if, on average, the slopes diverge.
  for (i in 1:(length(condTimes)-1)){
    if(rowMeans380[i] - rowMeans380[i+1] > 50 & rowMeans340[i + 1] - rowMeans340[i] > 50  | rowMeans380[i+1] - rowMeans380[i] > 50 & rowMeans340[i] - rowMeans340[i + 1] > 50){
      divergeStart[i] <- condTimes[i]
      divergeEnd[i] <- condTimes[i+1]
    }
  }
  divergeStart <- divergeStart[!is.na(divergeStart)]
  divergeEnd <- divergeEnd[!is.na(divergeEnd)]
  rects <- data.frame(xstart = divergeStart, xend = divergeEnd)

  # Plots -----------------------------------------------------
    
  # Template Theme and Geoms
  calPlot <- list(theme(plot.title = element_text(hjust = 0.5), legend.position ="none", text = element_text(size = 20)), geom_point(), geom_line(), geom_vline(xintercept = (addTimes), color = "red"))
  
  # To keep ggplot happy when working with rectangles
  commonData <- aes(x=calRatio$x, y=calRatio$y.values, color = calRatio$y.ind)
  
  # 380 Plot
  ggplot(data = cal380, aes(x=cal380$x, y=cal380$y.values, color = cal380$y.ind)) + 
    calPlot +
    labs(x = "Time (min)", y = "", title = "380") + 
    annotate("text", x=addTimes, y=max(cal380[2]), label = conds, size=10)
  ggsave("380.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340 Plot
  ggplot(data = cal340, aes(x=cal340$x, y=cal340$y.values, color = cal340$y.ind)) +
    calPlot + 
    labs(x = "Time (min)", y = "", title = "340") + 
    annotate("text", x=addTimes, y=max(cal340[2]), label = conds, size=10)
  ggsave("340.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 Plot
  ggplot(data = calRatio) + 
    theme(plot.title = element_text(hjust = 0.5), legend.position ="none", text = element_text(size = 20)) +
    geom_point(commonData) +
    geom_line(commonData) +
    geom_vline(xintercept = (addTimes), color = "red") +
    labs(x = "Time (min)", y = "", title = "340/380") + 
    annotate("text", x=addTimes, y=max(calRatio[2]), label = conds, size=10)+
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = "darkslategray4", alpha = .2)
  ggsave(paste("340\\380.pdf"), device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 as % Dynamic Range Plot
  ggplot(data = dynamicR, aes(x=dynamicR$x, y=dynamicR$y.values, color = dynamicR$y.ind)) + 
    calPlot + 
    labs(x = "Time (min)", y = "", title = "340/380 as % Dynamic Ratio") + 
    annotate("text", x=addTimes, y=max(dynamicR[2]), label = conds, size=10)
  ggsave(paste("340\\380 As %% Dynamic Ratio.pdf"), device = "pdf", width = 16, height = 9, units = "in")
}
