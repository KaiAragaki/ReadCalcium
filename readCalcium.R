readCalciumTester <- function(dateArg, ...) {
  
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
    addtlCond <- condInterval + tail(interval, n = 1)
    interval <- c(interval, addtlCond)
    addTimes <- c(addTimes, tail(interval + 1, n = 1))
  }
  
  # Tacks on ionomycin time
  ionomycinInterval <- tail(interval, n = 1) + c(1.5, 2, 3, 4)
  condTimes <- c(interval, ionomycinInterval)
  
  # Reads and wrangles tab delimited file. If Lab folder position changes, this will break!
  path <- file.path("~", "Desktop", "Lab", "Experiments", "Calcium", "By Subject", "INS 832:3 Glucose Response", dateArg) %>%
  setwd()
  calFile <- read.delim(paste0(dateArg,"Results.xls"))
  calFile <- calFile[ ,-1]
  
  # Subtracts background from each cell
  minusBG <- mutate_all(calFile, function(x) {x - calFile[ ,1]})
  minusBG <- minusBG[ ,-1]
  
  # Creates vectors of odd and even rows. Assumes 340 was first. Calculates ratio.
  cal340 <- minusBG[seq(1, nrow(minusBG), 2),] # Want to make it so I can just put them in a dataframe right at the get go
  cal380 <- minusBG[seq(2, nrow(minusBG), 2),]
  calRatio <- cal340/cal380
  
  # Represents as % dynamic range
  baseline <- colMeans(calRatio[1:5,])
  maxIono <- apply(tail(calRatio), 2, function(x) max(x))
  dynamicR <- sweep(sweep(calRatio, 2, baseline, "-"), 2, (maxIono - baseline), "/") * 100
  
  # Row means for finding slope means
  rowMeans380 <- rowMeans(cal380)
  rowMeans340 <- rowMeans(cal340)
  
  # Putting it all together...
  calDF <- data.frame("Condition Times" = condTimes, "380" = stack(cal380), "340" = stack(cal340), "340/380" = stack(cal340/cal380), "340/380 Dyn" = stack(dynamicR))
 
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
  calCommon <- list(theme(plot.title = element_text(hjust = 0.5), legend.position ="none", text = element_text(size = 20)), labs(x = "Time (min)", y = ""), geom_vline(xintercept = (addTimes), color = "red", alpha = 0.5))
  calPlot <- list(calCommon, geom_point(), geom_line())
  
  # To keep ggplot happy when working with rectangles
  commonData_1 <- aes(x=Condition.Times, y=X340.380.values, color = X340.380.ind)
  commonData_2 <- aes(x=Condition.Times, y=X340.380.Dyn.values, color = X340.380.Dyn.ind)
  
  # 380 Plot
  ggplot(data = calDF, aes(x=Condition.Times, y=X380.values, color = X380.ind)) + 
    calPlot +
    labs(title = "380") + 
    annotate("text", x=addTimes, y=max(calDF$X380.values), label = conds, size=10)
  ggsave("380.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340 Plot
  ggplot(data = calDF, aes(x=Condition.Times, y=X340.values, color = X340.ind)) +
    calPlot + 
    labs(title = "340") + 
    annotate("text", x=addTimes, y=max(calDF$X340.values), label = conds, size=10)
  ggsave("340.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 Plot
  ggplot(data = calDF) + 
    calCommon +
    geom_point(aes(x=Condition.Times, y=X340.380.values, color = X340.380.ind)) +
    geom_line(aes(x=Condition.Times, y=X340.380.values, color = X340.380.ind)) +
    labs(title = "340/380") + 
    annotate("text", x=addTimes, y=max(calDF$X340.380.values), label = conds, size=10) +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = "steelblue4", alpha = .2)
  ggsave(paste("340\\380.pdf"), device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 as % Dynamic Range Plot
  ggplot(data = calDF) + 
    calCommon +
    geom_point(commonData_2) +
    geom_line(commonData_2) +
    labs(title = "340/380 as % Dynamic Ratio") + 
    annotate("text", x=addTimes, y=max(calDF$X340.380.Dyn.values), label = conds, size=10) +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = "steelblue4", alpha = .2)
  ggsave(paste("340\\380 As %% Dynamic Ratio.pdf"), device = "pdf", width = 16, height = 9, units = "in")
}
