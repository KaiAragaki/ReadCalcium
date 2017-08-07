readCalcium <- function(dateArg, ...) {

  # Initiate Variables
  conds <- c(..., "Ionomycin")
  divergeStart <- divergeEnd <- c()
  
  # Sets baseline and standard cond time intervals
  interval <- c(0:4)
  condInterval <- c(1.5, 2, 3, 4, 6)
  addTimes <- c(5) # Once I change my protocol to match Craig's, I can likely get rid of this variable
  
  # Loops for each cond and appends the time - not including Ionomycin time, which has its own interval.
  for (i in 1:(length(conds) - 1)){
    addtlCond <- condInterval + last(interval)
    interval <- c(interval, addtlCond)
    addTimes <- c(addTimes, last(interval) + 1)
  }
  
  # Tacks on ionomycin time
  ionomycinInterval <- last(interval) + c(1.5, 2, 3, 4)
  condTimes <- c(interval, ionomycinInterval)
  
  # Reads and wrangles tab delimited file. If Lab folder position changes, this will break!
  path <- file.path("~", "Desktop", "Lab", "Experiments", "Calcium", "By Subject", "INS 832:3 Glucose Response", dateArg) %>%
    setwd()
  calFile <- read.delim(paste0(dateArg,"Results.xls"))
  calFile <- calFile[ ,-1]
  
  # Subtracts background from each cell
  minusBG <- mutate_all(calFile, function(x) {x - calFile[ ,1]})
  minusBG <- minusBG[ ,-1]
  
  # Slices and dices, with math
  calDF <- data.frame("condTimes" = condTimes, "380" = stack(minusBG[seq(2, nrow(minusBG), 2),]), "340" = stack(cal340 <- minusBG[seq(1, nrow(minusBG), 2),])) %>%
    mutate("340/380" = X340.values/X380.values) %>%
    group_by(X380.ind) %>%
    mutate("Baseline" = mean(`340/380`[1:5]), "maxIono" = max(tail(`340/380`)), "dynR" = (`340/380`-Baseline)/(maxIono-Baseline) * 100) %>%
    group_by(condTimes) %>%
    mutate("mean380" = mean(X380.values), "mean340" = mean(X340.values))
  
  # Loops through row means and checks to see if, on average, the slopes diverge.
  for (i in 1:(length(condTimes)-1)){
    if(calDF$mean380[i] - calDF$mean380[i+1] > 50 & calDF$mean340[i + 1] - calDF$mean340[i] > 50  | calDF$mean380[i+1] - calDF$mean380[i] > 50 & calDF$mean340[i] - calDF$mean340[i + 1] > 50){
      divergeStart <- c(divergeStart, condTimes[i])
      divergeEnd <- c(divergeEnd, condTimes[i+1])
    }
  }
  rects <- data.frame(xstart = divergeStart, xend = divergeEnd)
  
  # Template Plot
  calCommon <- list(theme(plot.title = element_text(hjust = 0.5), legend.position ="none", text = element_text(size = 20)), labs(x = "Time (min)", y = ""), geom_vline(xintercept = (addTimes), color = "red", alpha = 0.5), geom_point(), geom_line())
  
  # 380 Plot
  ggplot(data = calDF, aes(x=condTimes, y=X380.values, color = X340.ind)) + 
    calCommon +
    labs(title = "380") + 
    annotate("text", x=addTimes, y=max(calDF$X380.values), label = conds, size=10)
  ggsave("380.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340 Plot
  ggplot(data = calDF, aes(x=condTimes, y=X340.values, color = X340.ind)) +
    calCommon + 
    labs(title = "340") + 
    annotate("text", x=addTimes, y=max(calDF$X340.values), label = conds, size=10)
  ggsave("340.pdf", device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 Plot
  ggplot(data = calDF, aes(x=condTimes, y=`340/380`, color = X340.ind)) + 
    calCommon +
    labs(title = "340/380") + 
    annotate("text", x=addTimes, y=max(calDF$`340/380`), label = conds, size=10) +
    annotate("rect", xmin = rects$xstart, xmax = rects$xend, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "steelblue4")
  ggsave(paste("340\\380.pdf"), device = "pdf", width = 16, height = 9, units = "in")
  
  # 340/380 as % Dynamic Range Plot
  ggplot(data = calDF, aes(x=condTimes, y=dynR, color = X340.ind)) + 
    calCommon +
    labs(title = "340/380 as % Dynamic Ratio") + 
    annotate("text", x=addTimes, y=max(calDF$dynR), label = conds, size=10) +
    annotate("rect", xmin = rects$xstart, xmax = rects$xend, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "steelblue4")
  ggsave(paste("340\\380 As %% Dynamic Ratio.pdf"), device = "pdf", width = 16, height = 9, units = "in")
}
