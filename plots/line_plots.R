library("gdata")

#dual_plot <- function(excelFile, kernelStr, pdfFile) {
#  solve_data <- read.xls(excelFile)
# N <- solve_data[1,"N"] 
#
# pdf(file=pdfFile)
#
#  x <- solve_data[,"Num.Threads"]
#  xlabel <- "Number of Threads"
#  ylabel1 <- "Run Time (sec)"
#  knlTime <- na.omit(cbind(x, solve_data[,"KNL.Run.Time"]))
#  minTime <- min(knlTime[,2])
#  maxTime <- max(knlTime[,2])
#  snbTime <- na.omit(cbind(x, solve_data[,"SNB.Run.Time"]))
#  minTime <- min(minTime, snbTime[,2])
#  maxTime <- max(maxTime, snbTime[,2])
#  ylimits1 <- c(minTime, maxTime)
#
#  par(mar=c(5,4,4,4)+0.1)
#
#  plot(x=knlTime[,1], y=knlTime[,2], type="o", pch=16, lty=1, ylim=ylimits1, col="black", xlab="", ylab="", las=1, cex.axis=0.85)
#  lines(x=snbTime[,1], y=snbTime[,2], type="o", pch=17, lty=1, col="black")
#  mtext(xlabel, side=1, line=2.25, cex=0.85)
#  mtext(ylabel1, side=2, line=2.75, cex=0.85)
#  box()
#
#  ylabel2 <- "Strong Scaling"
#  knlScaling <- na.omit(cbind(x, solve_data[,"KNL.Scaling"]))
#  minScaling <- min(knlScaling[,2])
#  maxScaling <- max(knlScaling[,2])
#  snbScaling <- na.omit(cbind(x, solve_data[,"SNB.Scaling"]))
#  minScaling <- min(minScaling, snbScaling[,2])
#  maxScaling <- max(maxScaling, snbScaling[,2])
#  ylimits2 <- c(minScaling, maxScaling)
#
#  par(new=TRUE)
#  plot(knlScaling[,1], knlScaling[,2], type="o", pch=1, lty=2, ylim=ylimits2, col="black", axes=FALSE, xlab="", ylab="")
#  mtext(ylabel2, side=4 , line=2.0, cex=0.85)
#  axis(4, las=1, cex.axis=0.85)
#  lines(x=snbScaling[,1], y=snbScaling[,2], type="o", pch=2, lty=2, col="black")
#  legend("topright", legend=c("KNL Time", "SNB Time", "KNL Scaling", "SNB Scaling"), pch=c(16,17,1,2), lty=c(1,1,2,2))
#
#  titleFmt <- sprintf("%s, N = %%d", kernelStr)
#  titleStr <- sprintf(titleFmt, N)
#  title(main=titleStr, col.main="black", font.main=1, line=0.5, cex.main=0.85) 
#
#  dev.off()
#}


timings_plot <- function(excelFile, kernelStr, pdfFile) {
  solve_data <- read.xls(excelFile)
  N <- solve_data[1,"N"] 

  pdf(file=pdfFile)

  x <- solve_data[,"Num.Threads"]
  xlabel <- "Number of Threads"
  ylabel1 <- "Run Time (sec)"
  knlTime <- na.omit(cbind(x, solve_data[,"KNL.Run.Time"]))
  minTime <- min(knlTime[,2])
  maxTime <- max(knlTime[,2])
  snbTime <- na.omit(cbind(x, solve_data[,"SNB.Run.Time"]))
  minTime <- min(minTime, snbTime[,2])
  maxTime <- max(maxTime, snbTime[,2])
  ylimits1 <- c(minTime, maxTime)

  plot(x=knlTime[,1], y=knlTime[,2], type="o", pch=16, lty=1, ylim=ylimits1, col="black", xlab="", ylab="", las=1, cex.axis=0.85)
  lines(x=snbTime[,1], y=snbTime[,2], type="o", pch=17, lty=1, col="black")
  mtext(xlabel, side=1, line=2.25, cex=0.85)
  mtext(ylabel1, side=2, line=2.75, cex=0.85)
  legend("topright", legend=c("KNL Time", "SNB Time"), pch=c(16,17), lty=c(1,1))
  box()

  titleFmt <- sprintf("%s, N = %%d", kernelStr)
  titleStr <- sprintf(titleFmt, N)
  title(main=titleStr, col.main="black", font.main=1, line=0.5, cex.main=0.85) 

  dev.off()
}


# Read data intended to be displayed on the same plot
read_sheet_data <- function(excelFile, numSheets, maxNumThreads) {
   knlTimes <- list()
   snbTimes <- list()
   knlScalings <- list()
   snbScalings <- list()
   
   minTime <- Inf
   maxTime <- 0
   minScaling <- Inf
   maxScaling <- 0
   
   for (i in 1:numSheets) {
      results <- read.xls(sheet=i, excelFile)
      x <- results[,"Num.Threads"]

      # Process times
      knlTime <- na.omit(cbind(x, results[,"KNL.Run.Time"]))
      knlTimes[[i]] <- knlTime[knlTime[,1] <= maxNumThreads,]
      minTime <- min(knlTimes[[i]][,2], minTime)
      maxTime <- max(knlTimes[[i]][,2], maxTime)

      snbTime <- na.omit(cbind(x, results[,"SNB.Run.Time"]))
      snbTimes[[i]] <- snbTime[snbTime[,1] <= maxNumThreads,]
      minTime <- min(snbTimes[[i]][,2], minTime)
      maxTime <- max(snbTimes[[i]][,2], maxTime)
      
      # Process scaling values
      knlScaling <- na.omit(cbind(x, results[,"KNL.Scaling"]))
      knlScalings[[i]] <- knlScaling[knlScaling[,1] <= maxNumThreads,]
      minScaling <- min(knlScalings[[i]][,2], minTime)
      maxScaling <- max(knlScalings[[i]][,2], maxTime)

      snbScaling <- na.omit(cbind(x, results[,"SNB.Run.Time"]))
      snbScalings[[i]] <- snbScaling[snbScaling[,1] <= maxNumThreads,]
      minScaling <- min(snbScalings[[i]][,2], minTime)
      maxScaling <- max(snbScalings[[i]][,2], maxTime)
   }

   timeLimits <- c(minTime, maxTime)
   scalingLimits <- c(minScaling, maxScaling)
      
   returnValues <- list("knlTimes"=knlTimes, "snbTimes"=snbTimes, "knlScalings"=knlScalings, "snbScalings"=snbScalings, "timeLimits"=timeLimits, "scalingLimits"=scalingLimits)
}
             

# Plots results from multiple kernels on the same line chart
timings_plot_multi <- function(excelFile, numSheets, maxNumThreads, kernelNames, titleStr, pdfFile) {
  fontScaleFactor <- 0.85

  pdf(file=pdfFile)

  results <- read_sheet_data(excelFile, numSheets, maxNumThreads)
  knlTimes <- results$knlTimes
  snbTimes <- results$snbTimes
  timeLimits <- results$timeLimits
  scalingLimits <- results$scalingLimits 

  knlPch <- c(16, 17, 18, 15)
  snbPch <- c(1, 2, 5, 0)
  knlLty <- c(1, 1, 1, 1)
  snbLty <- c(2, 2, 2, 2)
  legendPch <- c(16, 1, 17, 2, 18, 5, 15, 0)
  legendLty <- c(1, 2, 1, 2, 1, 2, 1, 2)
  xlabel <- "Number of Threads"
  ylabel <- "Run Time (sec)"
  legendEntries <- list()

  # Plot times
  plot(knlTimes[[1]], type="o", pch=knlPch[1], lty=1, ylim=timeLimits, col="black", xlab="", ylab="", las=1, cex.axis=fontScaleFactor)
  legendEntries[[1]] <- sprintf("KNL time - %s", kernelNames[1]) 
  lines(snbTimes[[1]], type="o", pch=snbPch[1], lty=2, col="black")
  legendEntries[[2]] <- sprintf("SNB time - %s", kernelNames[1]) 
  mtext(xlabel, side=1, line=2.25, cex=fontScaleFactor)
  mtext(ylabel, side=2, line=2.75, cex=fontScaleFactor)

  for (i in 2:length(numSheets)) {
    lines(knlTimes[[i]], type="o", pch=knlPch[i], lty=knlLty[i], col="black")
    legendEntries[[2*(i-1)+1]] <- sprintf("KNL time - %s", kernelNames[i]) 
    lines(snbTimes[[i]], type="o", pch=snbPch[i], lty=snbLty[i], col="black")
    legendEntries[[2*(i-1)+2]] <- sprintf("SNB time - %s", kernelNames[i]) 
  }

  print(legendEntries)
  legend("topright", legend=legendEntries, pch=legendPch, lty=legendLty)
  #box()

  title(main=titleStr, col.main="black", font.main=1, line=0.5, cex.main=fontScaleFactor) 

  dev.off()
}


plot_example <- function() {
   excelFile <- "matrix.xlsx"
   numSheets <- 2
   maxNumThreads <- 68
   kernelNames <- c("matrix cross product", "mat. mat. mult.")
   titleStr <- "Matrix cross product and matrix-matrix multiplication"
   titleStr <- "Matrix cross product and matrix-matrix multiplication (N=20000)"
   pdfFile <- "./matrix.pdf"

   timings_plot_multi(excelFile, numSheets, maxNumThreads, kernelNames, titleStr, pdfFile)   
} 


scaling_plot <- function(excelFile, kernelStr, pdfFile) {
  solve_data <- read.xls(excelFile)
  N <- solve_data[1,"N"] 

  pdf(file=pdfFile)

  x <- solve_data[,"Num.Threads"]
  xlabel <- "Number of Threads"

  ylabel1 <- "Strong Scaling"
  knlScaling <- na.omit(cbind(x, solve_data[,"KNL.Scaling"]))
  minScaling <- min(knlScaling[,2])
  maxScaling <- max(knlScaling[,2])
  snbScaling <- na.omit(cbind(x, solve_data[,"SNB.Scaling"]))
  minScaling <- min(minScaling, snbScaling[,2])
  maxScaling <- max(maxScaling, snbScaling[,2])
  ylimits1 <- c(minScaling, maxScaling)

  plot(x=knlScaling[,1], y=knlScaling[,2], type="o", pch=1, lty=2, ylim=ylimits1, col="black", xlab="", ylab="", las=1, cex.axis=0.85)
  mtext(xlabel, side=1, line=2.25, cex=0.85)
  mtext(ylabel1, side=2, line=2.5, cex=0.85)
  lines(x=snbScaling[,1], y=snbScaling[,2], type="o", pch=2, lty=2, col="black")
  legend("topright", legend=c("KNL Scaling", "SNB Scaling"), pch=c(1,2), lty=c(2,2))

  titleFmt <- sprintf("%s, N = %%d", kernelStr)
  titleStr <- sprintf(titleFmt, N)
  title(main=titleStr, col.main="black", font.main=1, line=0.5, cex.main=0.85) 

  dev.off()
}

