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
read_sheet_data <- function(excelFile, sheetIndices, maxNumThreads) {
   knlTimes <- list()
   snbTimes <- list()
   knlScalings <- list()
   snbScalings <- list()

   numSheets <- length(sheetIndices)
   
   #minTime <- 0
   minTime <- Inf
   maxTime <- 0
   minScaling <- 0
   #minScaling <- Inf
   maxScaling <- 0
   
   for (i in 1:numSheets) {
      results <- read.xls(sheet=sheetIndices[i], excelFile)
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
      minScaling <- min(knlScalings[[i]][,2], minScaling)
      maxScaling <- max(knlScalings[[i]][,2], maxScaling)

      snbScaling <- na.omit(cbind(x, results[,"SNB.Scaling"]))
      snbScalings[[i]] <- snbScaling[snbScaling[,1] <= maxNumThreads,]
      minScaling <- min(snbScalings[[i]][,2], minScaling)
      maxScaling <- max(snbScalings[[i]][,2], maxScaling)
   }

   timeLimits <- c(minTime, maxTime)
   scalingLimits <- c(minScaling, maxScaling)
      
   returnValues <- list("knlTimes"=knlTimes, "snbTimes"=snbTimes, "knlScalings"=knlScalings, "snbScalings"=snbScalings, "timeLimits"=timeLimits, "scalingLimits"=scalingLimits)
}
             

# Plots results from multiple kernels on the same line chart
plot_multi <- function(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, ssLegendPosition) {
  fontScaleFactor <- 0.90
  pointScaleFactor <- 1.15

  pdfTimesFile <- paste(pdfBaseFileName, "-rt", ".pdf", sep="")
  pdfScalingFile <- paste(pdfBaseFileName, "-ss", ".pdf", sep="")
  knlTimes <- results$knlTimes
  snbTimes <- results$snbTimes
  knlScalings <- results$knlScalings
  snbScalings <- results$snbScalings
  timeLimits <- results$timeLimits
  scalingLimits <- results$scalingLimits 
  numKernels <- length(knlTimes) # Assume all node types have same  #kernels

  knlPch <- c(16, 17, 18, 15)
  snbPch <- c(1, 2, 5, 0)
  knlLty <- c(1, 1, 1, 1)
  snbLty <- c(2, 2, 2, 2)
  legendPch <- c(16, 1, 17, 2, 18, 5, 15, 0)
  legendLty <- c(1, 2, 1, 2, 1, 2, 1, 2)
  xTicks <- c(1, 2, 4, 8, 12, 16, 34, 50, 66, 68, 100, 136, 204, 250, 272)
  xlabel <- "Number of Threads"
  ylabelTime <- "Run Time (sec)"
  ylabelScaling <- "Strong Scaling"
  legendEntries <- list()

  # Plot times
  pdf(file=pdfTimesFile)

  tdiff <- timeLimits[2] - timeLimits[1]

  if (tdiff <= 300) {
     plot(knlTimes[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", yaxt="n", ylim=timeLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
  
     if (tdiff < 200) {
        byValue <- 5
     } else {
        byValue <- 10
     }

     tickMarks <- seq(floor(timeLimits[1]), ceiling(timeLimits[2]), by=byValue)
     axis(2, at=tickMarks, las=1, cex.axis=fontScaleFactor)
  } else {
     plot(knlTimes[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", ylim=timeLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
  }

  axis(1, at=xTicks)

  legendEntries[[1]] <- sprintf("KNL time - %s", kernelNames[1]) 
  lines(snbTimes[[1]], type="o", pch=snbPch[1], lty=2, col="black", cex=pointScaleFactor)
  legendEntries[[2]] <- sprintf("SNB time - %s", kernelNames[1]) 
  mtext(xlabel, side=1, line=2.25, cex=fontScaleFactor)
  mtext(ylabelTime, side=2, line=2.75, cex=fontScaleFactor)

  if (numKernels > 1) {
     for (i in 2:numKernels) {
       lines(knlTimes[[i]], type="o", pch=knlPch[i], lty=knlLty[i], col="black", cex=pointScaleFactor)
       legendEntries[[2*(i-1)+1]] <- sprintf("KNL time - %s", kernelNames[i]) 
       lines(snbTimes[[i]], type="o", pch=snbPch[i], lty=snbLty[i], col="black", cex=pointScaleFactor)
       legendEntries[[2*(i-1)+2]] <- sprintf("SNB time - %s", kernelNames[i]) 
     }
  }

  legend("topright", legend=legendEntries, pch=legendPch, lty=legendLty)
  title(main=titleStrTime, col.main="black", font.main=1, line=0.5, cex.main=fontScaleFactor) 

  dev.off()

  # Plot strong scaling
  legendEntries <- list()

  pdf(file=pdfScalingFile)

  if (scalingLimits[2] - scalingLimits[1] <= 50) {
     plot(knlScalings[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", yaxt="n", ylim=scalingLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
     tickMarks <- 1:ceiling(scalingLimits[2])
     axis(2, at=tickMarks, las=1, labels=FALSE)
     tickMarks <- seq(2, ceiling(scalingLimits[2]), 2)
     tickLabels <- as.character(tickMarks)
     axis(2, at=tickMarks, labels=tickLabels, las=1, cex.axis=fontScaleFactor)
  } else {
     plot(knlScalings[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", ylim=scalingLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
  }

  axis(1, at=xTicks, cex.axis=fontScaleFactor)

  legendEntries[[1]] <- sprintf("KNL scaling - %s", kernelNames[1]) 
  lines(snbScalings[[1]], type="o", pch=snbPch[1], lty=2, col="black", cex=pointScaleFactor)
  legendEntries[[2]] <- sprintf("SNB scaling - %s", kernelNames[1]) 
  mtext(xlabel, side=1, line=2.25, cex=fontScaleFactor)
  mtext(ylabelScaling, side=2, line=2.75, cex=fontScaleFactor)

  if (numKernels > 1) {
     for (i in 2:numKernels) {
        lines(knlScalings[[i]], type="o", pch=knlPch[i], lty=knlLty[i], col="black", cex=pointScaleFactor)
        legendEntries[[2*(i-1)+1]] <- sprintf("KNL scaling - %s", kernelNames[i]) 
        lines(snbScalings[[i]], type="o", pch=snbPch[i], lty=snbLty[i], col="black", cex=pointScaleFactor)
        legendEntries[[2*(i-1)+2]] <- sprintf("SNB scaling - %s", kernelNames[i]) 
     }
  }

  abline(0, 1, lty=6)
  legendEntries <- append(legendEntries, c("linear scaling"), 0)
  legendPch <- append(legendPch, NA_integer_, 0)
  legendLty <- append(legendLty, 6, 0)

  legend(ssLegendPosition, legend=legendEntries, pch=legendPch, lty=legendLty)
  title(main=titleStrScaling, col.main="black", font.main=1, line=0.5, cex.main=fontScaleFactor) 
  dev.off()

}


# Plots results from multiple kernels on the same line chart
plot_knl <- function(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, ssLegendPosition) {
  fontScaleFactor <- 0.90
  pointScaleFactor <- 1.15

  pdfTimesFile <- paste(pdfBaseFileName, "-rt", ".pdf", sep="")
  pdfScalingFile <- paste(pdfBaseFileName, "-ss", ".pdf", sep="")
  knlTimes <- results$knlTimes
  snbTimes <- results$snbTimes
  knlScalings <- results$knlScalings
  snbScalings <- results$snbScalings
  timeLimits <- results$timeLimits
  scalingLimits <- results$scalingLimits 
  numKernels <- length(knlTimes) # Assume all node types have same  #kernels

  knlPch <- c(16, 17, 18, 15)
  snbPch <- c(1, 2, 5, 0)
  knlLty <- c(1, 1, 1, 1)
  snbLty <- c(2, 2, 2, 2)
  legendPch <- knlPch
  legendLty <- knlLty
  #legendPch <- c(16, 1, 17, 2, 18, 5, 15, 0)
  #legendLty <- c(1, 2, 1, 2, 1, 2, 1, 2)
  xTicks <- c(1, 2, 4, 8, 12, 16, 34, 50, 66, 68, 100, 136, 204, 250, 272)
  xlabel <- "Number of Threads"
  ylabelTime <- "Run Time (sec)"
  ylabelScaling <- "Strong Scaling"
  legendEntries <- list()

  # Plot times
  pdf(file=pdfTimesFile)

  tdiff <- timeLimits[2] - timeLimits[1]

  if (tdiff <= 300) {
     plot(knlTimes[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", yaxt="n", ylim=timeLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)

     if (tdiff < 200) {
        byValue <- 5
     } else {
        byValue <- 10
     }

     tickMarks <- seq(floor(timeLimits[1]), ceiling(timeLimits[2]), by=byValue)
     axis(2, at=tickMarks, las=1, cex.axis=fontScaleFactor)
  } else {
     plot(knlTimes[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", ylim=timeLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
  }

  axis(1, at=xTicks, cex.axis=fontScaleFactor)

  legendEntries[[1]] <- sprintf("KNL time - %s", kernelNames[1]) 
#  lines(snbTimes[[1]], type="o", pch=snbPch[1], lty=2, col="black", cex=pointScaleFactor)
#  legendEntries[[2]] <- sprintf("SNB time - %s", kernelNames[1]) 
  mtext(xlabel, side=1, line=2.25, cex=fontScaleFactor)
  mtext(ylabelTime, side=2, line=2.75, cex=fontScaleFactor)

  if (numKernels > 1) {
     for (i in 2:numKernels) {
       lines(knlTimes[[i]], type="o", pch=knlPch[i], lty=knlLty[i], col="black", cex=pointScaleFactor)
       legendEntries[[i]] <- sprintf("KNL time - %s", kernelNames[i]) 
#       lines(snbTimes[[i]], type="o", pch=snbPch[i], lty=snbLty[i], col="black", cex=pointScaleFactor)
#       legendEntries[[2*(i-1)+2]] <- sprintf("SNB time - %s", kernelNames[i]) 
     }
  }

  legend("topright", legend=legendEntries, pch=legendPch, lty=legendLty)
  title(main=titleStrTime, col.main="black", font.main=1, line=0.5, cex.main=fontScaleFactor) 

  dev.off()

  # Plot strong scaling
  legendEntries <- list()

  pdf(file=pdfScalingFile)

  if (scalingLimits[2] - scalingLimits[1] <= 50) {
     plot(knlScalings[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", yaxt="n", ylim=scalingLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
     tickMarks <- 1:ceiling(scalingLimits[2])
     axis(2, at=tickMarks, las=1, labels=FALSE)
     tickMarks <- seq(2, ceiling(scalingLimits[2]), 2)
     tickLabels <- as.character(tickMarks)
     axis(2, at=tickMarks, labels=tickLabels, las=1, cex.axis=fontScaleFactor)
  } else {
     plot(knlScalings[[1]], type="o", pch=knlPch[1], lty=1, xaxt="n", ylim=scalingLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
  }

  axis(1, at=xTicks, cex.axis=fontScaleFactor)

  legendEntries[[1]] <- sprintf("KNL scaling - %s", kernelNames[1]) 
  #lines(snbScalings[[1]], type="o", pch=snbPch[1], lty=2, col="black", cex=pointScaleFactor)
  #legendEntries[[2]] <- sprintf("SNB scaling - %s", kernelNames[1]) 
  mtext(xlabel, side=1, line=2.25, cex=fontScaleFactor)
  mtext(ylabelScaling, side=2, line=2.75, cex=fontScaleFactor)

  if (numKernels > 1) {
     for (i in 2:numKernels) {
        lines(knlScalings[[i]], type="o", pch=knlPch[i], lty=knlLty[i], col="black", cex=pointScaleFactor)
        legendEntries[[i]] <- sprintf("KNL scaling - %s", kernelNames[i]) 
#        lines(snbScalings[[i]], type="o", pch=snbPch[i], lty=snbLty[i], col="black", cex=pointScaleFactor)
#        legendEntries[[2*(i-1)+2]] <- sprintf("SNB scaling - %s", kernelNames[i]) 
     }
  }

  abline(0, 1, lty=6)
  legendEntries <- append(legendEntries, c("linear scaling"), 0)
  legendPch <- append(legendPch, NA_integer_, 0)
  legendLty <- append(legendLty, 6, 0)

  legend(ssLegendPosition, legend=legendEntries, pch=legendPch, lty=legendLty)
  title(main=titleStrScaling, col.main="black", font.main=1, line=0.5, cex.main=fontScaleFactor) 
  dev.off()

}


# Plots results from multiple kernels on the same line chart
plot_snb <- function(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, ssLegendPosition) {
  fontScaleFactor <- 0.90
  pointScaleFactor <- 1.15

  pdfTimesFile <- paste(pdfBaseFileName, "-rt", ".pdf", sep="")
  pdfScalingFile <- paste(pdfBaseFileName, "-ss", ".pdf", sep="")
  knlTimes <- results$knlTimes
  snbTimes <- results$snbTimes
  knlScalings <- results$knlScalings
  snbScalings <- results$snbScalings
  timeLimits <- results$timeLimits
  scalingLimits <- results$scalingLimits 
  numKernels <- length(knlTimes) # Assume all node types have same  #kernels
  print(numKernels)

  knlPch <- c(16, 17, 18, 15)
  snbPch <- c(1, 2, 5, 0)
  knlLty <- c(1, 1, 1, 1)
  snbLty <- c(2, 2, 2, 2)
  legendPch <- snbPch
  legendLty <- snbLty
  #legendPch <- c(16, 1, 17, 2, 18, 5, 15, 0)
  #legendLty <- c(1, 2, 1, 2, 1, 2, 1, 2)
  xTicks <- c(1, 2, 4, 8, 12, 16, 34, 50, 66, 68, 100, 136, 204, 250, 272)
  xlabel <- "Number of Threads"
  ylabelTime <- "Run Time (sec)"
  ylabelScaling <- "Strong Scaling"
  legendEntries <- list()

  # Plot times
  pdf(file=pdfTimesFile)

  tdiff <- timeLimits[2] - timeLimits[1]

  if (tdiff <= 300) {
     plot(snbTimes[[1]], type="o", pch=snbPch[1], lty=2, xaxt="n", yaxt="n", ylim=timeLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)

     if (tdiff < 200) {
        byValue <- 5
     } else {
        byValue <- 10
     }

     tickMarks <- seq(floor(timeLimits[1]), ceiling(timeLimits[2]), by=byValue)
     axis(2, at=tickMarks, las=1, cex.axis=fontScaleFactor)
  } else {
     plot(snbTimes[[1]], type="o", pch=snbPch[1], lty=2, xaxt="n", ylim=timeLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
  }

  axis(1, at=xTicks, cex.axis=fontScaleFactor)

  legendEntries[[1]] <- sprintf("SNB time - %s", kernelNames[1]) 
#  lines(snbTimes[[1]], type="o", pch=snbPch[1], lty=2, col="black", cex=pointScaleFactor)
#  legendEntries[[2]] <- sprintf("SNB time - %s", kernelNames[1]) 
  mtext(xlabel, side=1, line=2.25, cex=fontScaleFactor)
  mtext(ylabelTime, side=2, line=2.75, cex=fontScaleFactor)

  if (numKernels > 1) {
     for (i in 2:numKernels) {
       legendEntries[[i]] <- sprintf("SNB time - %s", kernelNames[i]) 
       lines(snbTimes[[i]], type="o", pch=snbPch[i], lty=snbLty[i], col="black", cex=pointScaleFactor)
#       legendEntries[[i]] <- sprintf("SNB time - %s", kernelNames[i]) 
     }
  }

  legend("topright", legend=legendEntries, pch=legendPch, lty=legendLty)
  title(main=titleStrTime, col.main="black", font.main=1, line=0.5, cex.main=fontScaleFactor) 

  dev.off()

  # Plot strong scaling
  legendEntries <- list()

  pdf(file=pdfScalingFile)

  if (scalingLimits[2] - scalingLimits[1] <= 50) {
     plot(snbScalings[[1]], type="o", pch=snbPch[1], lty=1, xaxt="n", yaxt="n", ylim=scalingLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
     tickMarks <- 1:ceiling(scalingLimits[2])
     axis(2, at=tickMarks, las=1, labels=FALSE)
     tickMarks <- seq(2, ceiling(scalingLimits[2]), 2)
     tickLabels <- as.character(tickMarks)
     axis(2, at=tickMarks, labels=tickLabels, las=1, cex.axis=fontScaleFactor)
  } else {
     plot(snbScalings[[1]], type="o", pch=snbPch[1], lty=1, xaxt="n", ylim=scalingLimits, col="black", xlab="", ylab="", las=1, cex=pointScaleFactor, cex.axis=fontScaleFactor)
  }

  axis(1, at=xTicks, cex.axis=fontScaleFactor)

  legendEntries[[1]] <- sprintf("SNB scaling - %s", kernelNames[1]) 
  #lines(snbScalings[[1]], type="o", pch=snbPch[1], lty=2, col="black", cex=pointScaleFactor)
  #legendEntries[[2]] <- sprintf("SNB scaling - %s", kernelNames[1]) 
  mtext(xlabel, side=1, line=2.25, cex=fontScaleFactor)
  mtext(ylabelScaling, side=2, line=2.75, cex=fontScaleFactor)

  if (numKernels > 1) {
     for (i in 2:numKernels) {
        #lines(knlScalings[[i]], type="o", pch=knlPch[i], lty=knlLty[i], col="black", cex=pointScaleFactor)
        #legendEntries[[i]] <- sprintf("KNL scaling - %s", kernelNames[i]) 
        lines(snbScalings[[i]], type="o", pch=snbPch[i], lty=snbLty[i], col="black", cex=pointScaleFactor)
        legendEntries[[i]] <- sprintf("SNB scaling - %s", kernelNames[i]) 
     }
  }

  abline(0, 1, lty=6)
  legendEntries <- append(legendEntries, c("linear scaling"), 0)
  legendPch <- append(legendPch, NA_integer_, 0)
  legendLty <- append(legendLty, 6, 0)

  legend(ssLegendPosition, legend=legendEntries, pch=legendPch, lty=legendLty)
  title(main=titleStrScaling, col.main="black", font.main=1, line=0.5, cex.main=fontScaleFactor) 
  dev.off()

}


plot_chol_solve <- function(N, maxNumThreads) {
   excelFile <- paste("./chol_solve_", as.character(N), ".xlsx", sep="")
   sheetIndices <- c(1, 2)
   maxNumThreads <- 68

   kernelNames <- c("Cholesky fact.", "linear solve")
   titleStrTime <- paste("Run time of Cholesky factorization and linear solve (N=", as.character(N), ")", sep="")
   titleStrScaling <- paste("Strong scaling of Cholesky factorization and linear solve (N=", N, ")", sep="")
   pdfBaseFileName <- paste("chol_solve_", as.character(N), "_", as.character(maxNumThreads), sep="")
   results <- read_sheet_data(excelFile, sheetIndices, maxNumThreads)
   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, "bottomright")
   plot_knl(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_knl", sep=""), "bottomright")
   plot_snb(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_snb", sep=""), "bottomright")
}


#plot_cross_matmat <- function() {
#   excelFile <- "./cross_matmat_20000.xlsx"
#   sheetIndices <- c(1, 2)
#   maxNumThreads <- 272
#
#   kernelNames <- c("matrix cross prod.", "matrix-matrix mult.")
#   titleStrTime <- "Run time of matrix cross product and matrix-matrix mult. (N=20000)"
#   titleStrScaling <- "Strong scaling of matrix cross product and matrix-matrix mult. (N=20000)"
#   pdfBaseFileName <- "cross_matmat_20000_272"
#   results <- read_sheet_data(excelFile, sheetIndices, maxNumThreads)
#   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, "topright")
##   plot_knl(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_knl", sep=""), "topright")
#
#}


plot_eigen_matmat <- function(N, maxNumThreads) {
   excelFile <- paste("./eigen_matmat_", as.character(N), ".xlsx", sep="")
   sheetIndices <- c(1, 2)

   kernelNames <- c("eigendecomposition.", "matrix-matrix mult.")
   titleStrTime <- paste("Run time of eigendecomposition and matrix-matrix mult. (N=", as.character(N), ")", sep="")
   titleStrScaling <- paste("Strong scaling of eigendecomposition and matrix-matrix mult. (N=", as.character(N), ")", sep="")
   pdfBaseFileName <- paste("eigen_matmat_", as.character(N), "_", as.character(maxNumThreads), sep="")
   results <- read_sheet_data(excelFile, sheetIndices, maxNumThreads)
   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, "topright")
   plot_knl(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_knl", sep=""), "topright")
   plot_snb(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_snb", sep=""), "topright")

}


plot_matmat <- function(N, maxNumThreads) {
   excelFile <- paste("./eigen_matmat_", as.character(N), ".xlsx", sep="")
   sheetIndices <- c(2)

   kernelNames <- c("matrix-matrix mult.")
   titleStrTime <- paste("Run time of matrix-matrix mult. (N=", as.character(N), ")", sep="")
   titleStrScaling <- paste("Strong scaling of matrix-matrix mult. (N=", as.character(N), ")", sep="")
   pdfBaseFileName <- paste("matmat_", as.character(N), "_", as.character(maxNumThreads), sep="")
   results <- read_sheet_data(excelFile, sheetIndices, maxNumThreads)
   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, "bottomright")
   plot_knl(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_knl", sep=""), "bottomright")
   plot_snb(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_snb", sep=""), "bottomright")

}


plot_linsolve <- function(N, maxNumThreads) {
   excelFile <- paste("./chol_solve_", as.character(N), ".xlsx", sep="")
   sheetIndices <- c(2)

   kernelNames <- c("linear solve")
   titleStrTime <- paste("Run time of linear solve (N=", as.character(N), ")", sep="")
   titleStrScaling <- paste("Strong scaling of linear solve (N=", as.character(N), ")", sep="")
   pdfBaseFileName <- paste("linsolve_", as.character(N), "_", as.character(maxNumThreads), sep="")
   results <- read_sheet_data(excelFile, sheetIndices, maxNumThreads)
   print(results)
   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, "bottomright")
   plot_knl(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_knl", sep=""), "bottomright")
   plot_snb(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_snb", sep=""), "bottomright")

}


plot_eigen <- function(N, maxNumThreads) {
   excelFile <- paste("./eigen_", as.character(N), ".xlsx", sep="")
   sheetIndices <- c(1)

   kernelNames <- c("eigendecomposition")
   titleStrTime <- paste("Run time of eigendecomposition (N=", as.character(N), ")", sep="")
   titleStrScaling <- paste("Strong scaling of eigendecomposition (N=", as.character(N), ")", sep="")
   pdfBaseFileName <- paste("eigen_", as.character(N), "_", as.character(maxNumThreads), sep="")
   results <- read_sheet_data(excelFile, sheetIndices, maxNumThreads)
   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName, "topright")
   plot_knl(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_knl", sep=""), "bottomright")
   plot_snb(results, kernelNames, titleStrTime, titleStrScaling, paste(pdfBaseFileName, "_snb", sep=""), "bottomright")

}


#plot_eigen_qr <- function() {
#   excelFile <- "./eigen_qr_20000.xlsx"
#   numSheets <- 2
#   maxNumThreads <- 68
#
#   kernelNames <- c("eigendecomp.", "QR decomp.")
#   titleStrTime <- "Run time of eigendecomposition and QR decomposition (N=20000)"
#   titleStrScaling <- "Strong scaling of eigendecomposition and QR decomposition (N=20000)"
#   pdfBaseFileName <- "eigen_qr_20000_68"
#   results <- read_sheet_data(excelFile, numSheets, maxNumThreads)
#   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName)
#}


#plot_qr_svd <- function() {
#   excelFile <- "./qr_svd_20000.xlsx"
#   numSheets <- 2
#   maxNumThreads <- 68
#
#   kernelNames <- c("QR decomp.", "SVD")
#   titleStrTime <- "Run time of QR decomposition and SVD (N=20000)"
#   titleStrScaling <- "Strong scaling of QR decomposition and SVD (N=20000)"
#   pdfBaseFileName <- "qr_svd_20000_68"
#   results <- read_sheet_data(excelFile, numSheets, maxNumThreads)
#   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName)
#}


#plot_matvec <- function() {
#   excelFile <- "./matvec_40000.xlsx"
#   numSheets <- 1
#   maxNumThreads <- 68
#
#   kernelNames <- c("matrix-vector mult.")
#   titleStrTime <- "Run time of matrix-vector multiplication (N=40000)"
#   titleStrScaling <- "Strong scaling of matrix-vector multiplication (N=40000)"
#   pdfBaseFileName <- "matvec_40000_68"
#   results <- read_sheet_data(excelFile, numSheets, maxNumThreads)
#   plot_multi(results, kernelNames, titleStrTime, titleStrScaling, pdfBaseFileName)
#}

