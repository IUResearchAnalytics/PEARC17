library("gdata")

dual_plot <- function(excelFile, kernelStr, pdfFile) {
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

  par(mar=c(5,4,4,4)+0.1)

  plot(x=knlTime[,1], y=knlTime[,2], type="o", pch=16, lty=1, ylim=ylimits1, col="black", xlab="", ylab="", las=1, cex.axis=0.85)
  lines(x=snbTime[,1], y=snbTime[,2], type="o", pch=17, lty=1, col="black")
  mtext(xlabel, side=1, line=2.25, cex=0.85)
  mtext(ylabel1, side=2, line=2.75, cex=0.85)
  box()

  ylabel2 <- "Strong Scaling"
  knlScaling <- na.omit(cbind(x, solve_data[,"KNL.Scaling"]))
  minScaling <- min(knlScaling[,2])
  maxScaling <- max(knlScaling[,2])
  snbScaling <- na.omit(cbind(x, solve_data[,"SNB.Scaling"]))
  minScaling <- min(minScaling, snbScaling[,2])
  maxScaling <- max(maxScaling, snbScaling[,2])
  ylimits2 <- c(minScaling, maxScaling)

  par(new=TRUE)
  plot(knlScaling[,1], knlScaling[,2], type="o", pch=1, lty=2, ylim=ylimits2, col="black", axes=FALSE, xlab="", ylab="")
  mtext(ylabel2, side=4 , line=2.0, cex=0.85)
  axis(4, las=1, cex.axis=0.85)
  lines(x=snbScaling[,1], y=snbScaling[,2], type="o", pch=2, lty=2, col="black")
  legend("topright", legend=c("KNL Time", "SNB Time", "KNL Scaling", "SNB Scaling"), pch=c(16,17,1,2), lty=c(1,1,2,2))

  titleFmt <- sprintf("%s, N = %%d", kernelStr)
  titleStr <- sprintf(titleFmt, N)
  title(main=titleStr, col.main="black", font.main=1, line=0.5, cex.main=0.85) 

  dev.off()
}


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

