library('ggplot2')
library('gdata')
setwd("/Users/scamicha/ResearchGit/PaperWriting/PEARC17/plots")
infile <- "./matrix.xlsx"
scaledata <- read.xls(sheet=3,infile)
#rundata <- read.xls(sheet=2, infile)
#sizedata <- read.xls(sheet=3, infile)
#langdata <- read.xls(sheet=4, infile)

#lsdata$CmF <- lsdata$Cached - lsdata$Flat
#lsdata$CmFP <- (lsdata$Cached - lsdata$Flat)/lsdata$Cached

# ggplot(data=scaledata, aes(x=threads, y=CmFP))+geom_point()+ coord_cartesian(xlim = c(0, 70))+
#   labs(list(title="R Matrix Cross Product Scaling", x="Thread Count", y="Percentage Difference in Runtime")) +
#   theme(plot.title = element_text(hjust = 0.5))

ggplot(data=scaledata, aes(x=threads, y=scaling, group=interaction(processor,test), color=processor, linetype=test))+
  geom_line()+geom_point() + geom_abline(intercept = 0, slope = 1, linetype= 3) +
  coord_cartesian(xlim = c(0, 64)) + labs(list(title="R Matrix Cross Product Scaling", color="Architecture", linetype="Benchmark",
                                               x="Thread Count", y="Runtime in Seconds")) +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(labels=c("Knight's Landing","SandyBridge"))

ggplot(data=scaledata, aes(x=threads, y=time, group=interaction(processor,test), color=processor, linetype=test))+geom_line()+geom_point() +
  coord_cartesian(xlim = c(0, 64)) + labs(list(title="R Runtimes", color="Architecture", linetype="Benchmark",
                                                x="Thread Count", y="Scaled Performance")) +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(labels=c("Knight's Landing","SandyBridge"))

# ggplot(data=rundata, aes(x=threads, y=mxprun, group=processor, color=processor))+geom_line()+geom_point() +
#   coord_cartesian(xlim = c(0, 68)) + labs(list(title="R Matrix Cross Product Runtimes", color="Architecture",
#                                                 x="Thread Count", y="Runtime (sec)")) +
#   theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(labels=c("Knight's Landing","SandyBridge"))
# 
# ggplot(data=sizedata, aes(x=threads, y=eigensmall, group=processor, color=processor))+geom_line()+geom_point() +
#   coord_cartesian(xlim = c(0, 68)) + labs(list(title="R Eigenvalue Decomposition Runtimes n=4K", color="Architecture",
#                                                x="Thread Count", y="Runtime (sec)")) +
#   theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(labels=c("Knight's Landing","SandyBridge"))
# 
# ggplot(data=sizedata, aes(x=threads, y=eigenlarge, group=processor, color=processor))+geom_line()+geom_point() +
#   coord_cartesian(xlim = c(0, 68)) + labs(list(title="R Eigenvalue Decomposition Runtimes n=20K", color="Architecture",
#                                                x="Thread Count", y="Runtime (sec)")) +
#   theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(labels=c("Knight's Landing","SandyBridge"))
# 
# ggplot(data=langdata, aes(x=threads, y=runtime, group=lang, color=lang))+geom_line()+geom_point() +
#   coord_cartesian(xlim = c(0, 68)) + labs(list(title="C vs. R QR Decomposition Runtimes n=20K", color="Architecture",
#                                                x="Thread Count", y="Runtime (sec)")) +
#   theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(labels=c("C driver","R wrapper"))
# 

# mcpdata$KNLspeedup <- mcpdata$Runtime[1]/mcpdata$Runtime
# mcpdata$ivyspeedup <- mcpdata$Runtime[12]/mcpdata$Runtime
# mcpdata$speedup <- ifelse(grepl("KNL",mcpdata$Machine),mcpdata$KNLspeedup,mcpdata$ivyspeedup)
# ggplot(data=mcpdata, aes(x=Thread.Count, y=speedup, group=Machine, color=Machine))+geom_line()+geom_point() +
#   coord_cartesian(xlim = c(0, 70))+ labs(list(title="R Linear Solve Speedups", color="Architecture",
#                                               x="Thread Count", y="Runtime (sec)")) +
#   theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(labels=c("IvyBridge","Knight's Landing"))
