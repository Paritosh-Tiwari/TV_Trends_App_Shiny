library(shiny)
library(stringr)
library(plyr)
library(pbapply)
library(reshape2)
library(rCharts)
library(XML)
library(shinyIncubator)

data = read.csv("TV_Show_Tidy_Data.csv",header=TRUE)
data[,1] = as.character(data[,1])
data[,2] = as.character(data[,2])
data[,3] = as.character(data[,3])
data[,7] = as.character(data[,7])
data[,9] = as.character(data[,9])
data$Count = 1

##############################################################################


datamelt = melt(data,id=c("Period","Category","Name","Year_Started","Show_url","Rating"),measure.vars=c("User_Rating","User_Votes","sq","Count"))
t3 = dcast(datamelt,Category+Period+Rating~variable,sum)

###############################################################################