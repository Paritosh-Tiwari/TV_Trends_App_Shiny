library(stringr)
library(plyr)
library(pbapply)
library(reshape2)
library(rCharts)

################################################################################

## Sourcing Data

show_data = read.csv("Data.csv",header=TRUE)
show_data[,1] = as.character(show_data[,1])
show_data[,2] = as.character(show_data[,2])
show_data[,6] = as.character(show_data[,6])
dim(show_data)

################################################################################

## Removing Missing Values

missing_values = pbapply(show_data,1,function(x){sum(is.na(x))})

show_data_n = show_data[which(missing_values==0),]
dim(show_data_n)

data_updated = show_data_n[show_data_n$Year_Started>1900,]
dim(data_updated)

################################################################################

## Assigning Period

data = data_updated
data[,3] = as.numeric(as.character((data[,3])))
data[,4] = as.numeric(as.character((data[,4])))
data[,5] = as.numeric(as.character((data[,5])))
table(data$Category)

Period = pbsapply(data[,5],function(x) {decade_tagging(x)})
data$Period = Period
dim(data)
##################################################################################

## Getting decade wise statistics

datamelt = melt(data,id=c("Category","Name","Year_Started","Period"),measure.vars=c("User_Rating","User_Votes"))
Decade_Statistics = dcast(datamelt,Period~variable,sum)


#       Period User_Rating User_Votes
# 1  1911-1920         0.0          0
# 2  1921-1930        55.4       5758
# 3  1931-1940         0.0          0
# 4  1941-1950       143.5       1484
# 5  1951-1960      1658.4      38509
# 6  1961-1970      1823.3      54565
# 7  1971-1980      2211.4      62805
# 8  1981-1990      3822.7     174575
# 9  1991-2000      9076.3     818898
# 10 2001-2010     32854.2    2422001
# 11 2011-2014      8622.8     185695


# Very less number of shows before 1950. So re-dividing the statistics to include
# 1911-1950 in one group

names(Decade_Statistics) = c("Period","Total_User_Ratings","Total_User_Votes")

###################################################################################

## Calculation Success Quotient

data_merge = merge(data,Decade_Statistics,by.x="Period",by.y="Period",all=TRUE)

head(data_merge)

data_merge$sq = (data_merge$User_Votes/data_merge$Total_User_Votes)*data_merge$User_Rating*100

q = quantile(data_merge$sq,probs=seq(0,1,0.20))

## Successful Show threshold

upper_t = q[5]

Rating = pbsapply(data_merge[,10],function(x) {show_rating(x,upper_t)})

data_merge$Rating = Rating

data_merge$count = 1

head(data_merge)

#####################################################################################
write.csv(data_merge[,c(1,2,3,4,5,6,7,10,11)],"TV_Show_Tidy_Data.csv",row.names=FALSE)


#####################################################################################
# datamelt_n = melt(data_merge,id=c("decade","Category","Name","Year_Started","decade","rating"),measure.vars=c("User_Rating","User_Votes","Total_User_Ratings","Total_User_Votes","sq","count"))
# 
# t1 = dcast(datamelt_n,decade+rating~variable,sum)
# t2 = dcast(datamelt_n,Category+rating~variable,sum)
# t3 = dcast(datamelt_n,Category+decade+rating~variable,sum)
# 
# 
# ######################################################################################
# 
# n1 <- nPlot(count ~ decade, group = "rating", data = t1, type = "multiBarChart")
# n2 <- nPlot(count ~ Category, group = "rating", data = t2[1:10,], type = "multiBarChart")
# n3 <- nPlot(count ~ decade, group = "rating", data = t3[t3$Category=="Comedy",], type = "multiBarChart")
# n3$chart(color = c('blue', 'green'))
# n3$chart(reduceXTicks = FALSE)
# n3$xAxis(staggerLabels = TRUE)
# n3
# 
# 
# n4 <- nPlot(count ~ Category, group = "rating", data = t4, type = "multiBarChart")
# n4$chart(reduceXTicks = FALSE)
# n4$xAxis(rotateLabels=-15)
# #n4$set(width = 500, height = 300)
# n4
# 
# ######################################################################################
# 
# 
