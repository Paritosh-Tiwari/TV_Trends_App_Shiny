library(XML)
library(stringr)
library(plyr)
library(pbapply)
library(RCurl)
library(relenium)


#############################################################################################
base_url = "http://www.tv.com"
home_url = "http://www.tv.com/shows/"

home_page = htmlTreeParse(home_url,useInternal=TRUE)

categories = xpathSApply(home_page,"//*[@id='the_hub_page']/div/div/div/div[1]/a",xmlValue)
categories = categories[2:length(categories)]

categories_url = xpathSApply(home_page,"//*[@id='the_hub_page']/div/div/div/div[1]/a/@href")
categories_url = categories_url[2:length(categories_url)]
categories_url = as.character(categories_url)
categories_url = paste(base_url,categories_url,sep="")

show_list = data.frame()
if(file.exists("show_list.csv")) {show_list = read.csv("show_list.csv",header=TRUE)}
#show_list = shows_url(category_url,show_list)
# show_list = pblapply(categories_url[1:2],function(x) {shows_url(x,show_list)})

## 1594 pages
#show_list = shows_url(home_url,show_list)

for(i in l)
        {page_url = paste(home_url,"page",i,"/",sep="")
         show_list = shows_url(page_url,show_list)
        
        }

##################################################################################
show_list = read.csv("show_list.csv",header=TRUE)
show_data = data.frame()
show_url = as.character(show_list[1432:dim(show_list)[1],2])
show_data = ldply(show_url, function(x) {show_data_extract(x,show_data)})
write.csv(show_data,"data.csv",row.names=FALSE)
show_data = data.frame()

##################################################################################
show_list = read.csv("show_list.csv",header=TRUE)
show_data = read.csv("Data.csv",header=TRUE)
show_data[,1] = as.character(show_data[,1])
show_data[,2] = as.character(show_data[,2])
show_url = as.character(show_list[,2])
n = 17470
show_url = show_url[n:dim(show_list)[1]]

for(i in show_url)
        {print(i)
         show_page = htmlTreeParse(i,useInternal=TRUE)
         categories = xpathSApply(show_page,"//div[@class='m categories _standard_sub_module']/p/a",xmlValue)
         
         show_name = xpathSApply(show_page,"//div[@class='m show_head']/h1",xmlValue)
         user_rating = xpathSApply(show_page,"//div[@itemprop='ratingValue']",xmlValue)
         user_votes = xpathSApply(show_page,"//div[@class='votes']/span",xmlValue)
         user_votes = gsub(",","",user_votes)
         user_votes = as.numeric(user_votes)
         r1 = xpathSApply(show_page,"//*[@id='mantle_skin']/div[3]/div/div/div[1]/div[2]",xmlValue)
         r2 = xpathSApply(show_page,"//div[@class='tagline']/span/span",xmlValue)
         r3 = xpathSApply(show_page,"//div[@class='tagline']/span/span/span",xmlValue)
         if(is.null(r2))
         {year_end = unlist(str_split(r1," "))
          year_end = year_end[length(year_end)]
          year_end = gsub(")","",year_end)
          last_season = xpathSApply(show_page,"//td[@class='nums']/a",xmlValue)
          last_season = last_season[1]
          last_season = str_trim(last_season)
          last_season = unlist(str_split(last_season," "))
          v_options = grep("[0-9]",last_season)
          last_season = last_season[v_options[1]]
          last_season = unlist(str_split(last_season,":"))
          v_options = grep("[0-9]",last_season)
          last_season = last_season[v_options[1]]
          last_season = str_trim(last_season)
          Year_Started = as.numeric(year_end) - as.numeric(last_season)
         }
         if(!is.null(r2))
         {Year_Started = gsub(r3,"",r2)
          Year_Started = str_trim(Year_Started)
          Year_Started = unlist(str_split(Year_Started," "))
          Year_Started = Year_Started[length(Year_Started)]  
         }
         
         if(length(show_name)==0) show_name = "NA"
         if(length(user_rating)==0) user_rating = "NA"
         if(length(user_votes)==0) user_votes = "NA"
         if(length(Year_Started)==0) Year_Started = "NA"
         if(length(categories)==0) categories = "NA"
         
         for(category in categories)
                {show_data[dim(show_data)[1]+1,1] = category
                 show_data[dim(show_data)[1],2] = show_name
                 show_data[dim(show_data)[1],3] = user_rating
                 show_data[dim(show_data)[1],4] = user_votes
                 show_data[dim(show_data)[1],5] = Year_Started
                 show_data[dim(show_data)[1],6] = i
                }
         
         names(show_data) = c("Category","Name","User_Rating","User_Votes","Year_Started","Show_url")
         write.csv(show_data,"Data.csv",row.names=FALSE)
         print (n)
         n = n+1
         Sys.sleep(abs(rnorm(2,0.5)))
        }