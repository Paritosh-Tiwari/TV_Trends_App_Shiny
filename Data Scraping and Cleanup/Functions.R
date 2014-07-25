shows_url = function(category_url,show_list)
{
        Sys.sleep(abs(rnorm(2,0.5)))
        list_page = htmlTreeParse(category_url,useInternal=TRUE)
        
        shows_names = xpathSApply(list_page,"//*[@id='the_hub_page']/div[2]/div[3]/ul/li/div/h4/a",xmlValue)
        
        if(!is.null(shows_names))
        {shows_url = xpathSApply(list_page,"//*[@id='the_hub_page']/div[2]/div[3]/ul/li/div/h4/a/@href")
         shows_url = as.character(shows_url)
         shows_url = paste(base_url,shows_url,sep="")
         show_list = rbind(show_list,cbind(shows_names,shows_url))
         
         print(category_url)
         write.csv(show_list,"show_list.csv",row.names=FALSE)
         show_list
         
        }
}

##################################################################################

show_data_extract = function(show_url,show_data)
        {print(show_url)
         show_page = htmlTreeParse(show_url,useInternal=TRUE)
         categories = xpathSApply(show_page,"//*[@id='mantle_skin']/div[3]/div/div/div[3]/div[7]/div/p[1]/a",xmlValue)
         
         show_name = xpathSApply(show_page,"//*[@id='mantle_skin']/div[3]/div/div/div[1]/h1",xmlValue)
         user_rating = xpathSApply(show_page,"//*[@id='mantle_skin']/div[3]/div/div/div[2]/div[1]/div/div[1]/form/div[2]/div[1]",xmlValue)
         user_votes = xpathSApply(show_page,"//*[@id='mantle_skin']/div[3]/div/div/div[2]/div[1]/div/div[1]/form/div[2]/div[2]/div[2]/span",xmlValue)
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
         
         if(is.null(show_name)) show_name = "NA"
         if(is.null(user_rating)) user_rating = "NA"
         if(is.null(user_votes)) user_votes = "NA"
         if(is.null(Year_Started)) Year_Started = "NA"
         for(category in categories)
          {show_data = rbind(show_data,cbind(category,show_name,user_rating,user_votes,Year_Started))}
         show_url_new = unlist(str_split(show_url,"/"))
         show_url_new = show_url_new[length(show_url_new)-1]
         file = paste(show_url_new,".csv",sep="")
         write.csv(show_data,file,row.names=FALSE)
         Sys.sleep(abs(rnorm(2,0.5)))
         show_data
         
        }

###################################################################################

decade_tagging = function(n)
        { d = character()
          if(n>=1910 && n<=1950) d = "1911-1950"
#           else if(n>=1921 && n<=1930) d = "1921-1930"
#           else if(n>=1931 && n<=1940) d = "1931-1940"
#           else if(n>=1941 && n<=1950) d = "1941-1950"
          else if(n>=1951 && n<=1960) d = "1951-1960"
          else if(n>=1961 && n<=1970) d = "1961-1970"
          else if(n>=1971 && n<=1980) d = "1971-1980"
          else if(n>=1981 && n<=1990) d = "1981-1990"
          else if(n>=1991 && n<=2000) d = "1991-2000"
          else if(n>=2001 && n<=2010) d = "2001-2010"
          else if(n>=2011 && n<=2014) d = "2011-2014"
          d
        
        }

##################################################################################

show_rating = function(n,upper_t)
        { if(n>upper_t) tag = "Popular"
          else if(n<=upper_t) tag = "Not so much !!"
          tag
        }