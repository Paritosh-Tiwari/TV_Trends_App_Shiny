
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



shinyServer(function(input, output) {
        cat = reactive({input$Category})
        p = reactive({input$Period})
        choice = reactive({input$radio})
        top_20 = reactive({ sub_set_2 = data[data$Category==cat() & data$Period == p(),] 
                           sub_set_2 = sub_set_2[,3:10]
                           sub_set_2 = unique(sub_set_2)
                           sub_set_3 = sub_set_2[,c(1:6)]
                           new_data = sub_set_3[!duplicated(sub_set_3[,-c(3,6)]),]
                           ord_set = arrange(new_data,desc(sq))
                           top_20_shows = ord_set[1:20,]         
                           top_20_shows = top_20_shows[,c(1,2,3,4,5)]
                           return(top_20_shows)
                        })
        
        output$myChart <- renderChart({
                if(choice()==1)
                        {sub_set <- subset(t3, Category == cat())
                         a = table(sub_set$Period)
                         a = as.data.frame(a)
                         names(a) = c("Period","Freq")
                         b = a[a[,2]<2,]
                         if(dim(b)[1]>0)
                                {c = merge(b,sub_set,by.x="Period",by.y = "Period")
                                 d = lapply(c[,4],function(x) {if(x=="Popular") return("Not so much !!");if(x=="Not so much !!") return("Popular")})
                                 d = unlist(d)
                                 c$Rating = d
                                 c$Count = 0
                                 c$User_Rating = 0
                                 c$User_Votes = 0
                                 c$sq = 0
                                 c= c[,c(1,3,4,5,6,7,8)]
                                 e = rbind(sub_set,c)
                                }
                        if(dim(b)[1]==0) {e = sub_set}
                         
                        e = e[order(e$Period,e$Rating),]
                         
                        n3 <- nPlot(Count ~ Period, group = "Rating", data = e, type = "multiBarChart")
                        n3$addParams(dom = 'myChart')
                        n3$chart(color = c('blue', 'green'))
                        n3$chart(reduceXTicks = FALSE)
                        n3$chart(stacked = TRUE)
                        n3$yAxis( axisLabel = "Number  of  TV  Shows" )
                        n3$xAxis( axisLabel = "Time Period" )
                        n3$chart(margin = list(left = 75))
                        return(n3)
                        }
                        
                if(choice()==2)
                        {sub_set <- subset(t3, Period == p())
                         if(dim(sub_set)[1]<length(unique(sub_set$Category))*2)
                                {a = table(sub_set$Category)
                                 a = as.data.frame(a)
                                 names(a) = c("Category","Freq")
                                 b = a[a[,2]<2,]
                                 if(dim(b)[1]>0)
                                        {c = merge(b,sub_set,by.x="Category",by.y = "Category")
                                         d = lapply(c[,4],function(x) {if(x=="Popular") return("Not so much !!");if(x=="Not so much !!") return("Popular")})
                                         d = unlist(d)
                                         c$Rating = d
                                         c$Count = 0
                                         c$User_Rating = 0
                                         c$User_Votes = 0
                                         c$sq = 0
                                         c= c[,c(1,3,4,5,6,7,8)]
                                         e = rbind(sub_set,c)
                                        }
                                 if(dim(b)[1]==0) {e = sub_set}
                                }
                                         
                         if(dim(sub_set)[1]==length(unique(sub_set$Category))*2) {e = sub_set}
                         
                         e = e[order(e$Category,e$Rating),]
                         f = e[e$Rating=="Popular",]
                         cat_summary = ddply(f,'Category',summarize,popular_count_total = sum(Count))
                         cat_summary = cat_summary[order(cat_summary$popular_count_total),]
                         cat_in_scope = cat_summary[(dim(cat_summary)[1]-10):(dim(cat_summary)[1]),1]
                         
                         e = e[e$Category %in% cat_in_scope,]
                         
                         n3 <- nPlot(Count ~ Category, group = "Rating", data = e, type = "multiBarChart")
                         n3$addParams(dom = 'myChart')
                         n3$chart(color = c('blue', 'green'))
                         n3$chart(reduceXTicks = FALSE)
                         n3$chart(stacked = TRUE)
                         n3$xAxis(rotateLabels=-20)
                         n3$xAxis( axisLabel = "Top  10  Categories" )
                         n3$yAxis( axisLabel = "Number  of  TV  Shows" )
                         n3$chart(margin = list(left = 100))
                         return(n3)
                        }
                
                })
        
                
        
        output$top_20 <- renderDataTable({
                if(choice()==3) 
                        {top_20_shows = top_20()
                         top_20_shows[,5] = paste('<a href="', top_20_shows[,5], '">', top_20_shows[,5], '</a>', sep = "")
                         top_20_shows
                        }
                        
                })
        
        output$preImage <- renderImage({
                if(choice()==3)
                        {top_20_shows = top_20()
                         #list(src = "NullImgae.jpg",alt = "Image2")
                         if(nchar(top_20_shows[1,1])==2) 
                                {list(src = "NullImgae.jpg",alt = "Image2")}
                         if(nchar(top_20_shows[1,1])>2)
                                {page = htmlTreeParse(top_20_shows[1,5],useInternal=TRUE)
                                 image_link = xpathSApply(page,"//div[@class='m featured_story']/*//a/img/@src | //div[@class='m show_sum_large']/*//img/@src | //div[@class='image_bg']//img/@src") 
                                 filename = "c.jpg"
                                 image_link = gsub(" ","%20",image_link)
                                 download.file(image_link[1],"c.jpg",mode="wb")
                                 list(src = "c.jpg",alt = "Image")
                                }
                         
                        } 
                }, deleteFile = FALSE )
        
        
        
                         
                
        })
