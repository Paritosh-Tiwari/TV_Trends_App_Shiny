
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

options(RCHART_LIB = 'nvd3')
shinyUI(pageWithSidebar(
        headerPanel("The Idiot Box Trends"),
        
        sidebarPanel(
                selectInput(inputId = "Category",
                            label = h4("Select Category"),
                            choices = sort(unique(data[,2])),
                            selected = "Action & Adventure"),
                helpText("Select Category to see changing trend of categories with time"),
                #hr(),
                
                radioButtons("radio", label = h4("Summarize Data by"),
                             choices = list("Category" = 1, "Period" = 2,"Both" = 3), 
                             selected = 1),
                
                helpText("Select Both to see Top 20 Shows in the selected category during the selected Period. (Give some time for data to load and ignore the initial error)"),
                #hr(),
                selectInput(inputId = "Period",
                            label = h4("Select Period"),
                            choices = sort(unique(data[,1])),
                            selected = "2011-2014"),
                
                helpText("Select Period to see Top 10 Popular Categories in the desired time-period")
                
                
                
                
        ),
        
        mainPanel(
                wellPanel(
                        
                        conditionalPanel(
                                condition = "input.radio == '3'",
                                imageOutput('preImage',height='auto',width='auto'),
                                
                                dataTableOutput(outputId="top_20")
                                        ),
                        
                        conditionalPanel(
                                condition = "input.radio != '3'",
                                showOutput("myChart","nvd3")
                                        )
                        
                        )  
                
                
                
                )
                
        )
    )

