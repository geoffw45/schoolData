
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(data.table)



df1<-as.data.frame(read.csv("~/Google Drive/R/SchoolsDate/Data2016.csv", header = TRUE, stringsAsFactors = FALSE))

#df1$symbol<-0
#df1$size=5
#df1$opacity = 0.25

View(df1)


kw<-subset(df1, df1$URN==141131)
nla<-subset(df1, df1$URN==131065)
st<-subset(df1, df1$URN==139589)
utc<-subset(df1, df1$URN==139588)

LA<-(unique(sort(df1$LA.Name)))
Typ<-(unique(sort(df1$NFTYPE.x)))
Gender<-(unique(sort(df1$GENDER)))
Rel<-(unique(sort(df1$RELDENOM)))



# Define server logic required to draw a histogram
shinyServer <- function(input, output) {
  output$Selected <- renderUI({
    switch(input$Data,
           "All Data" =  selectInput("Selected",
                                     label = "All Data Selected",
                                     choices = "All Data"
                                      ),
           
           "Local Authority" =  selectInput("Selected",
                                            label = "Choose a Local Authority",
                                            choices = c(LA)
                                            
           ),
           
           "Sponsor Type" =  selectInput("Selected",
                                            label = "Choose a Sponsor Type",
                                            choices = c(Typ)
                                            
           ),
           
           "Gender" =  selectInput("Selected",
                                         label = "Choose Gender",
                                         choices = c(Gender)
                                         
           ),
           
           "Denomination" =  selectInput("Selected",
                                   label = "Choose Denomination",
                                   choices = c(Rel)
           )
           
    )

  })
  
  
  output$values <- renderText({
    outputValues()
    paste(commnt)
  })
  
  outputValues <- reactive({
   
    Sel<-input$Selected
    

    if (input$Data=="All Data"){
      
        df2<<-df1
        
      } else if (input$Data=="Local Authority"){
        
        df2<<-df1[df1$LA.Name==Sel,]
         
      } else if (input$Data=="Sponsor Type"){
        
         df2<<-subset(df1, df1$NFTYPE.x==Sel)
         
      } else if (input$Data=="Gender"){
        
         df2<<-subset(df1, df1$GENDER==Sel)
         
      } else if (input$Data=="Denomination"){
        
        df2<<-subset(df1, df1$RELDENOM==Sel)
        
      }
View(df2)
    
#All school data
    avP8All<-round(mean(df1$P8MEA, na.rm = TRUE),2)
    sdP8All<-round(sd(df1$P8MEA, na.rm = TRUE),2)
    avAtt8All<-round(mean(df1$ATT8SCR, na.rm = TRUE),2)
    sdAtt8All<-round(sd(df1$ATT8SCR, na.rm = TRUE),2)
    nAll<-length(df1$X)
    medP8All<-median(df1$P8MEA, na.rm = TRUE)
    medAtt8All<-median(df1$ATT8SCR, na.rm = TRUE)
    
    P8All<-paste(avP8All,"±",sdP8All)
    ATT8All<-paste(avAtt8All,"±",avAtt8All)

    avP8<-round(mean(df2$P8MEA, na.rm = TRUE),2)
    sdP8<-round(sd(df2$P8MEA, na.rm = TRUE),2)
    avAtt8<-round(mean(df2$ATT8SCR, na.rm = TRUE),2)
    sdAtt8<-round(sd(df2$ATT8SCR, na.rm = TRUE),2)
    n<-length(df2$X)
    medP8<-median(df2$P8MEA, na.rm = TRUE)
    medAtt8<-median(df2$ATT8SCR, na.rm = TRUE)
    
    SelP8<-paste(avP8,"±",sdP8)
    SelATT8<-paste(avAtt8,"±",sdAtt8)
    
    
    summDF<-data.frame(cbind("All Data",P8All,ATT8All, medP8All, medAtt8All, nAll))
    summDF1<-data.frame(cbind("Selection",SelP8,SelATT8, medP8, medAtt8, n))

    
    colnames(summDF)<-c("Selection", "mean P8 +/- SD", "mean Att8 +/-SD", "Median P8", "Median Att8", "Number")
    colnames(summDF1)<-c("Selection", "mean P8 +/- SD", "mean Att8 +/-SD", "Median P8", "Median Att8", "Number")
    
    View(summDF)
    View(summDF1)
    
    res<<-data.frame(rbind(summDF[1,], summDF1[1,]))
    View(res)

    
    commnt<<-paste(length(df2$X),"schools. The average P8 for 2016 was ",avP8,"±",sdP8,"and Attainment8 was", avAtt8,"±",sdAtt8)
    #commnt<-paste("The table and chart below show the national summary and a summary for selected data. Further detail on a school can be foound by hovering over a popint on the chart.")
    
    output$table <-renderTable({res})
    
    output$plot <-renderPlotly({
      pl<-ggplot(df2, aes(x=P8MEA,y=ATT8SCR,  alpha = 0.01, colour = "blue"))+geom_point()+ xlim(-3,2)+ylim(-10,80)
 
      pl<-pl+geom_point(data=kw, aes(x=P8MEA, y=ATT8SCR, fill = kw$SCHNAME), colour = "black", size = 4, shape = 15) 
      pl<-pl+geom_point(data=nla, aes(x=P8MEA, y=ATT8SCR, fill = nla$SCHNAME), colour = "black", size = 4, shape = 16)
      pl<-pl+geom_point(data=utc, aes(x=P8MEA, y=ATT8SCR, fill = utc$SCHNAME), colour = "black", size = 4, shape = 17)
      pl<-pl+geom_point(data=st, aes(x=P8MEA, y=ATT8SCR, fill = st$SCHNAME), colour = "black", size = 4, shape = 18)
      
      pl<-pl+geom_vline(xintercept = 0.5)+geom_vline(xintercept = -0.5)
      pl<-pl+scale_fill_discrete(name="School Type",
                                 breaks=c(df2$NFTYPE.x),
                                 labels=c(df2$NFTYPE.x))
      ggplotly(pl)
      

      
    })
    
  })
  
}