#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(extrafont)
library(forcats)

# Ref the following http://shanghai.hosting.nyu.edu/data/r/case-2-2-shinyapp.html to get the switch

# converted long format data from University enrollment data by Status. Retrieved from https://data.ontario.ca/en/dataset/university-enrolment

university_data <- 
    tibble(
        `Enrollment Period` = c("2012-2013","2012-2013","2012-2013","2012-2013","2013-2014","2013-2014","2013-2014","2013-2014","2014-2015", "2014-2015", "2014-2015","2014-2015","2015-2016","2015-2016","2015-2016","2015-2016","2016-2017","2016-2017","2016-2017","2016-2017", "2017-2018" ,"2017-2018", "2017-2018", "2017-2018", "2018-2019", "2018-2019", "2018-2019", "2018-2019")
        ,
        Type = c( "Undergraduate", "Undergraduate", "Graduate", "Graduate", "Undergraduate", "Undergraduate", "Graduate", "Graduate"    , "Undergraduate", "Undergraduate", "Graduate", "Graduate", "Undergraduate", "Undergraduate", "Graduate", "Graduate", "Undergraduate" ,"Undergraduate", "Graduate" ,     "Graduate" ,"Undergraduate", "Undergraduate", "Graduate", "Graduate"     
                  ,"Undergraduate", "Undergraduate", "Graduate" ,"Graduate" ),
        Status = c("Domestic",  "International", "Domestic",  "International", "Domestic",  "International", "Domestic" ,     "International" ,"Domestic", "International", "Domestic", "International", "Domestic", "International", "Domestic", "International"
                   ,"Domestic", "International", "Domestic", "International", "Domestic", "International", "Domestic", "International","Domestic" ,     "International", "Domestic", "International")
        ,
        Headcount = c(50671,  8564, 11817,  2163, 51234,  9477, 12283,  2407, 50900, 11025, 12659,  2598, 51424, 12296, 13041,  2777, 50850, 13486, 13618,  2988, 50000, 14947, 13960,  3097, 49401, 16319, 14339,  3495)
    )




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h2("University of Toronto Enrollment Headcount by Enrollment Status", align = "center")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "peak",
                label = "Enrollment Type:",
                choices = c("Undergraduate", "Graduate"))
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("stackbar"),
           br(),
           br(),
           br(),
           plotlyOutput("line"),
           br(),
           br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dataInput <- reactive({
        switch(input$peak,
               "Undergraduate" = university_data %>% filter(Type == "Undergraduate"),
               "Graduate" = university_data %>% filter(Type == "Graduate"))
    })

    output$stackbar <- renderPlotly({
         
           plot <-  ggplot(dataInput(),aes(x = `Enrollment Period`, y = Headcount, fill = fct_rev(Status)))+
            geom_col(width = 0.7)+
            theme_minimal()+
            labs(x = "Enrollment Period",
                 y = "Headcount")+
            scale_fill_hue(c=70,  l=55)+
            scale_fill_manual(values = c("#FF6666","#619CFF"))+
            ggtitle(paste("University of Toronto Fall Full-time", input$peak, "Enrollment Headcount"))+
            theme(text=element_text(size=12,  family="serif"),
                  plot.title = element_text(hjust = 0.5), legend.title = element_blank())
              
        ggplotly(plot, tooltip = c("Headcount")) %>% config(displayModeBar = FALSE) %>% layout(hoverlabel=list(bgcolor="white"))
        
    })
    output$line <- renderPlotly({
        
        plot <- 
            ggplot(dataInput(),aes(x = `Enrollment Period`, y = Headcount, group = Status, color = Status))+
            geom_line()+
            geom_point(alpha = 0.8)+
            guides(color = guide_legend(reverse = TRUE), group = guide_legend(reverse = TRUE))+
            theme_minimal()+
            labs(x = "Enrollment Period",
                 y = "Headcount")+
            scale_color_hue(c=70,  l=55)+
            scale_color_manual(values = c("#619CFF","#FF6666"))+
            ggtitle(paste("University of Toronto Fall Full-time", input$peak, "Enrollment Headcount"))+
            theme(text=element_text(size=12,  family="serif"),
                  plot.title = element_text(hjust = 0.5), legend.title = element_blank())+
            expand_limits(y = 0)
    
        ggplotly(plot, tooltip = c("Headcount")) %>% config(displayModeBar = FALSE) %>% layout(hoverlabel=list(bgcolor="white"))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
