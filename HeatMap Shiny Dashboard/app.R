#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
ugdphseas <- structure(data.frame(
  c(0, 2, 30, 113, 12, 30, 14, 36), 
  c(1, 1, 27, 43, 14, 23, 23, 23), 
  c(0, 1, 3, 1, 0, 2, 0, 2), 
  c(0, 0, 6, 9, 1, 1, 4, 4), 
  c(7, 6, 105, 110, 46, 69, 76, 71), 
  c(5, 2, 52, 34, 13, 44, 61, 46), 
  c(12, 8, 127, 115, 86, 77, 79, 82), 
  c(3, 2, 22, 51, 10, 2, 6, 10),
  c(0, 0, 3, 5, 2, 4, 2, 1), 
  c(0, 0, 18, 6, 2, 3, 2, 12), 
  c(9, 5, 150, 219, 87, 82, 75, 71), 
  c(8, 2, 48, 60, 13, 39, 67, 38), 
  c(2, 3, 138, 394, 241, 51, 136, 54), 
  c(0, 0, 1, 0, 0, 1, 1, 0), 
  c(7, 13, 89, 39, 31, 102, 77, 22), 
  c(0, 0, 13, 22, 68, 6, 0, 1), 
  row.names = c("ANAKAPALLI ", "BHEEMILI ", "ZONE1 ", "ZONE2 ", "ZONE3 ", "ZONE4 ", "ZONE5 ", "ZONE6 "), stringsAsFactors = FALSE),
  names = make.names(c("Absenteesim of door to door garbage collector ", "Absenteesim of sweepers ",
                       "Broken Bin ", "Burning of garbage ", "Desilting of Drain ", 
                       "Illegal draining of sewage to SWD/Open site ", "Improper Sweeping ",
                       "Obstruction of water flow ",
                       "Over flowing of garbage bins ", "Provision of garbage bin ",
                       "Removal of garbage ", "Stagnation of water ",
                       "UGD Over Flow ", "Chikungunya", "Dengue", "Malaria"), unique = TRUE))



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   titlePanel("Grievances and Sanitation Correlation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
       
      # Copy the line below to make a select box 
      
      selectInput("select1", label = h3("Type of Display in Chart"), 
                  choices = list("Circle" = "circle", 
                                 "Square " = "square",
                                 "Number" = "number", 
                                 "Shade " = "shade"), 
                  selected = 1)
    
      ),
      # Show a plot of the generated distribution
      mainPanel(
        # tabsetPanel(type = "tabs",
        #             tabPanel("Plot", 
                             textOutput("selected_var"),
                             plotOutput("distPlot"),
                             h3("Summary"),
                             textOutput("action"),
                             textOutput("action1"))
                    #,tabPanel("Summary", verbatimTextOutput("summary")),
                    #tabPanel("Table", tableOutput("table"))
                    
        
         #tableOutput('tbl')
      )
   )
# )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     ugdphseas.cor <- cor(
       x = if(is.data.frame(ugdphseas)) ugdphseas[, sapply(ugdphseas, is.numeric), drop = FALSE] else ugdphseas, 
       use = "everything", method = "pearson")
     df<-as.table(ugdphseas.cor)
     library(corrplot)
    
      # draw the histogram with the specified number of bins
     corrplot(ugdphseas.cor, type="lower",method = input$select1,
              tl.offset = 0.5,tl.cex = 0.9,number.cex = .7,sig.level = 0.01,
              insig = "blank")
   })
   output$selected_var <- renderText({ 
     paste(
       "Pearson correlation measures a linear dependence between variables")
   })
   output$action <- renderText({ 
     paste("1. Illegal Draining of Sewage  , Improper Sweeping and Stagnation of Water are Highly Correlated to Diseases Chikungunaya and Dengue.
           These Complants need to be attended with highest Priority."
           )
   })
   output$action1 <- renderText({ 
     paste("2. Stagnation of Water is co-related with Desilting of Drain, Illeagal Draining of Sewage,Improper Sweeping and Obstruction of Water Flow.
           This Clearly Indicates a case of Stagnation Leading to the following co-occurance of Complaints or vice-versa."
           )
   })
   output$table <- renderDataTable(ugdphseas)
}

# Run the application 
shinyApp(ui = ui, server = server)

