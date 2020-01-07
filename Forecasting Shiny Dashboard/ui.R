#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(prophet)
library(shinythemes)
library(xlsx)
library(shinyWidgets)
library(plotly)
# Define UI for data upload app ----
#ui ----
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Machine Learning Prediction Dashboard",titleWidth = 400),
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu(
      menuItem("Upload Data", tabName = "dataupload", icon = icon("dashboard")),
      menuItem("ML Model", tabName = "MLModel", icon = icon("th")),
      menuItem("Prediction", tabName = "Prediction", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #f4b943;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #f4b943;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #f4b943;
                              }        
                              
                              /* main sidebar */
                              .skin-red .main-sidebar {
                              background-color: darkslategrey;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ff69b4;
                              }
                              '))),
    tabItems(  
      # First tab content
      tabItem(tabName = "dataupload",h2("User Data Upload"),
              # Parameter Boxes
              fluidRow(
                column(width = 4,
                       box(title = "Data Upload", width = 12, height = 225,solidHeader = TRUE,status = "success", 
                           radioButtons("dataType", "File Type",choices = c(CSV = "CSV", Excel = "XLSX"),
                                        selected = "CSV", inline = TRUE),
                            # Only show this panel if the plot type is a histogram
                            conditionalPanel(condition = "input.dataType == 'CSV'",
                              # Input: Select a file ----
                              fileInput("file", "Choose CSV File",multiple = FALSE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"))
                            
                              ),
                           # Only show this panel if Custom is selected
                           conditionalPanel(condition = "input.dataType == 'XLSX'",
                                            # Input: Select a file ----
                                            fileInput("file1", "Choose Excel File",multiple = FALSE,
                                                      accept = c(".xlsx")))
                       ),
                       box(title = "Data Range", width = 12, height = 225,solidHeader = TRUE,status = "success", 
                           # Input: Select number of rows to display ----
                           radioButtons("disp", "Display",
                                        choices = c(All = "all",
                                                    Head = "head",
                                                    Tail="tail"),
                                        selected = "head"))
                ),
                column(width = 8,
                       box(title = "Input Data", width = 12,solidHeader = TRUE,status = "success", 
                           DT::dataTableOutput("table1")) 
                )
              )
              ,
              fluidRow(
                column(width = 4,
                       box(title = "Select Algorithm", width = 12, height = 125,solidHeader = TRUE,status = "success",
                           radioButtons("MLType", "Algorithm Type",choices = c(Regression = "R",Classification = "C"),
                                        selected = "R", inline = TRUE)),
                       box(title = "Select Predict Variable", width = 12, height = 120,solidHeader = TRUE,status = "success",
                           selectInput("predict", label = "Select the Column you want to Predict", "")),
                       box(title = "Select Other Variables", width = 12, solidHeader = TRUE,status = "success",
                            checkboxGroupInput("x_variables", label = NULL, ""))
                ),
                column(width = 8,
                       box(title = "Selected Data For Predection Analysis", width = 12, solidHeader = TRUE,status = "success",
                           DT::dataTableOutput("table2"))
                )
              )
            )
       ,
      # Second tab content
      tabItem(tabName = "MLModel",h2("Model Summary & Metrics"),
        fluidRow(
          column(width=12,
                 box(title = "Evaluation Metrics", width = 12, solidHeader = TRUE,status = "success",
                   valueBoxOutput("accuracypercent",width = 3),verbatimTextOutput("accuracy"))
              )
          ),
        fluidRow(
          column(width=12,
              materialSwitch(inputId = "Id079",label = "Click to See Summary Statistics of Prediction Model",
                value = FALSE,status = "success",width=  '100%'),
              conditionalPanel(condition = "input.Id079 == true",
              box(title = "Summary Statistics", width = 12, solidHeader = TRUE,status = "success",
                verbatimTextOutput("summary"))
               )
            )
          ),
          fluidRow(
          column(width=12,
                  box(title = "Plot", width = 12, solidHeader = TRUE,status = "success",
                      #DT::dataTableOutput
                      plotlyOutput("plot"))
           )
         )
            ),# End of Second tab content
      # Third tab content
      tabItem(tabName = "Prediction",h2("Prediction"),
        fluidRow(
          column(width=12,
            box(title = "User Input Parameters", width = 12, solidHeader = TRUE,status = "success",
            uiOutput("numinputs",inline = TRUE))
            )
          ),
        # fluidRow(
        #   column(width=12,
        #     box(title = "Predection Table", width = 12, solidHeader = TRUE,status = "success",
        #     DT::dataTableOutput("table4"))
        #     )
        #   ),
        fluidRow(
          column(width=12,
                 box(title = "Predicted Output", width = 12, solidHeader = TRUE,status = "success",
                              verbatimTextOutput("pred"))
                 
          )
        ),
        fluidRow(
          column(width = 12,
          materialSwitch(inputId = "Id080",label = "Click here for multi-input file upload",
                         value = FALSE,status = "success",width=  '100%'),
          conditionalPanel(condition = "input.Id080 == true",
          column(width = 4,
                 box(title = "Data Upload", width = 12, height = 225,solidHeader = TRUE,status = "success", 
                     radioButtons("uploaddatatype", "File Type",choices = c(CSV = "CSV", Excel = "XLSX"),
                                  selected = "CSV", inline = TRUE),
                     
                     conditionalPanel(condition = "input.uploaddatatype == 'CSV'",
                                      # Input: Select a file ----
                                      fileInput("file2", "Choose CSV File",multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"))),
                     
                     conditionalPanel(condition = "input.uploaddatatype == 'XLSX'",
                                      # Input: Select a file ----
                                      fileInput("file3", "Choose Excel File",multiple = FALSE,
                                                accept = c(".xlsx")))
                 ),
                 box(title = "Data Range", width = 12, height = 225,solidHeader = TRUE,status = "success", 
                     # Input: Select number of rows to display ----
                     radioButtons("uploaddisp", "Display",
                                  choices = c(All = "all",
                                              Head = "head",
                                              Tail="tail"),
                                  selected = "head"))
          )
           ,column(width = 8,
                  box(title = "Uploaded Data", width = 12,solidHeader = TRUE,status = "success", 
                      DT::dataTableOutput("uploadtable")) 
           ))
        )
        ),
        fluidRow (conditionalPanel(condition = "input.Id080 == true",
          column(width=12,
                 box(title = "Prediction Output", width = 12, solidHeader = TRUE,status = "success",
                     downloadButton("downloadData", "Download"),
                     verbatimTextOutput("multipred"))
                 
          ))
        )
        # ,fluidRow(
        #      column(width=12,
        #             box(title = "Plot", width = 12, solidHeader = TRUE,status = "success",
        #                 #DT::dataTableOutput
        #                 plotlyOutput("plot"))
        #      )
        #    )
        
        )
      # End of Third tab content

      ) #End of Tab Items
  )# End of Dashboard Body
) # End of UI


