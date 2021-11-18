#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  

    # Application title
    titlePanel("FFQ analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          fileInput("file", label = h3("File input"),accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")),
          
          tags$hr(),
          
          #checkboxInput("smooth", "Smooth"),
          #conditionalPanel(
            #condition = "input.smooth == true",
            #selectInput("smoothMethod", "Method",
                        #list("lm", "glm", "gam", "loess", "rlm"))),
          
          fluidRow(column(4, verbatimTextOutput("value"))),
          tags$div(id = 'placeholder')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
              tabPanel("Summary Plot",plotOutput('plot')),
              tabPanel("Category Summary Plot",plotOutput('cat_plot')),
              tabPanel("PCA by Food",plotOutput('pca_food')),
              tabPanel("PCA by Category",plotOutput('pca_cat'))
        )
    )
)))
