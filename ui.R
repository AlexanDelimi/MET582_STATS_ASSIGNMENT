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
          selectInput('choice','Choose where the statistical analysis will be performed on',choices =c( "Individual Foods","Food Categories")),
          
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
              tabPanel("Summary",  verbatimTextOutput("summary")),
              tabPanel("Summary Plot",plotOutput('plot',height = 300, brush = brushOpts(id = "plot2_brush", clip = TRUE, resetOnNew = TRUE))),
              tabPanel("PCA",plotOutput('pca',height = 300, brush = brushOpts(id = "plot2_brush", clip = TRUE, resetOnNew = TRUE)),verbatimTextOutput("pca_summary")),
              tabPanel("Missingness",plotOutput("missingno"),plotOutput('missingno1'),uiOutput("country"),plotOutput('missingno2')),
              tabPanel("Kruskal-Wallis",selectInput('category', "Choose the category to perform Kruskal Wallace test on", choices=c("Meat and Fish"="MEAT & FISH",
                                                                                                                                    "Bread and Savoury Biscuits"="BREAD & BISCUITS",
                                                                                                                                    "Cereals"="CEREALS",
                                                                                                                                    "Potatotes, Rice and Pasta"="POTATOES, RICE & PASTA",
                                                                                                                                    "Dairy Products and Fats"="DAIRY & FATS",
                                                                                                                                    "On Bread or Vegetables"= "TOPPING",
                                                                                                                                    "Sweets and Snacks"="SWEETS & SNACKS",
                                                                                                                                    "Soups, Sauces and Spreads"="SOUPS & SPREADS",
                                                                                                                                    "Drinks"="DRINKS",
                                                                                                                                    "Fruit"="FRUIT",
                                                                                                                                    "Vegetables"="VEG")), verbatimTextOutput("test"))
        )
    )
)))
