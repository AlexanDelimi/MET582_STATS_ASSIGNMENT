#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(reshape2)
library(dplyr)
library(tidyverse)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(nortest)
library(sm)
library(tibble)
library(devtools)
library(ggbiplot)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
    
  getdata<-reactive({
      inFile <- input$file
      
      if (is.null(inFile))
        return(NULL)
      
      DOMINO <- read_csv(file = inFile$datapath, col_names = TRUE, na = "NA") %>%
        select(COUNTRY:OLIVES)
      DOMINO[DOMINO == -9] <- NA
      DOMINO[DOMINO == -8] <- NA
      DOMINO$ID <- seq.int(nrow(DOMINO))
      return(DOMINO %>%
               select(COUNTRY, ID, BEEF:OLIVES))
    })
    
    getcategories<-reactive({
      DOMINO<-getdata()
      MeatFish <- DOMINO %>%
        select(COUNTRY, ID, BEEF:TROUT) %>%
        pivot_longer(cols = BEEF:TROUT, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "MEAT & FISH")
      
      Bread <- DOMINO %>%
        select(COUNTRY, ID, WHTBREADSL:RYEBRD) %>%
        pivot_longer(cols = WHTBREADSL:RYEBRD, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "BREAD & BISCUITS")
      
      Cereals <- DOMINO %>%
        select(COUNTRY, ID, PORRIDGE:CEREALMUS) %>%
        pivot_longer(cols = PORRIDGE:CEREALMUS, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "CEREALS")
      
      PotRicPas <- DOMINO %>%
        select(COUNTRY, ID, POTSPREP:BARLYGRO) %>%
        pivot_longer(cols = POTSPREP:BARLYGRO, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "POTATOES, RICE & PASTA")
      
      DairyFats <- DOMINO %>%
        select(COUNTRY, ID, SICREAM:OTHSALAD) %>%
        pivot_longer(cols = SICREAM:OTHSALAD, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "DAIRY & FATS")
      
      OnBreadVeg <- DOMINO %>%
        select(COUNTRY, ID, BUTTERMILK:VLOWSPREAD) %>%
        pivot_longer(cols = BUTTERMILK:VLOWSPREAD, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "TOPPING")
      
      SweetSnacks <- DOMINO %>%
        select(COUNTRY, ID, SWBISCHO:JELLY) %>%
        pivot_longer(cols = SWBISCHO:JELLY, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "SWEETS & SNACKS")
      
      SoupsSpreads <- DOMINO %>%
        select(COUNTRY, ID, VEGSOUP:COLDSOUP) %>%
        pivot_longer(cols = VEGSOUP:COLDSOUP, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "SOUPS & SPREADS")
      
      Drinks <- DOMINO %>%
        select(COUNTRY, ID, TEA:TOMJUICE) %>%
        pivot_longer(cols = TEA:TOMJUICE, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "DRINKS")
      
      Fruit <- DOMINO %>%
        select(COUNTRY, ID, APPLES:BLACKCURR) %>%
        pivot_longer(cols = APPLES:BLACKCURR, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "FRUIT")
      
      Veg <- DOMINO %>%
        select(COUNTRY, ID, CARPUMP:OLIVES) %>%
        pivot_longer(cols = CARPUMP:OLIVES, names_to = "FOOD", values_to = "FFQ") %>%
        add_column(CATEGORY = "VEG")
      
      
      # Create new table
      DOMINO_bind <- rbind(MeatFish, Bread, Cereals, PotRicPas, DairyFats, OnBreadVeg, SweetSnacks, SoupsSpreads, Drinks, Fruit, Veg)
      
      # Reorder
      DOMINO_cat <- DOMINO_bind %>%
        select(ID, COUNTRY, CATEGORY, FOOD, FFQ)
      DOMINO_cat
    })
    
    
    output$plot <- renderPlot({
      DOMINO_grouped <- pivot_longer(data = getdata(), cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
      
       ggplot (data = DOMINO_grouped, mapping = aes(x = FOOD, y = FFQ)) + 
        geom_col(aes(fill = COUNTRY)) +
        labs(title = "Cross-cultural Nutrional Differences") +
        xlab("Food Type") +
        ylab("FFQ Score") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
        theme(legend.position="bottom") +
        scale_fill_brewer(palette="Spectral")
    })
    
    output$cat_plot<-renderPlot({
    ggplot (data = getcategories(), mapping = aes(x = CATEGORY, y = FFQ)) + 
        geom_col(aes(fill = COUNTRY)) +
        labs(title = "Cross-cultural Nutrional Differences by Food Category") +
        xlab("Food Category") +
        ylab("FFQ Score") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
        scale_fill_brewer(palette="Spectral")
    })
    output$pca_food<-renderPlot({
      DOMINO_grouped <- pivot_longer(data = getdata(), cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
      DOMINO_food <- DOMINO_grouped %>%
        na.omit() %>%
        pivot_wider(id_cols = COUNTRY:ID, names_from = FOOD, values_from = FFQ) %>%
        select(BEEF:OLIVES)
      
      DOMINO_pca <- prcomp(DOMINO_food, center = TRUE, scale. = TRUE)
      DOMINO_countries <- c(rep("UK", 10), rep("Switzerland", 2), rep("Poland", 18), rep("Spain", 30))
      DOMINO_bp <- ggbiplot(DOMINO_pca, ellipse = TRUE,  groups = DOMINO_countries)
      DOMINO_bp
      
      })
    output$pca_cat<-renderPlot({   
      DOMINO_wide <- getcategories() %>%
      na.omit() %>%
      select(-FOOD) %>%
      pivot_wider(id_cols = ID:COUNTRY, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
      select(3:13)
    DOMINO_cat_pca <- prcomp(DOMINO_wide, center = TRUE, scale. = TRUE)
    DOMINO_countries <- c(rep("UK", 10), rep("Switzerland", 2), rep("Poland", 18), rep("Spain", 30))
    DOMINO_cat_bp <- ggbiplot(DOMINO_cat_pca, ellipse = TRUE,  groups = DOMINO_countries)
    DOMINO_cat_bp})
    
})
