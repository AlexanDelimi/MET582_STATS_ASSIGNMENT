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

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ = source for colour blind friendly palettes 

# Define server logic.
shinyServer(function(input, output) {
    
    
    
    getdata<-reactive({
        inFile <- input$file
        
        if (is.null(inFile))
            return(NULL)
        
        DOMINO <- read_csv(file = inFile$datapath, col_names = TRUE, na = "NA") %>%
            select(COUNTRY:OLIVES)
        DOMINO[DOMINO == -9] <- NA
        DOMINO[DOMINO == -8] <- NA
        DOMINO[DOMINO ==  1] <- 0       #FFQ NEVER
        DOMINO[DOMINO ==  2] <- 0.067   #FFQ 1-3 TIMES A MONTH(AVERAGE 2 TIMES A MONTH 2/30)
        DOMINO[DOMINO ==  3] <- 0.143   #FFQ ONCE A WEEK 4.29/30
        DOMINO[DOMINO ==  4] <- 0.429   #FFQ 2-4 TIMES A WEEK AVG 3 TIMES A WEEK 3*4.29/30
        DOMINO[DOMINO ==  5] <- 0.786   #FFQ 5-6 TIMES A WEEK AVG 5.5 TIMES A WEEK 5.5*4.29/30
        DOMINO[DOMINO ==  6] <- 1       #FFQ ONCE A DAY 30/30
        DOMINO[DOMINO ==  7] <- 2.5     #FFQ 2-3 TIMES A DAY 2.5*30/30   
        DOMINO[DOMINO ==  8] <- 4.5     #FFQ 4-5 TIMES A DAY 4.5*30/30 
        DOMINO[DOMINO ==  9] <- 6       #FFQ 6+ TIMES A DAY 6*30/30
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
            select(COUNTRY, ID, SICREAM:SOURMILK) %>%
            pivot_longer(cols = SICREAM:SOURMILK, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "DAIRY & FATS")
        
        OnBreadVeg <- DOMINO %>%
            select(COUNTRY, ID, BUTTER:VLOWSPREAD) %>%
            pivot_longer(cols = BUTTER:VLOWSPREAD, names_to = "FOOD", values_to = "FFQ") %>%
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
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    observe({
        brush <- input$Disp_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
    getsummary<-reactive({
        if(input$choice =='Individual Foods')
        {
            DOMINO_grouped <- pivot_longer(data = getdata(), cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
            ggplot (data = DOMINO_grouped, mapping = aes(x = FOOD, y = FFQ)) + 
                geom_col(aes(fill = COUNTRY)) +
                labs(title = "Cross-cultural Nutrional Differences") +
                xlab("Food Type") +
                ylab("FFQ Score") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
                theme(legend.position="bottom") +
                scale_fill_manual(values=cbbPalette)
                }
        else
        {DOMINO_grouped <-getcategories()
        ggplot (data = getcategories(), mapping = aes(x = CATEGORY, y = FFQ)) + 
            geom_col(aes(fill = COUNTRY)) +
            labs(title = "Cross-cultural Nutrional Differences by Food Category") +
            xlab("Food Category") +
            ylab("FFQ Score") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
            scale_fill_manual(values=cbbPalette)}
    })
    
    output$plot <- renderPlot({
        getsummary()
        
    })
    
    getpca<-reactive({
        if(input$choice =='Individual Foods')
        { DOMINO<-getdata()
        DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
        
        DOMINO_food <- DOMINO_grouped %>%
            na.omit() %>%
            pivot_wider(id_cols = COUNTRY:ID, names_from = FOOD, values_from = FFQ) %>%
            select(BEEF:OLIVES)
        
        DOMINO_pca <- prcomp(DOMINO_food, center = TRUE, scale. = TRUE)
        DOMINO_countries <- as_vector(DOMINO%>%
                                          select(COUNTRY))
        DOMINO_bp <- ggbiplot(DOMINO_pca, choices = 1:2, ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette) #Comparing PC1 and PC2
        #DOMINO_bp2 <- ggbiplot(DOMINO_pca, choices = 1:3, ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette) #Comparing PC1 and PC3
        #DOMINO_bp2 <- ggbiplot(DOMINO_pca, choices = 2:3, ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette) #Comparing PC2 and PC3
        #These are the different pca plots
        }
        else{
            DOMINO<-getdata()
            DOMINO_wide <- getcategories() %>%
                na.omit() %>%
                select(-FOOD) %>%
                pivot_wider(id_cols = ID:COUNTRY, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
                select(3:13)
            DOMINO_cat_pca <- prcomp(DOMINO_wide, center = TRUE, scale. = TRUE)
            DOMINO_countries <- as_vector(DOMINO%>%
                                              select(COUNTRY))
            DOMINO_bp <- ggbiplot(DOMINO_cat_pca, choices = 1:2, ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette)
            #DOMINO_bp2 <- ggbiplot(DOMINO_cat_pca, choices = 1:3, ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette)
            #DOMINO_bp3 <- ggbiplot(DOMINO_cat_pca, choices = 2:3,  ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette)
            #return(DOMINO_cat_bp)
        }
        return(DOMINO_bp)
    })
    pca_sum<-reactive({
        if(input$choice =='Individual Foods')
        { DOMINO<-getdata()
        DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
        
        DOMINO_food <- DOMINO_grouped %>%
            na.omit() %>%
            pivot_wider(id_cols = COUNTRY:ID, names_from = FOOD, values_from = FFQ) %>%
            select(BEEF:OLIVES)
        
        DOMINO_pca <- prcomp(DOMINO_food, center = TRUE, scale. = TRUE)
        DOMINO_pca
        }
        else{
            DOMINO<-getdata()
            DOMINO_wide <- getcategories() %>%
                na.omit() %>%
                select(-FOOD) %>%
                pivot_wider(id_cols = ID:COUNTRY, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
                select(3:13)
            DOMINO_pca <- prcomp(DOMINO_wide, center = TRUE, scale. = TRUE)
            DOMINO_pca
        }
        
    })
    
    output$pca_summary<-renderPrint({summary(pca_sum())})
    output$pca<-renderPlot({
        getpca()
    })
    
    summ<-reactive({
        if(input$choice =='Individual Foods')
        {summary(getdata())}
        else
            summary(DOMINO_wide <- getcategories() %>%
                        na.omit() %>%
                        select(-FOOD) %>%
                        pivot_wider(id_cols = ID:COUNTRY, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
                        select(3:13))
    })
    
    output$summary<-renderPrint({summ()})
    output$missingno<-renderPlot({
        DOMINO<-getdata()
        proportion_missing <- colSums(is.na(DOMINO)) / nrow(DOMINO)
        Food_group <- colnames(DOMINO) 
        missing_data_country <- tibble(Food_group, proportion_missing) %>%
            filter(Food_group != "COUNTRY")
        
        missing_data_country %>%
            ggplot(aes(x = Food_group, y = proportion_missing)) +
            geom_col() +
            geom_text(aes(label = ifelse(proportion_missing > 0, as.character(Food_group), ''))) +
            labs(x = "Food group", 
                 y = "proportion of values missing", 
                 title = "Proportion of values which are missing for each food group across all countries")+
            theme_minimal() +
            scale_colour_manual(values=cbbPalette) +
            theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
    })
    output$missingno1<-renderPlot({
        missing.values <- getdata() %>%
            gather(key = "key", value = "val") %>%
            mutate(isna = is.na(val)) %>%
            group_by(key) %>%
            dplyr::mutate(total = n()) %>%
            group_by(key, total, isna) %>%
            dplyr::summarise(num.isna = n()) %>%
            mutate(pct = num.isna / total * 100)
        
        levels <-
            (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
        
        percentage.plot <- missing.values %>%
            ggplot() +
            geom_bar(aes(x = reorder(key, desc(pct)), 
                         y = pct, fill=isna), 
                     stat = 'identity', alpha=0.8) +
            scale_x_discrete(limits = levels) +
            scale_fill_manual(name = "", 
                              values = cbbPalette, labels = c("Present", "Missing")) +
            coord_flip() +
            labs(title = "Percentage of missing values", x =
                     'Variable', y = "% of missing values")
        
        percentage.plot
    })
    output$country<- renderUI({
        DOMINO<-getdata()
        list_countries<-DOMINO%>%
            select(COUNTRY)
        countries<-distinct(list_countries)
        
        selectInput('var', "Choose the Country of Missingness", countries)
    })
    output$missingno2<-renderPlot({
        DOMINO<-getdata()
        country<-input$var
        missing_data_Sw <- filter(DOMINO, COUNTRY == country) %>%
            gather(key = "Food_group", value = "val") %>%
            mutate(is.missing = is.na(val)) %>%
            group_by(Food_group, is.missing) %>%
            dplyr::summarise(n_missing_Sw = n()) %>%
            filter(is.missing == T) %>%
            select(-is.missing) %>%
            arrange(desc(n_missing_Sw)) 
        ###:https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
        
        missing_data_Sw$proportion_missing_Sw <- missing_data_Sw$n_missing_Sw / nrow(filter(DOMINO, COUNTRY == country))
        
        missing_data_Sw %>%
            ggplot() +
            geom_col(aes(x = Food_group, y = proportion_missing_Sw)) +
            labs(x = "Food group", y = "Proportion of missing values", title = "Proportion of missing values for chosen country by food group") +
            theme_bw()
    })
    
    
    output$test<-renderPrint({
        DOMINO_category<-getcategories()
        DOMINO_MF <- filter(DOMINO_category, CATEGORY == input$category)
        KW_MF <- kruskal.test(FFQ ~ COUNTRY, data = DOMINO_MF)
        KW_MF_p <- p.adjust(p = KW_MF$p.value, method = "holm")
        print(KW_MF)
        print(KW_MF_p)
        
    })
})
