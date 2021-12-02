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
library(klaR)
library(GGally)
library(nortest)
library(ggpubr)
library(rstatix)
library(fmsb)
library(openxlsx)
library(XLConnect)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ = source for colour blind friendly palettes 

# Define server logic.
shinyServer(function(input, output) {
    
    
    
    getdata<-reactive({
        inFile <- input$file
        
        if (is.null(inFile))
            return(NULL)
        
        DOMINO <- read_csv(file = inFile$datapath, col_names = TRUE, na = "NA")
        DOMINO<-DOMINO%>%
          dplyr::select(-dateCompleted,-(OTHFOOD:FREQ5))
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
        #DOMINO<-DOMINO%>%mutate(SITUATION=ifelse(ID>30, "TRUE","FAlse"))
          
        return(DOMINO)
    })
    
    getcategories<-reactive({
        DOMINO<-getdata()
        MeatFish <- DOMINO %>%
            dplyr::select(input$groups, ID, BEEF:TROUT) %>%
            pivot_longer(cols = BEEF:TROUT, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "MEAT & FISH")
        
        Bread <- DOMINO %>%
            dplyr::select(input$groups, ID, WHTBREADSL:RYEBRD) %>%
            pivot_longer(cols = WHTBREADSL:RYEBRD, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "BREAD & BISCUITS")
        
        Cereals <- DOMINO %>%
            dplyr::select(input$groups, ID, PORRIDGE:CEREALMUS) %>%
            pivot_longer(cols = PORRIDGE:CEREALMUS, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "CEREALS")
        
        PotRicPas <- DOMINO %>%
            dplyr::select(input$groups, ID, POTSPREP:BARLYGRO) %>%
            pivot_longer(cols = POTSPREP:BARLYGRO, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "POTATOES, RICE & PASTA")
        
        DairyFats <- DOMINO %>%
            dplyr::select(input$groups, ID, SICREAM:SOURMILK) %>%
            pivot_longer(cols = SICREAM:SOURMILK, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "DAIRY & FATS")
        
        OnBreadVeg <- DOMINO %>%
            dplyr::select(input$groups, ID, BUTTER:VLOWSPREAD) %>%
            pivot_longer(cols = BUTTER:VLOWSPREAD, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "TOPPING")
        
        SweetSnacks <- DOMINO %>%
            dplyr::select(input$groups, ID, SWBISCHO:JELLY) %>%
            pivot_longer(cols = SWBISCHO:JELLY, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "SWEETS & SNACKS")
        
        SoupsSpreads <- DOMINO %>%
            dplyr::select(input$groups, ID, VEGSOUP:COLDSOUP) %>%
            pivot_longer(cols = VEGSOUP:COLDSOUP, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "SOUPS & SPREADS")
        
        Drinks <- DOMINO %>%
            dplyr::select(input$groups, ID, TEA:TOMJUICE) %>%
            pivot_longer(cols = TEA:TOMJUICE, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "DRINKS")
        
        Fruit <- DOMINO %>%
            dplyr::select(input$groups, ID, APPLES:BLACKCURR) %>%
            pivot_longer(cols = APPLES:BLACKCURR, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "FRUIT")
        
        Veg <- DOMINO %>%
            dplyr::select(input$groups, ID, CARPUMP:OLIVES) %>%
            pivot_longer(cols = CARPUMP:OLIVES, names_to = "FOOD", values_to = "FFQ") %>%
            add_column(CATEGORY = "VEG")
        
        
        # Create new table
        DOMINO_bind <- rbind(MeatFish, Bread, Cereals, PotRicPas, DairyFats, OnBreadVeg, SweetSnacks, SoupsSpreads, Drinks, Fruit, Veg)
        
        # Reorder
        DOMINO_cat <- DOMINO_bind %>%
            dplyr::select(ID, input$groups, CATEGORY, FOOD, FFQ)
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
        {   DOMINO_grouped <- pivot_longer(data = getdata(), cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
            ggplot (data = DOMINO_grouped, mapping = aes(x = FOOD, y = FFQ)) + 
                geom_col(aes_string(fill = input$groups)) +
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
            geom_col(aes_string(fill = input$groups)) +
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

     getsummary1 <-reactive({
         if(input$choice =='Individual Foods')
         {
             
         }
         else
         {DOMINO_grouped <-getcategories()
         ggplot(DOMINO_grouped, aes_string(x="CATEGORY", y="FFQ", fill = input$groups)) +
             geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
             geom_jitter(width = 0.2, alpha = 0.025) +
             labs(title = "Cross-cultural Nutrional Differences by Food Category") +
             xlab("Food Category") +
             ylab("Mean FFQ Score") +
             theme_minimal() +
             theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
             scale_fill_manual(values=cbbPalette)}
     })
     
     output$boxplot <- renderPlot({
         getsummary1()
         
     })

     getsummary2 <-reactive({
         if(input$choice =='Individual Foods')
         {
             
         }
         else
         {DOMINO_grouped <-getcategories()
         ggplot(DOMINO_grouped, aes_string(x="CATEGORY", y="FFQ", fill = input$groups)) +
             geom_violin(na.rm = TRUE, outlier.shape = NA) +
             geom_jitter(width = 0.2, alpha = 0.025) +
             labs(title = "Cross-cultural Nutrional Differences by Food Category") +
             xlab("Food Category") +
             ylab("Mean FFQ Score") +
             theme_minimal() +
             theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
             scale_fill_manual(values=cbbPalette)}
     })
     
     output$vioplot <- renderPlot({
         getsummary2()
         
     })
     output$group<-renderUI({
       DOMINO<-getdata()
       groups<-DOMINO%>%
         dplyr::select(-(BEEF:OLIVES))
       
       
       selectInput('groups', "Choose the groups", colnames(groups))
       
     })
      
    getpca<-reactive({
        if(input$choice =='Individual Foods')
        { DOMINO<-getdata()
        DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
        
        DOMINO_food <- DOMINO_grouped %>%
            na.omit() %>%
            pivot_wider(id_cols = input$groups:ID, names_from = FOOD, values_from = FFQ) %>%
            dplyr::select(BEEF:OLIVES)
        
        DOMINO_pca <- prcomp(DOMINO_food, center = TRUE, scale. = TRUE)
        DOMINO_countries <- as_vector(DOMINO%>%
                                          dplyr::select(input$groups))
        DOMINO_bp <- ggbiplot(DOMINO_pca, choices = c(input$Principal1,input$Principal2), ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette) #Comparing PC1 and PC2
        #DOMINO_bp2 <- ggbiplot(DOMINO_pca, choices = 1:3, ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette) #Comparing PC1 and PC3
        #DOMINO_bp2 <- ggbiplot(DOMINO_pca, choices = 2:3, ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette) #Comparing PC2 and PC3
        #These are the different pca plots
        }
        else{
            DOMINO<-getdata()
            DOMINO_wide <- getcategories() %>%
                na.omit() %>%
                dplyr::select(-FOOD) %>%
                pivot_wider(id_cols = ID:input$groups, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
                dplyr::select(3:13)
            DOMINO_cat_pca <- prcomp(DOMINO_wide, center = TRUE, scale. = TRUE)
            number<-ncol(DOMINO_cat_pca$rotation)
            DOMINO_countries <- as_vector(DOMINO%>%
                                              dplyr::select(input$groups))
            DOMINO_bp <- ggbiplot(DOMINO_cat_pca, choices = c(input$Principal1,input$Principal2), ellipse = TRUE,  groups = DOMINO_countries)+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + ggtitle("PCA Bioplot of Data") + scale_color_manual(values=cbbPalette)
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
            pivot_wider(id_cols = input$groups:ID, names_from = FOOD, values_from = FFQ) %>%
            dplyr::select(BEEF:OLIVES)
        
        DOMINO_pca <- prcomp(DOMINO_food, center = TRUE, scale. = TRUE)
        DOMINO_pca
        }
        else{
            DOMINO<-getdata()
            DOMINO_wide <- getcategories() %>%
                na.omit() %>%
                dplyr::select(-FOOD) %>%
                pivot_wider(id_cols = ID:input$groups, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
                dplyr::select(3:13)
            DOMINO_pca <- prcomp(DOMINO_wide, center = TRUE, scale. = TRUE)
            DOMINO_pca
        }
        
    })
    
   
    output$pca_summary<-renderPrint({pca_sum()
      })
    
    output$pca<-renderPlot({
        getpca()
    })
    
    summ<-reactive({
        if(input$choice =='Individual Foods')
        {summary(getdata())}
        else
            summary(DOMINO_wide <- getcategories() %>%
                        na.omit() %>%
                        dplyr::select(-FOOD) %>%
                        pivot_wider(id_cols = ID:input$groups, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
                        dplyr::select(3:13))
    })
    
    output$summary<-renderPrint({summ()})
    
    output$missingno<-renderPlot({
        DOMINO<-getdata()
        proportion_missing <- colSums(is.na(DOMINO)) / nrow(DOMINO)
        Food_group <- colnames(DOMINO) 
        missing_data_country <- tibble(Food_group, proportion_missing) %>%
            filter(Food_group != input$groups)
        
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
    
    no_of_pcs<-reactive({
      if(input$choice =='Individual Foods')
      { DOMINO<-getdata()
      DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
      
      DOMINO_food <- DOMINO_grouped %>%
        na.omit() %>%
        pivot_wider(id_cols = input$groups:ID, names_from = FOOD, values_from = FFQ) %>%
        dplyr::select(BEEF:OLIVES)
      
      
      number<-ncol(DOMINO_food)
      }
      else{
       
        DOMINO_wide <- getcategories()%>%
          na.omit() %>%
          dplyr::select(-FOOD) %>%
          pivot_wider(id_cols = ID:input$groups, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
          dplyr::select(3:13)
        number<-ncol(DOMINO_wide)
      }
      return(number)
    })
    
    output$pc1<-renderUI({
      number<-no_of_pcs()
      numericInput('Principal1', "Pick the Principal Component that will go on y axis," , 1, min = 1, max = number, step=1)
      
    })
    output$pc2<-renderUI({
      number<-no_of_pcs()
      numericInput('Principal2', "Pick the Principal Component that will go on y axis," , 2, min = 1, max = number, step=1)
      
    })
    output$country<- renderUI({
        DOMINO<-getdata()
        list_countries<-DOMINO%>%
            dplyr::select(input$groups)
        countries<-distinct(list_countries)
        
        selectInput('var', "Choose the Country of Missingness", countries)
    })
    output$missingno2<-renderPlot({
        DOMINO<-getdata()
        country<-input$var
        missing_data_Sw <- DOMINO%>%
          filter(!!as.symbol(input$groups) == country) %>%
            gather(key = "Food_group", value = "val") %>%
            mutate(is.missing = is.na(val)) %>%
            group_by(Food_group, is.missing) %>%
            dplyr::summarise(n_missing_Sw = n()) %>%
            filter(is.missing == T) %>%
            dplyr::select(-is.missing) %>%
            arrange(desc(n_missing_Sw)) 
        ###:https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
        
        missing_data_Sw$proportion_missing_Sw <- missing_data_Sw$n_missing_Sw / nrow(filter(DOMINO, input$groups == country))
        
        missing_data_Sw %>%
            ggplot() +
            geom_col(aes(x = Food_group, y = proportion_missing_Sw)) +
            labs(x = "Food group", y = "Proportion of missing values", title = "Proportion of missing values for chosen country by food group") +
            ylim(0,1)+  
            theme_bw()
    })
    
    
    output$test<-renderPrint({
        DOMINO_category<-getcategories()
        DOMINO_MF <- dplyr::filter(DOMINO_category, CATEGORY == input$category)
       
     
        KW_MF <- kruskal.test(FFQ ~ COUNTRY, data = DOMINO_MF)
        KW_MF_p <- p.adjust(p = KW_MF$p.value, method = "holm")
        print(KW_MF)
        #print("With Holm's adjustment:")
        #print(KW_MF_p)
        
    })
    
    getpcacomp1<-reactive({
      if(input$choice =='Individual Foods')
    { 
    DOMINO<-getdata()
    DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
    
    DOMINO_food <- DOMINO_grouped %>%
      na.omit() %>%
      pivot_wider(id_cols = input$groups:ID, names_from = FOOD, values_from = FFQ) %>%
      dplyr::select(BEEF:OLIVES)
    
    DOMINO_pca <- prcomp(DOMINO_food, center = TRUE, scale. = TRUE)
    
    
    loading_threshold <- sqrt(1/ncol(dplyr::select(getdata(), BEEF:OLIVES)))
    }
      else{
        DOMINO<-getdata()
        DOMINO_wide <- getcategories() %>%
          na.omit() %>%
          dplyr::select(-FOOD) %>%
          pivot_wider(id_cols = ID:input$groups, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
          dplyr::select(3:13)
        DOMINO_pca <- prcomp(DOMINO_wide, center = TRUE, scale. = TRUE)
        loading_threshold <- sqrt(1/13)
      }
      category_loadings <- as.data.frame(DOMINO_pca$rotation[,1:3])
      food_categories <- rownames(category_loadings)
      PCA_loadings <- cbind(food_categories, category_loadings)
      loading_intercepts <- data.frame(y = c(loading_threshold, -loading_threshold), type = factor(c(1, 1)))
      yaxis1<-paste0("PC",input$Principal1)
      
      pcanalysis1<-ggplot(PCA_loadings) +
        geom_col(aes_string(x = "food_categories", y = yaxis1)) +
        geom_hline(data = loading_intercepts, aes(yintercept = y, colour = type), linetype = "dashed") +
        scale_colour_manual(values = c("green"), 
                            labels = c("High"),
                            name = "Strength of association to PC2") +
        scale_x_discrete(guide = guide_axis(n.dodge = 2))+
        xlab("Food category") +
        ylab("Loading") +
        ylim(c(-0.7, 0.7)) +
        labs(title = paste(yaxis1,"variable loadings")) +
        theme(legend.position = "bottom")
      
      
      return(pcanalysis1)
      })
    
    getpcacomp2<-reactive({
      if(input$choice =='Individual Foods')
      { 
        DOMINO<-getdata()
        DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")
        
        DOMINO_food <- DOMINO_grouped %>%
          na.omit() %>%
          pivot_wider(id_cols = input$groups:ID, names_from = FOOD, values_from = FFQ) %>%
          dplyr::select(BEEF:OLIVES)
        
        DOMINO_pca <- prcomp(DOMINO_food, center = TRUE, scale. = TRUE)
        
        
        loading_threshold <- sqrt(1/ncol(dplyr::select(getdata(), BEEF:OLIVES)))
      }
      else{
        DOMINO<-getdata()
        DOMINO_wide <- getcategories() %>%
          na.omit() %>%
          dplyr::select(-FOOD) %>%
          pivot_wider(id_cols = ID:input$groups, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
          dplyr::select(3:13)
        DOMINO_pca <- prcomp(DOMINO_wide, center = TRUE, scale. = TRUE)
        loading_threshold <- sqrt(1/13)
        
      }
      category_loadings <- as.data.frame(DOMINO_pca$rotation[,1:3])
      food_categories <- rownames(category_loadings)
      PCA_loadings <- cbind(food_categories, category_loadings)
      loading_intercepts <- data.frame(y = c(loading_threshold, -loading_threshold), type = factor(c(1, 1)))
      yaxis2<-paste0("PC",input$Principal2)
      
      pcanalysis2<-ggplot(PCA_loadings) +
        geom_col(aes_string(x = "food_categories", y = yaxis2)) +
        geom_hline(data = loading_intercepts, aes(yintercept = y, colour = type), linetype = "dashed") +
        scale_colour_manual(values = c("green"), 
                            labels = c("High"),
                            name = paste("Strength of association to",paste0("PC",input$Principal1))) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2))+
        xlab("Food category") +
        ylab("Loading") +
        ylim(c(-0.7, 0.7)) +
        labs(title = paste(yaxis2,"variable loadings")) +
        theme(legend.position = "bottom")
     
      return(pcanalysis2)
    })
    
    output$PC1_plot<-renderPlot({
      getpcacomp1()
      
      
    })
    output$PC2_plot<-renderPlot({
      getpcacomp2()
      
      
    })
    getcategorycovar<-reactive({
      DOMINO_wide <- getcategories() %>%
        na.omit() %>%
        dplyr::select(-FOOD) %>%
        pivot_wider(id_cols = ID:input$groups, names_from = CATEGORY, values_from = FFQ, values_fn = sum) %>%
        dplyr::select(3:13)
      scatterplot_matrix <- DOMINO_wide
      
      # simpler visualisation:
      x<-ggcorr(scatterplot_matrix, label = TRUE, hjust = 0.9, size = 4,layout.exp = 2, method = c("all.obs", "spearman"))
      return(x)
    })
    
    getcategorycovar2<-reactive({
      scatterplot_matrix_MF<-getcategories()%>%
        dplyr::filter(DOMINO_category, CATEGORY == input$Covariance_columns)
     x<- ggcorr(scatterplot_matrix_MF, label = TRUE, hjust = 0.90, size= 3, layout.exp =5, method = c("all.obs", "spearman"))
      return(x)
    })
    output$cov1<-renderPlot(
      {
        getcategorycovar()
      }
    )
    output$cov2<-renderPlot({
      getcategorycovar2()
    })
    output$Hypothesis<-renderText({paste("Anderson-Darling test for normality used as DOMINO_category daily data contains more than 5000  values.", 

"H0: data are normally distributed",

"H1: data are not normally distributed")})
    output$normality<-renderPrint({
      DOMINO_category<-getcategories()
      ad.test(DOMINO_category$FFQ)
    })
    output$Result<-renderText({
      "p-value < 0.05, therefore accept the alternate hypothesis.

The data are not normally distributed so non-parametric statistical tests should be used for analyses."
    })
    output$KWHypothesis<-renderText({
      "Kruskal-Wallis tests used as the data is non-parametric and ordinal.
      
      H0: median food intake does not differ between countries for each food category
      
      H1: median food intake does differ between countries for each food category"
    })
    
    KW_boxplot<-reactive({
      DOMINO_category <- getcategories()
      wilcox_p <- DOMINO_category %>%
        group_by(CATEGORY) %>%
        wilcox_test(FFQ ~ COUNTRY, p.adjust.method = "holm") %>%
        add_significance("p.adj") %>%
        add_xy_position(x = "y_pos")
      
      MF_p_adj <- dplyr::filter(wilcox_p, CATEGORY == input$category) 
     
      boxdata<-DOMINO_category%>%
        dplyr::filter(CATEGORY==input$category)
      
      x<-ggboxplot(data =boxdata,  x = "COUNTRY", y = "FFQ", fill = "COUNTRY",
                xlab = "Country",
                ylab = "Daily FFQ score",
                title = paste("Daily FFQ score of" ,input$category,"by country")) +
        stat_compare_means(method = "kruskal.test", label.y = 12) +
        stat_pvalue_manual(MF_p_adj, xmin = "group1", xmax = "group2", label = "p.adj")
        return(x)
    })
    
    output$"p-Boxplots"<-renderPlot({
      KW_boxplot()
    })
    overall <-function(DOMINO1, df, dfcolumn){
      
      temp<-df%>%
        dplyr::select(dfcolumn)
      temp<-t(temp)
      colnames(temp)<-df$FOOD
      temp<-as_tibble(temp)
      temp_domino<-DOMINO1%>%
        dplyr::select(BEEF:OLIVES)
      temp<-temp%>%
        dplyr::slice(rep(1:n(),nrow(DOMINO1)))
      res<-temp*temp_domino
      return (res)
    }
    get_med_data<-reactive({
      df<-readWorksheetFromFile("UK Data.xlsx", sheet = 1)
      #return(df)
      
      DOMINO<-getdata()
      #Extracting nutritional information and multiplying by daily intake 
      KJ <- overall(DOMINO,df,'ENERGY..KJ.')
      KJ$sum <- rowSums(KJ, na.rm = TRUE)
      Veg <- overall(DOMINO,df, 'Vegetables')
      Veg$sum <- rowSums(Veg, na.rm = TRUE)
      Fruit <- overall(DOMINO,df,'Fruit')
      Fruit$sum <- rowSums(Fruit, na.rm = TRUE)
      W.Grain <- overall(DOMINO,df,'Wholegrains')
      W.Grain$sum <- rowSums(W.Grain, na.rm = TRUE)
      Dairy <- overall(DOMINO,df,'Dairy')
      Dairy$sum <- rowSums(Dairy, na.rm = TRUE)
      Protein <- overall(DOMINO,df,'PROTEIN..g.')
      Protein$sum <- rowSums(Protein, na.rm = TRUE)
      T.Fat <- overall(DOMINO,df,'Total.fat..g.')
      T.Fat$sum <- rowSums(T.Fat, na.rm = TRUE)
      Sodium <- overall(DOMINO,df,'sodium..mg.')
      Sodium$sum <- rowSums(Sodium, na.rm = TRUE)
      Sat.Fat <- overall(DOMINO,df,'Sat.fat..g.')
      Sat.Fat$sum <- rowSums(Sat.Fat, na.rm = TRUE)
      Fish <- overall(DOMINO,df,'Total.Fish')
      Fish$sum <- rowSums(Fish, na.rm = TRUE)
      R.Meat <- overall(DOMINO,df,'Red.meat')
      R.Meat$sum <- rowSums(R.Meat, na.rm = TRUE)
      #Nuts Seeds and Olives...Need help adding nuts column
      N.S.O <- as.data.frame(DOMINO$FLAXSEED)
      N.S.O$Olive <- DOMINO$OLIVES
      N.S.O$Peanut <- DOMINO$PEANUT
      N.S.O$Sum <- rowSums(N.S.O)
      #Legumes
      Legumes <- as.data.frame(DOMINO$PEAS)
      Legumes$Gbean <- DOMINO$GBEANS
      Legumes$Babeans <- DOMINO$BABEANS
      Legumes$Lentils <- DOMINO$LENTILS
      Legumes$Sum <- rowSums(Legumes)
      #Processed Meat
      P.Meat <- as.data.frame(DOMINO$HAM)
      P.Meat$Chorizo <- DOMINO$CHORIZO
      P.Meat$C.Beef <- DOMINO$CORNBEEF
      P.Meat$Sausage <- DOMINO$SAUSAGE
      P.Meat$M.Ball <- DOMINO$MEATBALLS
      P.Meat$Sum <- rowSums(P.Meat)
      #Potatoes
      Potato <- as.data.frame(DOMINO$POTSPREP)
      Potato$Plain <- DOMINO$POTSPLAIN
      Potato$Salad <- DOMINO$POTSALAD
      Potato$Cake <- DOMINO$POTCAKE
      Potato$croq <- DOMINO$CROQUETTE
      Potato$Crisps <- DOMINO$CRISPS
      Potato$Sum <- rowSums(Potato)
      #Greens and Beans 
      gb <- as.data.frame(DOMINO$BSPROUT)
      gb$Gsalad <- DOMINO$GSALAD
      gb$babean <- DOMINO$BABEANS
      gb$lentil <- DOMINO$LENTILS
      gb$spinach <- DOMINO$SPINACH
      gb$broccoli <- DOMINO$BROCCOLI
      gb$sprouts <- DOMINO$SPROUTS
      gb$cabbage <- DOMINO$CABBAGE
      gb$chincabbage <- DOMINO$CHINCABB
      gb$stemcab <- DOMINO$STEMCABB
      gb$sauerk <- DOMINO$SAUERK
      gb$aspar <- DOMINO$ASPAR
      gb$marrow <- DOMINO$MARROW
      gb$sum <- rowSums(gb)
      #Whole Fruit
      W.Fruit <- as.data.frame(DOMINO$APPLES)
      W.Fruit$Pear <- DOMINO$PEARS
      W.Fruit$orange <- DOMINO$ORANGES
      W.Fruit$grape.f <- DOMINO$GRAPEFRT
      W.Fruit$banana <- DOMINO$BANANAS
      W.Fruit$grape <- DOMINO$GRAPES
      W.Fruit$melon <- DOMINO$MELON
      W.Fruit$peach <- DOMINO$PEACH
      W.Fruit$strawb. <- DOMINO$STRAWB
      W.Fruit$Cherry <- DOMINO$CHERRIES
      W.Fruit$mango <- DOMINO$MANGO
      W.Fruit$blackc. <- DOMINO$BLACKCURR
      W.Fruit$sum <- rowSums(W.Fruit)
      #SEAFOOD PROTEIN HELP!!!
      # Grouping Mediteranian Diet
      m <- cbind(Fruit$sum, Veg$sum, W.Grain$sum, N.S.O$Sum, Dairy$sum, DOMINO$CHICKEN, Fish$sum, DOMINO$EGGS, Legumes$Sum, Potato$Sum, R.Meat$sum, P.Meat$Sum)
      
      med.diet <- as.data.frame(m)
      colnames(med.diet) <- c("Fruit", "Vegetables", "WholeGrain", "Nuts,Seeds,Olives", "Dairy", "WhiteMeat", "Fish", "Eggs", "Legumes", "Potato", "RedMeat", "ProcessedMeat")
      # Divide daily intake by recomended daily intake to give a "perfect score" of 0
      lapply(med.diet, as.numeric)
      med.diet$Fruit <- med.diet$Fruit/3
      med.diet$Vegetables <- med.diet$Vegetables/6
      med.diet$WholeGrain <- med.diet$WholeGrain/4.5
      med.diet$`Nuts,Seeds,Olives` <- med.diet$`Nuts,Seeds,Olives`/1.5
      med.diet$Dairy <- med.diet$Dairy/2
      med.diet$WhiteMeat <- med.diet$WhiteMeat/0.286
      med.diet$Fish <- med.diet$Fish/0.286
      med.diet$Eggs <- med.diet$Eggs/0.429
      med.diet$Legumes <- med.diet$Legumes/0.286
      med.diet$Potato <- med.diet$Potato/0.429
      med.diet$RedMeat <- med.diet$RedMeat/0.286
      med.diet$ProcessedMeat <- med.diet$ProcessedMeat/0.143
      x<-dplyr::select(DOMINO, input$groups)
      med.diet$Country <- as_vector(x)
      return(med.diet)})
    
      getmedpos<-reactive({
        med.diet<-get_med_data()
      # Separate out positive and negative med.diet categories 
      med.diet.pos <- as.data.frame (cbind(med.diet$Fruit, med.diet$Vegetables, med.diet$WholeGrain, med.diet$`Nuts,Seeds,Olives`, med.diet$Legumes, med.diet$Fish, med.diet$WhiteMeat, med.diet$Dairy))
      colnames(med.diet.pos) <- c("Fruit", "Vegetables", "WholeGrain", "Nuts,Seeds,Olives", "Legumes", "Fish", "WhiteMeat", "Dairy")
      med.diet.pos$Mean <- rowMeans(med.diet.pos)
      med.diet.pos$Country <-med.diet$Country
      return(med.diet.pos)
      })
        
      getmedneg<-reactive({
        
        med.diet<-get_med_data()
        med.diet.neg <- as.data.frame (cbind(med.diet$Potato, med.diet$RedMeat, med.diet$ProcessedMeat))
      colnames(med.diet.neg) <- c("Potato", "RedMeat", "ProcessedMeat")
      med.diet.neg$Mean <- rowMeans(med.diet.neg)
      med.diet.neg$Country <- med.diet$Country
      return(med.diet.neg)
      })
      
      getmedmeans<-reactive({
    
        med.diet<-get_med_data()
      med.diet_means <- med.diet %>% 
        dplyr::group_by(Country) %>%
        dplyr::summarise(across(everything(), mean))
 
      med.diet_means <- add_row(med.diet_means, "Fruit" = 0, "Vegetables" = 0,"WholeGrain"=0,  "Nuts,Seeds,Olives" = 0, "Dairy" = 0, "WhiteMeat" = 0, "Fish" = 0, "Eggs" = 0, "Legumes" = 0, "Potato" = 0, "RedMeat" = 0, "ProcessedMeat" = 0) %>%
        add_row("Fruit" = 7.5, "Vegetables" = 7.5, "WholeGrain"=7.5, "Nuts,Seeds,Olives" = 7.5, "Dairy" = 7.5, "WhiteMeat" = 7.5, "Fish" = 7.5, "Eggs" = 7.5, "Legumes" = 7.5, "Potato" = 7.5, "RedMeat" = 7.5, "ProcessedMeat" = 7.5)
      countries<-med.diet_means%>%
        dplyr::select(Country)
      countries<-head(countries,-2)
      y<-as_vector(distinct(countries))
      y<-append(y, "min")
      y<-append(y, "max")
      rownames(med.diet_means) <- y 
      med.diet_means <- dplyr::select(med.diet_means, -Country)
      rownames(med.diet_means) <- y
      
      return(med.diet_means)
    })
    med_radar_plot<-reactive({
      med.diet_means<-getmedmeans()
      countries<-head(row.names(med.diet_means),-2)
      z<-countries
      z<-append(z,"min",0)
      z<-append(z,"max",0)
      
      radarchart(med.diet_means[z, ],
                 cglty = 3, plty = 1, pcol = c("red", "blue", "green", "purple"))
      ## Add legend to radar plot
      n<-nrow(med.diet_means)-2
      df2 <- med.diet_means[c(1:n),] 
      rownames(df2) <- head(row.names(med.diet_means),-2)
      legend(x = "bottomright", legend = rownames(df2), horiz = FALSE,
             bty = "n", pch = 20 , col = c("red", "blue", "green", "purple"),
             text.col = "black", cex = 1, pt.cex = 1.5, title = input$groups)
    })
    testxl<-reactive({
      wb <- loadWorkbook("UK Data.xlsx")
      df <- readWorksheet(wb, sheet=1) 
      return(df)
    })
    output$Med_radar<-renderPlot(
      med_radar_plot()
    )
    
    pos<-reactive({
      med.diet.pos<-getmedpos()
      # Preparing Mediterranean data for graph
      med.pos.long <- med.diet.pos %>%
        pivot_longer(cols = Fruit:Dairy, names_to = "Categories", values_to = "Score")
      med.pos.graph <- ggplot (data = med.pos.long, mapping = aes(x = Country, y =Score)) + 
        geom_col(aes(fill = Categories), position = "dodge") +
        labs(title = "Positive Meditetanian Diet Intake") +
        xlab(input$groups) +
        ylab("Intake based on Perfect Score of 0") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
        scale_colour_manual(values=cbbPalette)
      return(med.pos.graph + geom_hline(yintercept = 1, size=2))
      
    })
  
    output$Positive<-renderPlot({
      pos()
})
    
    negs<-reactive({
      med.diet.neg<-getmedneg()
      med.neg.long <- med.diet.neg %>%
        pivot_longer(cols = Potato:ProcessedMeat, names_to = "Categories", values_to = "Score")
      # Negative intake graph
      med.neg.graph <- ggplot (data = med.neg.long, mapping = aes(x = Country, y = Score)) + 
        geom_col(aes(fill = Categories), position = "dodge") +
        labs(title = "Negative Meditetanian Diet Intake") +
        xlab(input$groups) +
        ylab("Intake based on Perfect Score of 0") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
        scale_colour_manual(values=cbbPalette)
      med.neg.graph + geom_hline(yintercept = 1, size=2)
      return(med.neg.graph+ geom_hline(yintercept = 1, size=2))
    })
    
    output$Negative<-renderPlot({
      negs()
    })
    chei<-reactive({
      df<-readWorksheetFromFile("UK Data.xlsx", sheet = 1)
      
      #return(df)
      
      DOMINO<-getdata()
      #Extracting nutritional information and multiplying by daily intake 
      KJ <- overall(DOMINO,df,'ENERGY..KJ.')
      KJ$sum <- rowSums(KJ, na.rm = TRUE)
      Veg <- overall(DOMINO,df, 'Vegetables')
      Veg$sum <- rowSums(Veg, na.rm = TRUE)
      Fruit <- overall(DOMINO,df,'Fruit')
      Fruit$sum <- rowSums(Fruit, na.rm = TRUE)
      W.Grain <- overall(DOMINO,df,'Wholegrains')
      W.Grain$sum <- rowSums(W.Grain, na.rm = TRUE)
      Dairy <- overall(DOMINO,df,'Dairy')
      Dairy$sum <- rowSums(Dairy, na.rm = TRUE)
      Protein <- overall(DOMINO,df,'PROTEIN..g.')
      Protein$sum <- rowSums(Protein, na.rm = TRUE)
      T.Fat <- overall(DOMINO,df,'Total.fat..g.')
      T.Fat$sum <- rowSums(T.Fat, na.rm = TRUE)
      Sodium <- overall(DOMINO,df,'sodium..mg.')
      Sodium$sum <- rowSums(Sodium, na.rm = TRUE)
      Sat.Fat <- overall(DOMINO,df,'Sat.fat..g.')
      Sat.Fat$sum <- rowSums(Sat.Fat, na.rm = TRUE)
      Fish <- overall(DOMINO,df,'Total.Fish')
      Fish$sum <- rowSums(Fish, na.rm = TRUE)
      R.Meat <- overall(DOMINO,df,'Red.meat')
      R.Meat$sum <- rowSums(R.Meat, na.rm = TRUE)
      #Nuts Seeds and Olives...Need help adding nuts column
      N.S.O <- as.data.frame(DOMINO$FLAXSEED)
      N.S.O$Olive <- DOMINO$OLIVES
      N.S.O$Peanut <- DOMINO$PEANUT
      N.S.O$Sum <- rowSums(N.S.O)
      #Legumes
      Legumes <- as.data.frame(DOMINO$PEAS)
      Legumes$Gbean <- DOMINO$GBEANS
      Legumes$Babeans <- DOMINO$BABEANS
      Legumes$Lentils <- DOMINO$LENTILS
      Legumes$Sum <- rowSums(Legumes)
      #Processed Meat
      P.Meat <- as.data.frame(DOMINO$HAM)
      P.Meat$Chorizo <- DOMINO$CHORIZO
      P.Meat$C.Beef <- DOMINO$CORNBEEF
      P.Meat$Sausage <- DOMINO$SAUSAGE
      P.Meat$M.Ball <- DOMINO$MEATBALLS
      P.Meat$Sum <- rowSums(P.Meat)
      #Potatoes
      Potato <- as.data.frame(DOMINO$POTSPREP)
      Potato$Plain <- DOMINO$POTSPLAIN
      Potato$Salad <- DOMINO$POTSALAD
      Potato$Cake <- DOMINO$POTCAKE
      Potato$croq <- DOMINO$CROQUETTE
      Potato$Crisps <- DOMINO$CRISPS
      Potato$Sum <- rowSums(Potato)
      #Greens and Beans 
      gb <- as.data.frame(DOMINO$BSPROUT)
      gb$Gsalad <- DOMINO$GSALAD
      gb$babean <- DOMINO$BABEANS
      gb$lentil <- DOMINO$LENTILS
      gb$spinach <- DOMINO$SPINACH
      gb$broccoli <- DOMINO$BROCCOLI
      gb$sprouts <- DOMINO$SPROUTS
      gb$cabbage <- DOMINO$CABBAGE
      gb$chincabbage <- DOMINO$CHINCABB
      gb$stemcab <- DOMINO$STEMCABB
      gb$sauerk <- DOMINO$SAUERK
      gb$aspar <- DOMINO$ASPAR
      gb$marrow <- DOMINO$MARROW
      gb$sum <- rowSums(gb)
      #Whole Fruit
      W.Fruit <- as.data.frame(DOMINO$APPLES)
      W.Fruit$Pear <- DOMINO$PEARS
      W.Fruit$orange <- DOMINO$ORANGES
      W.Fruit$grape.f <- DOMINO$GRAPEFRT
      W.Fruit$banana <- DOMINO$BANANAS
      W.Fruit$grape <- DOMINO$GRAPES
      W.Fruit$melon <- DOMINO$MELON
      W.Fruit$peach <- DOMINO$PEACH
      W.Fruit$strawb. <- DOMINO$STRAWB
      W.Fruit$Cherry <- DOMINO$CHERRIES
      W.Fruit$mango <- DOMINO$MANGO
      W.Fruit$blackc. <- DOMINO$BLACKCURR
      W.Fruit$sum <- rowSums(W.Fruit)
      h <- cbind(Veg$sum, gb$sum, Fruit$sum, W.Fruit$sum, W.Grain$sum, Dairy$sum, Protein$sum, T.Fat$sum, Sodium$sum, Sat.Fat$sum)
      Comparative.HEI.Categories <- as.data.frame(h)
      colnames(Comparative.HEI.Categories) <- c("Vegetables", "Greens&Beans", "Fruit", "WholeFruit", "WholeGrain", "Dairy", "Protein(g)", "TotalFat(g)", "Sodium(mg)", "SatFat(g)")
      #function to scale results 1-10
      fn <- function(x) x * 10/max(x, na.rm = TRUE)
      #applying sca;es
      Comparative.HEI.Categories <- data.frame(lapply(Comparative.HEI.Categories, fn))
      # Adding countries as identifier
      x<-dplyr::select(DOMINO, input$groups)
      Comparative.HEI.Categories$Country <- as_vector(x)
      ces_means <- Comparative.HEI.Categories %>% 
        group_by(Country) %>%
        dplyr::summarise(across(everything(), mean))
      ces_means_plot <- add_row(ces_means, "Vegetables" = 0, "Greens.Beans" = 0,  "Fruit" = 0, "WholeFruit" = 0, "WholeGrain" = 0, "Dairy" = 0, "Protein.g." = 0, "TotalFat.g." = 0, "Sodium.mg." = 0, "SatFat.g." = 0) %>%
        add_row("Vegetables" = 10, "Greens.Beans" = 10,  "Fruit" = 10, "WholeFruit" = 10, "WholeGrain" = 10, "Dairy" = 10, "Protein.g." = 10, "TotalFat.g." = 10, "Sodium.mg." = 10, "SatFat.g." = 10)
      countries<-ces_means%>%
        dplyr::select(Country)
      countries<-head(countries)
      y<-as_vector(countries)
      y<-append(y, "min")
      y<-append(y, "max")
      rownames(ces_means_plot) <- y
      ces_means_plot <- dplyr::select(ces_means_plot, -Country)
      rownames(ces_means_plot) <- y 
      z<-as_vector(countries)
      z<-append(z,"min",0)
      z<-append(z,"max",0)
      radarchart(ces_means_plot[z, ],
                 cglty = 3, plty = 1, pcol = c("red", "blue", "green", "purple"), axistype = 2,
                 title = paste("Mean Comparative Eating Score by",input$groups))
      ## Add legend to radar plot
      n<-nrow(ces_means_plot)-2
      df2 <- ces_means_plot[c(1:n),] 
      rownames(df2) <-  as_vector(countries)
      legend(x = "bottomright", legend = rownames(df2), horiz = FALSE,
             bty = "n", pch = 20 , col = c("red", "blue", "green", "purple"),
             text.col = "black", cex = 1, pt.cex = 1.5, title = input$groups)
    })
    output$CHEI_radar<-renderPlot({chei()})
})