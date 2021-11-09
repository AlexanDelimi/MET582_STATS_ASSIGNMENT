# Load Data sets
library(readr)
library(reshape2)
library(dplyr)
library(tidyverse)
setwd("~/Documents")

### Import data and select prelimary variables (beef to olive)
DOMINO <- read_csv(file = "FFQ_for_MET582.csv", col_names = TRUE, na = "NA") %>%
  select(COUNTRY:OLIVES) #%>%
  #head()

### Code missing values as NA and perform summary statistics
DOMINO[DOMINO == -9] <- NA
DOMINO[DOMINO == -8] <- NA
summary(DOMINO)

### Make summary plot showing food type by FFQ score, coloured by country.
DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")

food_plot <- ggplot (data = DOMINO_grouped, mapping = aes(x = FOOD, y = FFQ)) + 
  geom_col(aes(fill = COUNTRY)) +
  labs(title = "Cross-cultural Nutrional Differences") +
  xlab("Food Type") +
  ylab("FFQ Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
print(food_plot)

#### Group by major food groups by adding category column to DOMINO table
MF <- colnames(DOMINO)[2:28]
BSB <- colnames(DOMINO)[29:35]
C <- colnames(DOMINO)[36:39]
PRP <- colnames(DOMINO)[40:57]
DF <- colnames(DOMINO)[58:79]
OBV <- colnames(DOMINO)[80:88]
SwS <- colnames(DOMINO)[89:115]
SSS <- colnames(DOMINO)[116:128]
D <- colnames(DOMINO)[129:146]
F <- colnames(DOMINO)[147:163]
V <- colnames(DOMINO)[164:196]

DOMINO_category <- pivot_longer(DOMINO, -COUNTRY, names_to = "FOOD", values_to = "FFQ") %>%
mutate(category = case_when(FOOD %in% MF ~ "meat_fish",
                            FOOD %in% BSB ~ "bread_sav_biscuits",
                            FOOD %in% C ~ "cereals",
                            FOOD %in% PRP ~ "potatoes_rice_pasta",
                            FOOD %in% DF ~ "dairy_fats",
                            FOOD %in% OBV ~ "on_bread_veg",
                            FOOD %in% SwS ~ "sweets_snacks",
                            FOOD %in% SSS ~ "soups_sauces_spreads",
                            FOOD %in% D ~ "drinks",
                            FOOD %in% F ~ "fruits",
                            FOOD %in% V ~ "vegetables"))
                            
#### Create subsets of data for each category for summaries
DOMINO_meat_fish <- select(DOMINO, COUNTRY, BEEF:TROUT) 
DOMINO_bread_savoury_biscuits <- select(DOMINO, COUNTRY, WHTBREADSL:RYEBRD)
DOMINO_cereals <- select(DOMINO, COUNTRY, PORRIDGE:CEREALMUS) 
DOMINO_potatoes_rice_pasta <- select(DOMINO, COUNTRY, POTSPREP:BARLYGRO) 
DOMINO_dairy_fats <- select(DOMINO, COUNTRY, SICREAM:SOURMILK) 
DOMINO_on_bread_veg <- select(DOMINO, COUNTRY, BUTTER:VLOWSPREAD) 
DOMINO_sweets_snacks <- select(DOMINO, COUNTRY, SWBISCHO:JELLY) 
DOMINO_soups_sauces_spreads <- select(DOMINO, COUNTRY, VEGSOUP:COLDSOUP) 
DOMINO_drinks <- select(DOMINO, COUNTRY, TEA:TOMJUICE) 
DOMINO_fruits <- select(DOMINO, COUNTRY, APPLES:BLACKCURR)
DOMINO_vegetables <- select(DOMINO, COUNTRY, CARPUMP:OLIVES) 

#### Boxplot of food group consumption by country
foodgroup_boxplot <- function(df) {
  df %>%
    pivot_longer(-COUNTRY, names_to = "FOOD", values_to = "FFQ") %>%
    group_by(COUNTRY) %>%
    ggplot(aes(x = COUNTRY, y = FFQ, colour = COUNTRY)) +
    geom_boxplot() +
    theme_bw()
}
#### For example, meat and fish intake by country
print(foodgroup_boxplot(DOMINO_meat_fish) + labs(title= "Meat and fish intake by country",
                                                 y= "Intake score"))

#### Barplot of mean food group consumption by country
foodgroup_barplot <- function(df) {
  df %>%
    pivot_longer(-COUNTRY, names_to="FOOD", values_to="FFQ") %>%
    group_by(COUNTRY) %>%
    summarise(sum_val = sum(FFQ, na.rm = TRUE),
              mean_val = mean(FFQ, na.rm=TRUE),
              sd_val = sd(FFQ, na.rm = TRUE),
              median_val = median(FFQ)) %>%
    ggplot(aes(x = COUNTRY, y = mean_val, fill = COUNTRY)) +
    geom_col() +
    geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.2) +
    theme_bw()
}
#### For example, meat and fish mean intake score by country:
print(foodgroup_barplot(DOMINO_meat_fish) + labs(title="Mean meat and fish intake score by country",
                                              y= "Mean intake score"))


