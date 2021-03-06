# Load Data sets
library(readr)
library(reshape2)
library(dplyr)
library(tidyverse)
library(stringr)
library(RColorBrewer)

setwd("D:/siobh/Documents/Uni/MSc/MET582/Coursework/Data Project")

### Import data and select prelimary variables (beef to olive)
DOMINO <- read_csv(file = "FFQ_for_MET582.csv", col_names = TRUE, na = "NA") %>%
  select(COUNTRY:OLIVES)
head(DOMINO)

### Code missing values as NA and perform summary statistics
DOMINO[DOMINO == -9] <- NA
DOMINO[DOMINO == -8] <- NA

### Add participant IDs
DOMINO$ID <- seq.int(nrow(DOMINO))
DOMINO %>%
  select(COUNTRY, ID, BEEF:OLIVES)
summary(DOMINO)

means <- DOMINO %>%
  select (-1) %>%
  map_dbl(mean)
sds <- DOMINO %>%
  select (-1) %>%
  map_dbl(sd)


### Make summary plot showing food type by FFQ score, coloured by country.
DOMINO_grouped <- pivot_longer(data = DOMINO, cols = BEEF:OLIVES, names_to = "FOOD", values_to = "FFQ")

food_plot <- ggplot (data = DOMINO_grouped, mapping = aes(x = FOOD, y = FFQ)) + 
  geom_col(aes(fill = COUNTRY)) +
  labs(title = "Cross-cultural Nutrional Differences") +
  xlab("Food Type") +
  ylab("FFQ Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  scale_fill_brewer(palette="Spectral")
print(food_plot)

#### Group by major food groups.
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

DOMINO_bind <- rbind(MeatFish, Bread, Cereals, PotRicPas, DairyFats, OnBreadVeg, SweetSnacks, SoupsSpreads, Drinks, Fruit, Veg)

DOMINO_cat <- DOMINO_bind %>%
  select(ID, COUNTRY, CATEGORY, FOOD, FFQ)

### Make summary plots showing food category by FFQ score, coloured by country.

cat_plot <- ggplot (data = DOMINO_cat, mapping = aes(x = CATEGORY, y = FFQ)) + 
  geom_col(aes(fill = COUNTRY)) +
  labs(title = "Cross-cultural Nutrional Differences by Food Category") +
  xlab("Food Category") +
  ylab("FFQ Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  scale_fill_brewer(palette="Spectral")
print(cat_plot)

cat_box <- ggplot(DOMINO_cat, aes(x=CATEGORY, y=FFQ, fill=COUNTRY)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  labs(title = "Mean Cross-cultural Nutrional Differences by Food Category") +
  xlab("Food Category") +
  ylab("Mean FFQ Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_brewer(palette="Spectral")
print(cat_box)

cat_violin <- ggplot(DOMINO_cat, aes(x=CATEGORY, y=FFQ, fill=COUNTRY)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun=mean, geom="point", shape=4, size=3,  colour="blue") +
  labs(title = "Mean Cross-cultural Nutrional Differences by Food Category") +
  xlab("Food Category") +
  ylab("Mean FFQ Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_brewer(palette="Spectral")
print(cat_violin)


