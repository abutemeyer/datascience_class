#Load data
library(rio)
inequality_data = import("inequality.xlsx", which = 1)
# The data is cross-sectional, as it covers a specific time (2015). Panel would be a range (ex: 2000-2015)

#Demonstrate all data is from 2015, thus cross-sectional
print(inequality_data$year)

# Subset Gini for Denmark and Sweden
inequality_datasubS <- subset(inequality_data, country == c("Sweden"), select = inequality_gini)
inequality_datasubD <- subset(inequality_data, country == c("Denmark"), select = inequality_gini)

# Subset Gini for Brazil
inequality_datasubBR <- subset(inequality_data, country == c("Brazil"), select = inequality_gini)

#6 It's better to have lower scores

#Quick peak at dataframe
head(inequality_data)

#Create accent.remove function 
remove.accents <- function(s) {
  # 1 character substitutions
  old1 <- "ú"
  new1 <- "u"
  s1 <- chartr(old1, new1, s)}

#Fix accents
inequality_data$country = remove.accents(inequality_data$country)

#Check accent fix
head(inequality_data)

#Sort data by lowest gini scores
inequality_data = inequality_data[order(inequality_data$inequality_gini),]

#Check new top 5 countries
head(inequality_data)

#Mean of inequality_gini = 36.81375
mean(inequality_data$inequality_gini, na.rm = TRUE)

#Create 2 dummy variables high_inequality and low_inequality with ifelse
inequality_data$high_inequality <- ifelse(inequality_data$inequality_gini > 36.81375, 1, 0)
inequality_data$low_inequality <- ifelse(inequality_data$inequality_gini < 36.81375, 0, 1)

#Cross tab
library(doBy)
summaryBy(high_inequality ~ low_inequality, data=inequality_data, FUN=c(mean,length))

# Actor vector and print w/ for loop
actors <- c('The World Bank', 'African Development Bank', 'Bill and Melinda Gates Foundation') 

for ( i in actors){
  print(i) }

#WDI indicator - Poverty gap at $1.90 a day, links to intensity of poverty
library(WDI)
poverty_gap = WDI(country = "all",
indicator = c("SI.POV.GAPS"), start = 2015, end = 2015, extra = FALSE, cache = NULL)

#Renaming variable
library(data.table) 
setnames(poverty_gap,"SI.POV.GAPS", "Poverty_Gap")

#Merge data
library(magrittr)
library(tidyverse)
merged_data = left_join(x=inequality_data,
                        y=poverty_gap,
                        by =c("iso2c", "year"))

merged_data = subset(merged_data, select = -country.y )
setnames(merged_data,"country.x", "country")

#Remove missing data based on inequality_gini and poverty gap
merged_data <- na.omit(merged_data, select=c("inequality_gini", "Poverty_Gap"))

#Inequality gini greater than 30
data_greater_30 <-
  merged_data %>% 
  dplyr::filter(!(inequality_gini<30))

#Count "ai"
count(data_greater_30, var = "ai")

#Sum gini
sapply(data_greater_30, sum)

#Label df
library(labelled)
var_label(merged_data) <- list(`country` = "Country",
                               `year` = "year",
                               `inequality_gini` = "Inequality Gini",
                               `high_inequality` = "High Inequality",
                               `iso2c`= "ISO-2 Country Code",
                               `low_inequality` = "Low Inequality",
                               `Poverty_Gap` = "Poverty Gap")

# save the dataset in Stata format with the labels
library(rio)
export(merged_data, file = "final_data.dta")






