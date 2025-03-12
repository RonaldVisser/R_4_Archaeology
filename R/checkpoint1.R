# load libraries
library(dplyr)
library(ggplot2)
library(pastecs)

# force non scientific formatting of numbers
options(scipen = 999)
options(digits=3)

# read CSV files and store in variables
finds <- read.csv("data/VONDST.csv", row.names=NULL)
bag_contents <- read.csv("data/SPLITS2.csv")

# view top 10 rows of bag_contents
head(bag_contents, n=10)

# join 2 tables on column VONDSTNR
finds_total <- inner_join(finds, bag_contents, by ="VONDSTNR")

# display basic statistics for variable ‘finds_total’
summary(finds_total)

# group by feature number (SPOORNR) and sum number of finds (AANTAL) and weight (GEWICHT)
feature_groups <- group_by(finds_total, SPOORNR) 
feature_sums <- summarise(feature_groups, AANTAL = sum(AANTAL), GEWICHT = sum(GEWICHT))

# view stats on number of finds (AANTAL) and weight (GEWICHT)
stat.desc(feature_sums$AANTAL)
stat.desc(feature_sums$GEWICHT)