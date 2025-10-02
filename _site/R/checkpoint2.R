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

# draw histogram showing counts of AANTAL (number of finds), in bins of 100
ggplot(feature_sums, aes(x=AANTAL)) + geom_histogram(bins = 100)

# draw boxplot showing counts of AANTAL (number of finds)
ggplot(feature_sums, aes(y=AANTAL)) + geom_boxplot()

# filter data to include only features with 100 or less finds
feature_sums_less_than_100 <- filter(feature_sums, AANTAL < 100)
# then plot histogram and boxplot again
ggplot(feature_sums_less_than_100, aes(x=AANTAL)) + geom_histogram(bins = 100)
ggplot(feature_sums_less_than_100, aes(y=AANTAL)) + geom_boxplot()

# plot weight (GEWICHT) for the features that contain less than 100 finds
ggplot(feature_sums_less_than_100, aes(x=GEWICHT)) + geom_histogram(bins = 100)
ggplot(feature_sums_less_than_100, aes(y=GEWICHT)) + geom_boxplot()

# boxplot per find category (ABR_ALG)
ggplot(finds_total, aes(x=ABR_ALG,y=AANTAL)) + geom_boxplot()

# barchart per more specific find category (INHOUD)
ggplot(finds_total, aes(x=INHOUD.y,y=AANTAL)) + geom_bar(stat="identity")

# number of finds per feature
ggplot(finds_total, aes(x=factor(SPOORNR),y=AANTAL)) + geom_bar(stat="identity")

# number of finds per feature, stacked bar based on find type (ABR_ALG)
ggplot(finds_total, aes(x=factor(SPOORNR),y=AANTAL, fill = ABR_ALG)) + geom_bar(stat="identity")

# number of finds per feature, separate bar charts based on find type (ABR_ALG)
ggplot(finds_total, aes(x=factor(SPOORNR),y=AANTAL)) + geom_bar(stat="identity") + facet_wrap(~ABR_ALG)

# histogram and boxplot showing distribution of weight over features
ggplot(feature_sums, aes(x=GEWICHT)) + geom_histogram(bins = 100)
ggplot(feature_sums, aes(y=GEWICHT)) + geom_boxplot()

# boxplot and bar chart of weight vs. find categories
ggplot(finds_total, aes(x=ABR_ALG,y=GEWICHT)) + geom_boxplot()
ggplot(finds_total, aes(x=ABR_ALG,y=GEWICHT)) + geom_bar(stat="identity")

# stacked bar chart of weight per feature, with find category as fill
ggplot(finds_total, aes(x=factor(SPOORNR),y=GEWICHT, fill = ABR_ALG)) + geom_bar(stat="identity")

# add find type to feature sums
feature_groups_type <- group_by(finds_total, SPOORNR, ABR_ALG) 
feature_sums_type <- summarise(feature_groups_type, AANTAL = sum(AANTAL), GEWICHT = sum(GEWICHT))
# plot relationship between weight and quantity, per feature. Colour per find type 
ggplot(feature_sums_type, aes(x=GEWICHT,y=AANTAL, colour = ABR_ALG)) + geom_point(alpha = 1/3)

# plot quantities per trench
ggplot(finds_total, aes(x=factor(PUTNR),y=AANTAL)) + geom_bar(stat="identity")
ggplot(finds_total, aes(x=factor(PUTNR),y=AANTAL, fill = ABR_ALG)) + geom_bar(stat="identity")
# plot weight per trench
ggplot(finds_total, aes(x=factor(PUTNR),y=GEWICHT)) + geom_bar(stat="identity")
ggplot(finds_total, aes(x=factor(PUTNR),y=GEWICHT, fill = ABR_ALG)) + geom_bar(stat="identity")

# fragmentation per feature
feature_sums_break <-  mutate(feature_sums, break_index = GEWICHT / AANTAL)
ggplot(feature_sums_break, aes(x=breek_index)) + geom_histogram(bins = 100)
feature_sums_break_idx100 <- filter(feature_sums_break, break_index < 100)
ggplot(feature_sums_idx100, aes(x=breek_index)) + geom_histogram(bins = 100)
ggplot(feature_sums_idx100, aes(x=breek_index)) + geom_density()
