#######################
# Detta script används för att ladda in datasettet,
# Undersöka datasetet(datatyper? saknade värden? inkonsekvenser?)
######################

library(tidyverse)

#läser in datasetet
data_raw <- read.csv("data/insurance_costs.csv")

#Dataöversikt
view(data_raw)
glimpse(data_raw)
summary(data_raw)

#Räknar saknade värden per kolumn
colSums(is.na(data_raw))