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
#bmi och annual_checkups innehåller saknade värden


#Samla kategoriska variabler
cols <- c("sex", "region", "smoker", "chronic_condition", "exercise_level", "plan_type")

#Undersök kategoriska variabler(antal unika värden, problem?)
for (col in cols){
  cat("\n---", col, "---\n")
  print(unique(data_raw[[col]]))
}

#Efter kontroll av stavning och mellanslag:
#identifierar problem i följande kategoriska variabler:
#
#region
#smoker
#plan_type

#kategorisk variabel med "tom" kategori/ kategori utan namn:
#exercise_level




