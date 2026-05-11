#Laddar in städat dataset
source("scripts/02_clean_data.R")
library(tidyverse)

#Statisisk sammanfattning, charges
charges_summary <- data_clean %>%
  summarise(
    total_charges = sum(charges, na.rm = TRUE),
    avg_charges = mean(charges, na.rm = TRUE),
    median_charges = median(charges, na.rm = TRUE)
  ) %>%
  arrange(desc(total_charges))

charges_summary

##########Tolkning:
# Genomsnittet är högre än medianen, datan är något högerskev
#
#
#####

#Statistisk sammanfattning, charges by smoker
smoker_summary <- data_clean %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    mean_charge = mean(charges, na.rm = TRUE),
    median_charge = median(charges, na.rm = TRUE),
    sd_charge = sd(charges, na.rm = TRUE)
  )%>%
  #Andel
  mutate(share = count / sum(count))

###########Tolkning:
#Här ser vi en stor skillnad på genomsnitt och median. Rökare har betydligt
#högre charges än icke-rökare
#
######


#Histogram, charges
charges_hist_viz <-ggplot(data_clean, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Fördelning av försäkringskostnader",
    x = "Kostnad (charges)",
    y = "Antal kunder"
  )+
  theme_minimal()

charges_hist_viz

#####Tolkning:
#Bekräftar resultatet från den statiska sammanfattningen av charges.
#Högerskrev fördelning,de flesta kunder har låga/medelhöga kostnader.
#Finns vissa extremvärden.
#####

#Boxplot, charges per BMI-kategori
bmi_cat_box <- ggplot(data_clean, aes(x = bmi_cat, y = charges, fill = bmi_cat)) +
  geom_boxplot() +
  labs(
    title = "Kostnadsskillnader mellan BMI-kategorier",
    x = "BMI-kategori",
    y = "Kostnad (charges)"
  )+
  theme_minimal()

bmi_cat_box

##########Tolkning:
#Både spannet och topparna ökar för varje BMI-grupps ökning
#Det ser ut att finnas ett samband mellan BMI och charges
#Sambandet förklarar inte allt
##########



#Boxplot, charges per åldersgrupp
age_group_viz <- ggplot(data_clean, aes(x = age_group, y = charges, fill = age_group)) +
  geom_boxplot() +
  labs(
    title = "Kostnadsskillnader mellan åldersgrupper",
    x = "Åldersgrupp",
    y = "Kostnad (charges)"
  )+
  theme_minimal()

age_group_viz

#########Tolkning:
#Seniorer har högst kostnader i genomsnitt, därefter middle-aged och sedan young_adults.
#Spridningen följer samma mönster. Alla grupper har extremvärden.
#Boxarna överlappar, vi kan se att ålder påverkar kostnad men förklarar inte allt.
#####



#Scatterplot, Ålder, kostnad och rökning
age_smoke_viz <- ggplot(data_clean, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Samband mellan ålder och kostnad baserat på rökning",
    x = "Ålder",
    y = "Kostnad (charges)" 
  )+
  theme_minimal()

age_smoke_viz

##########Tolkning:
#Positiv trendlinje, ålder har ett samband med charges.
#Vi ser även att rökarna är fördelade högre upp än icke-rökarna.
#Rökstatus har alltså en starkare påverkan på charges jämfört med bara ålder. 
#####

