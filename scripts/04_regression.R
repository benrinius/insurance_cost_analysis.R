source("scripts/02_clean_data.R")
library(tidyverse)

##########Prediktorer: age, smoker, bmi
#Baserat på resultaten från "initial analysis" verkar dessa 
#prediktorer viktigast.
#charges verkar öka med Rökning, ålder och bmi.
#####


#Enklare modell med basvariabler
model_1 <- lm(
  charges ~ age + smoker + bmi, 
  data = data_clean)

summary(model_1)
##########
#förklarar 54% av variationen.
#Bra startpunkt men lämnar mycket oförklarat
####


#Utökad modell med fler hälsofaktorer
model_2 <- lm(
  charges ~ age + smoker + bmi + chronic_condition + exercise_level, 
  data = data_clean)

summary(model_2)
##########
#Förklarar 70% av variationen, stor ökning!
#Kundens nuvarande hälsostaus är en väldigt stark predikator.
#####


#Utökad modell med fler hälsofaktorer + tidigare skador och claims
model_3 <- lm(
  charges ~ age + smoker + bmi + chronic_condition + exercise_level + 
  prior_accidents + prior_claims, data = data_clean)

summary(model_3)
##########
#Förklarar 73% av variationen, liten ökning
#Att inkludera kundens historik fångar upp unik information som inte
#tidigare fångats upp av ålder och hälsostatus
#####


#Modelljämförelse
compare_models <- tibble(
  Modell = c("Model 1 (Bas)", "Model 2 (Bas + Hälsa)", "Model 3 (Bas + Hälsa + Historik)"),
  `R-squared` = c(summary(model_1)$r.squared, summary(model_2)$r.squared, summary(model_3)$r.squared),
  `Adj R-squared` = c(summary(model_1)$adj.r.squared, summary(model_2)$adj.r.squared, summary(model_3)$adj.r.squared ),
  `Residual Error` = c(summary(model_1)$sigma, summary(model_2)$sigma, summary(model_3)$sigma),
)
print(compare_models)
##########
#Adj r-squared samt residual error har minskat i varje steg,
#model 3 är den mest kompletta modell för att förklara 
#variationen i charges
#####

model_3_diagnostics <- data_clean %>%
  mutate(
    fitted_value = fitted(model_3),
    residual = resid(model_3),
    model = "Modell 3 (Full)"
  )

model_1_diagnostics <- data_clean %>%
  mutate(
    fitted_value = fitted(model_1),
    residual = resid(model_1),
    model = "Modell 1 (Bas)"
  )


model_3_diagnostics %>%
  select(charges, fitted_value, residual) %>%
  slice_head(n = 10)


ggplot(model_3_diagnostics, aes(x = fitted_value, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  labs(
    title = "Residualer mot predikterade värden (Modell 3)",
    x = "Predikterad kostnad",
    y = "Residual (fel)"
  )
########
#Modellens fel är slumpade runt noll, viss ökad spridning
#när kostnaderna blir högre
#########


qqnorm(resid(model_3))
qqline(resid(model_3), col = "red")

##Visar på högerskevhet samt att modellen har svårt att fånga extremvärden


ggplot(model_3_diagnostics, aes(x = fitted_value, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red")+
  labs(
    title = "Faktisk kostnad mot predikterade värden (Modell 3)",
    x = "Predikterad kostnad",
    y = "Faktisk kostnad"
  )
#########
#Punkterna ligger nära linjen för de flesta kunder.
#Modellen funkar alltså bra för huvudmassan av datan.
#######

compare_model_viz <- bind_rows(model_1_diagnostics, model_3_diagnostics) %>%
  ggplot(aes(x = fitted_value, y = residual)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~model) +
  labs(title = "Jämförelse av residualer: Enkel vs Avancerad modell")

compare_model_viz
#######
#De extra variablerna i model 3 har bidragit till en bättre träffsäkerhet i modellen.
#####

