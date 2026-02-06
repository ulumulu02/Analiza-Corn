install.packages("mice")
library(dplyr)
library(mice)
library(ggplot2)
corn <- read.csv("corn_OC_FullData.txt", sep = ";")
colSums(is.na(corn))

corn_imputation$data

# corn <- corn %>% mutate(Country = if_else(Number == 265, "Greece" , Country))

corn_imputation$data %>% 
  filter(Country == "Greece") %>%
  ggplot(aes(Temperature)) + 
  geom_histogram()

corn %>% 
  filter(Country == "Greece") %>%
  ggplot(aes(Temperature)) + 
  geom_histogram()
rm(list=ls())
corn$Country <- factor(corn$Country)
# Macierz predykcji
all_variables <- colnames(corn)
n_variables <- length(all_variables)

Mlink <- matrix(0, n_variables, n_variables,
                dimnames = list(all_variables,all_variables))

Mlink[c("Temperature", "Rainfall", "Soil_pH"),c("Country")] <- 1
Mlink

corn_imputation <- mice(corn, m = 5, maxit=50, predictorMatrix = Mlink, method = "pmm")

stripplot(corn_imputation, Temperature ~ Country)

# Tworzenie modelu dla kazdego z podzbioru danych
fit <- with(
  corn_imputation,
  lm("tu formuła naszego modelu")
)
pooled <- pool(fit)
summary(pooled)

19/300
