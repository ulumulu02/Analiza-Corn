library(mice)
# library(dplyr)
# library(stringr)

#Macierz predykcji
all_variables <- colnames(dane)
n_variables <- length(all_variables)

Mlink <- matrix(0, n_variables, n_variables,
                dimnames = list(all_variables,all_variables))
rm(Mlink)
Mlink[c("Temperature", "Rainfall", "Soil_pH"),c("Country")] <- 1
Mlink["Soil_pH", c("Temperature","Rainfall")] <- 1
Mlink
rm(dane_imputation)
dane_imputation <- mice(dane, m = 20, maxit=30, predictorMatrix = Mlink, method = "pmm", seed=666, donors = 15)
dane_long <- complete(dane_imputation, action = "long", include = FALSE)

# 20 ramek danych
imputed_values <- complete(dane_imputation, "all")
stripplot(dane_imputation, Rainfall ~ Country)
stripplot(dane_imputation, Soil_pH ~ Country) #niebieskie kolka nasze a czerwone z bazy
stripplot(dane_imputation, Temperature ~ Country)
# Porównanie wykresów gęstości danych przed imputacja (niebieskie) + dane imputowane (czerwone)
mice::densityplot(dane_imputation, ~Soil_pH | Country, bw = 0.5)
