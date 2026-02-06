library(dplyr)
library(stringr)

# Definicja dopuszczalnych wartości
valid_countries <- c("France", "Brazil", "Greece", "Ireland", "Portugal")
valid_varieties <- c("V1", "V2", "V3", "V4")
valid_fertilizers <- c("F1", "F2", "F3", "F4")

# Funkcja pomocnicza do czyszczenia tekstów (wielkie litery, usuwanie spacji/myślników)
clean_text_code <- function(x) {
  x <- toupper(x)
  x <- gsub("[ -]", "", x)
  return(x)
}

dane <- dane %>%
  # --- KRAJ (Country) ---
  mutate(Country = recode(Country, 
                          'Ierland' = 'Ireland', 
                          'Greese' = 'Greece', 
                          'Brasil' = 'Brazil')) %>%
  mutate(Country = str_trim(Country)) %>%
  mutate(Country = ifelse(Country %in% valid_countries, Country, NA)) %>%
  # --- ODMIANA (Variety) ---
  mutate(Variety = clean_text_code(Variety)) %>%
  mutate(Variety = ifelse(Variety %in% valid_varieties, Variety, NA)) %>%
  # --- NAWÓZ (Fertilizer_type) ---
  mutate(Fertilizer_type = clean_text_code(Fertilizer_type)) %>%
  mutate(Fertilizer_type = ifelse(Fertilizer_type %in% valid_fertilizers, Fertilizer_type, NA)) %>%
  # --- HERBICYD i NAWADNIANIE ---
  mutate(Herbicide = ifelse(Herbicide %in% c(0, 1), Herbicide, NA),
         Irrigation = ifelse(Irrigation %in% c(0, 1), Irrigation, NA))

# - Opady/Temperatura: > Q3 + 3*IQR to błąd (NA). 
# (Pomiędzy 1.5 a 3 to "weryfikacja ręczna" - tu zostawiamy, usuwamy tylko pewne błędy)
# - Opady: < 0 to błąd.
get_iqr_limits <- function(x, multiplier_upper = 3, multiplier_lower = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lower <- q1 - multiplier_lower * iqr_val
  upper <- q3 + multiplier_upper * iqr_val
  return(list(lower = lower, upper = upper))
}

# A. Opady i Temperatura - z podziałem na kraje
dane <- dane %>%
  group_by(Country) %>%
  mutate(
    # Opady (Rainfall)
    Rainfall = {
      limits <- get_iqr_limits(Rainfall, multiplier_upper = 3)
      ifelse(Rainfall < 0 | Rainfall > limits$upper, NA, Rainfall)
    },
    # Temperatura (Temperature)
    Temperature = {
      limits <- get_iqr_limits(Temperature, multiplier_upper = 3, multiplier_lower = 3) 
      ifelse(Temperature > limits$upper, NA, Temperature) 
    }
  ) %>%
  ungroup()

# B. pH gleby (Soil_pH) - [3.0, 9.0]
dane <- dane %>%
  mutate(Soil_pH = ifelse(Soil_pH >= 3.0 & Soil_pH <= 9.0, Soil_pH, NA))

dane$Country <- factor(dane$Country)
dane$Variety <- factor(dane$Variety)
dane$Fertilizer_type <- factor(dane$Fertilizer_type)
dane$Herbicide <- factor(dane$Herbicide)
dane$Irrigation <- factor(dane$Irrigation)