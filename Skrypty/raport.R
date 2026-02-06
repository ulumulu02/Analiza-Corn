###################statysyki opisowe ###########################
# Statystyki opisowe po imputacji
# Lista zmiennych ciągłych do analizy
zmienne_ciagle <- c("Rainfall", "Temperature", "Soil_pH", "Yield")

statystyki_ciagle <- dane_long %>%
  # Wybieramy tylko interesujące nas kolumny + numer imputacji
  select(.imp, all_of(zmienne_ciagle)) %>%
  # Zmieniamy format na długi, żeby łatwiej liczyć dla wszystkich zmiennych naraz
  tidyr::pivot_longer(cols = -c(.imp), names_to = "Zmienna", values_to = "Wartosc") %>%
  # Grupujemy po numerze imputacji i zmiennej
  group_by(.imp, Zmienna) %>%
  # Liczymy statystyki dla każdego z 20 zestawów OSOBNO
  summarise(
    Srednia = mean(Wartosc, na.rm = TRUE),
    Mediana = median(Wartosc, na.rm = TRUE),
    Q1 = quantile(Wartosc, 0.25, na.rm = TRUE),
    Q3 = quantile(Wartosc, 0.75, na.rm = TRUE),
    Min = min(Wartosc, na.rm = TRUE),
    Max = max(Wartosc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Teraz uśredniamy wyniki z 20 imputacji (Pool results)
  group_by(Zmienna) %>%
  summarise(
    Srednia_Pooled = mean(Srednia),
    Mediana_Pooled = mean(Mediana),
    Q1_Pooled = mean(Q1),
    Q3_Pooled = mean(Q3),
    Min_Pooled = mean(Min),
    Max_Pooled = mean(Max)
  )

# Wyświetlenie wyniku
print(statystyki_ciagle)


#STATYSTYKI KATEGORYCZNE
# Funkcja pomocnicza do liczenia statystyk dla jednej zmiennej kategorycznej
licz_kategorie <- function(data, nazwa_kolumny) {
  data %>%
    group_by(.imp, !!sym(nazwa_kolumny)) %>%
    summarise(n = n(), .groups = 'drop_last') %>%
    mutate(procent = n / sum(n) * 100) %>%
    group_by(!!sym(nazwa_kolumny)) %>% # Grupujemy po samej kategorii (np. Francja)
    summarise(
      Zmienna = nazwa_kolumny,
      n_srednie = mean(n),       # Średnia liczebność z 20 imputacji
      procent_sredni = mean(procent) # Średni procent
    ) %>%
    rename(Kategoria = !!sym(nazwa_kolumny))
}

# Obliczenia dla poszczególnych zmiennych
stat_kraj <- licz_kategorie(dane_long, "Country")
stat_odmiana <- licz_kategorie(dane_long, "Variety")
stat_nawoz <- licz_kategorie(dane_long, "Fertilizer_type")

# Dla zmiennych 0/1 (Herbicyd, Nawadnianie) warto zamienić je na factor lub napisy, żeby ładnie wyglądały
dane_long_cat <- dane_long %>%
  mutate(
    Herbicide = ifelse(Herbicide == 1, "Tak", "Nie"),
    Irrigation = ifelse(Irrigation == 1, "Tak", "Nie")
  )

stat_herbicyd <- licz_kategorie(dane_long_cat, "Herbicide")
stat_nawadnianie <- licz_kategorie(dane_long_cat, "Irrigation")

# Połączenie wszystkiego w jedną tabelę
tabela_kategoryczna <- bind_rows(stat_kraj, stat_odmiana, stat_nawoz, stat_herbicyd, stat_nawadnianie)

# Wyświetlenie wyniku
print(tabela_kategoryczna)
