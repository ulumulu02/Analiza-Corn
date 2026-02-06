
# Tworzenie modelu dla kazdego z podzbioru danych,
fit <- with(
  dane_imputation,
  lm(Yield~ Temperature)
)
pooled <- pool(fit)
summary(pooled)