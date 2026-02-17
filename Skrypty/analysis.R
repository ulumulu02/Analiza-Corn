library(car)

# Tworzenie modelu dla kazdego z podzbioru danych,
dane <- dane %>% 
  mutate(Yield_2=Yield^2)

# Przykład: leveneTest(zmienna_zalezna ~ grupujaca, data = dane)
leveneTest(Yield^2 ~ Fertilizer_type, data = dane) # wariancje rowne miedzy grupami
leveneTest(Yield^2 ~ Variety, data = dane)#wairancje rowne miedzy grupami
# anova dla typu nawozu
anova1 <- aov(Yield_2 ~ Fertilizer_type, data= dane)
summary(anova1)
TukeyHSD(anova1)
# anova dla odmiany nawozy
anova2 <- aov(Yield_2 ~ Variety, data= dane)
summary(anova2)
TukeyHSD(anova2)

library(miceadds)

# Tworzenie modelu dla kazdego z podzbioru danych
fit <- with(
  dane_imputation,
  lm(Yield ~ Temperature + Rainfall + Soil_pH + Herbicide + Irrigation + Variety + Fertilizer_type)
)
pooled <- pool(fit)
summary(pooled, conf.int = TRUE)

confint(pooled, level=0.95)

# Diagnostyka
fits <- lapply(imputed_values, function(df) {
  lm(Yield ~ Temperature + Rainfall + Soil_pH + Herbicide + Irrigation + Variety + Fertilizer_type, data = df)
})
plot(fits[[1]])
summary(pool(fits))

# R^2 adjusted - średnia ze wszystkich R^2
r2_values <- sapply(fits, function(fit) summary(fit)$adj.r.squared)
mean_r2 <- mean(r2_values)
mean_r2
range(r2_values)

# F - średnia wartość ze wszystkich modeli
# Extract F-statistics
f_stats <- sapply(fits, function(fit) summary(fit)$fstatistic[1])
mean(f_stats)
range(f_stats)

# AIC i BIC ze wszystkich modeli
aic_values <- sapply(fits, AIC)
bic_values <- sapply(fits, BIC)

# średnie wartości AIC i BIC
mean(aic_values)
range(aic_values)
mean(bic_values)
range(bic_values)

# Shapiro-Wilk
shapiro_p <- sapply(fits, function(fit) {
  shapiro.test(residuals(fit))$p.value
})
shapiro_p
mean(shapiro_p)
range(shapiro_p)
sort(shapiro_p)

library(ggplot2)

# Wykres Q-Q plot
res <- residuals(fits[[6]])

ggplot(data.frame(res), aes(sample = res)) +
  stat_qq(size = 1.5) +
  stat_qq_line(linewidth = 0.7, color = "red") +
  labs(
    title = "Wykres kwantylowy",
    x = "Kwantyle teoretyczne",
    y = "Kwantyle empiryczne"
  ) +
  theme_minimal()

bp_pvalues <- sapply(fits, function(fit) {
  bptest(fit)$p.value
})

mean(bp_pvalues)
bp_pvalues
range(bp_pvalues)
install.packages("lmtest")
library(lmtest)
bptest()

# Wykres - hetero

plot_data <- data.frame(
  fitted = fitted(fits[[6]]),
  residuals = residuals(fits[[6]])
)
library(ggplot2)
ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Reszty vs wartości dopasowane",
    subtitle = "Yield ~ Temperature + Rainfall + Soil_pH + Herbicide + Irrigation + Variety + Fertilizer_type",
    x = "Wartości dopasowane",
    y = "Reszty"
  ) +
  theme_minimal(base_size = 14)

# VIFY
vifs <- lapply(fit$analyses, vif)
vifs_last <- lapply(vifs, function(x) x[, "GVIF^(1/(2*Df))"])
vifs_last
vifs_mat <- do.call(cbind, vifs_last)
mean_vif <- rowMeans(vifs_mat)
mean_vif
max_vif <- apply(vifs_mat, 1, max)
max_vif

###### Zredukowany model -----
# Diagnostyka
fits <- lapply(imputed_values, function(df) {
  lm(Yield ~ Temperature + Herbicide + Irrigation + Variety + Fertilizer_type, data = df)
})
plot(fits[[1]])
summary(pool(fits))

# R^2 adjusted - średnia ze wszystkich R^2
r2_values <- sapply(fits, function(fit) summary(fit)$adj.r.squared)
mean_r2 <- mean(r2_values)
mean_r2
range(r2_values)

# F - średnia wartość ze wszystkich modeli
# Extract F-statistics
f_stats <- sapply(fits, function(fit) summary(fit)$fstatistic[1])
mean(f_stats)
range(f_stats)

# AIC i BIC ze wszystkich modeli
aic_values <- sapply(fits, AIC)
bic_values <- sapply(fits, BIC)

# średnie wartości AIC i BIC
mean(aic_values)
range(aic_values)
mean(bic_values)
range(bic_values)

# Shapiro-Wilk
shapiro_p <- sapply(fits, function(fit) {
  shapiro.test(residuals(fit))$p.value
})
shapiro_p
mean(shapiro_p)
range(shapiro_p)
sort(shapiro_p)

library(ggplot2)

# Wykres Q-Q plot
res <- residuals(fits[[6]])

ggplot(data.frame(res), aes(sample = res)) +
  stat_qq(size = 1.5) +
  stat_qq_line(linewidth = 0.7, color = "red") +
  labs(
    title = "Wykres kwantylowy",
    x = "Kwantyle teoretyczne",
    y = "Kwantyle empiryczne"
  ) +
  theme_minimal()

bp_pvalues <- sapply(fits, function(fit) {
  bptest(fit)$p.value
})

mean(bp_pvalues)
bp_pvalues
range(bp_pvalues)
sort(bp_pvalues)
install.packages("lmtest")
library(lmtest)
bptest()
# # Model - 1 zmienna
# fit_2 <- with(
#   dane_imputation,
#   lm(Yield ~ Temperature + Soil_pH + Herbicide + Irrigation + Variety + Fertilizer_type)
# )
# pooled <- pool(fit_2)
# summary(pooled)
# # Diagnostyka
# fits_2 <- lapply(imputed_values, function(df) {
#   lm(Yield ~ Temperature + Soil_pH + Herbicide + Irrigation + Variety + Fertilizer_type, data = df)
# })
# # R^2 adjusted - średnia ze wszystkich R^2
# r2_values <- sapply(fits_2, function(fit) summary(fit)$adj.r.squared)
# mean_r2 <- mean(r2_values)
# mean_r2
# 
# # F - średnia wartość ze wszystkich modeli
# # Extract F-statistics
# f_stats <- sapply(fits_2, function(fit) summary(fit)$fstatistic[1])
# mean(f_stats)
# range(f_stats)
# 
# # AIC i BIC ze wszystkich modeli
# aic_values <- sapply(fits_2, AIC)
# bic_values <- sapply(fits_2, BIC)
# 
# # średnie wartości AIC i BIC
# mean(aic_values)
# range(aic_values)
# mean(bic_values)
# range(bic_values)

####### FINALNY #########
# Model - 2 zmienne

# Diagnostyka
# imputed_values$`1`$Temp_centered <- imputed_values$`1`$Temperature - mean(imputed_values$`1`$Temperature)
for(i in 1:20) {
  imputed_values[[i]]$Temp_centered <- 
    imputed_values[[i]]$Temperature - mean(imputed_values[[i]]$Temperature)
}
# Yield ~ Temp_centered + I(Temp_centered^2) + Herbicide + Irrigation + Variety + Fertilizer_type
fits_3 <- lapply(imputed_values, function(df) {
  lm(Yield ~ Temp_centered + I(Temp_centered^2) + Herbicide + Irrigation + Variety + Fertilizer_type, 
     data = df)
})
pooled_test <- pool(fits_3)
summary(pooled_test, conf.int = TRUE)

# R^2 adjusted - średnia ze wszystkich R^2
r2_values <- sapply(fits_3, function(fit) summary(fit)$adj.r.squared)
mean_r2 <- mean(r2_values)
range(r2_values)
mean_r2
summary(fits_3[[1]])
plot(fits_3[[1]], which = 2)
influencePlot(fits_3[[1]], id.method="identify")
# F - średnia wartość ze wszystkich modeli
# Extract F-statistics
f_stats <- sapply(fits_3, function(fit) summary(fit)$fstatistic[1])
mean(f_stats)
range(f_stats)

# AIC i BIC ze wszystkich modeli
aic_values <- sapply(fits_3, AIC)
bic_values <- sapply(fits_3, BIC)

# średnie wartości AIC i BIC
mean(aic_values)
range(aic_values)
mean(bic_values)
range(bic_values)

# Shapiro-Wilk
shapiro_p <- sapply(fits_3, function(fit) {
  shapiro.test(residuals(fit))$p.value
})
sort(shapiro_p, decreasing = TRUE)
mean(shapiro_p)
range(shapiro_p)
plot(fits_3[[19]])
# B pagan
library(lmtest)
bp_pvalues <- sapply(fits_3, function(fit) {
  bptest(fit)$p.value
})

mean(bp_pvalues)
bp_pvalues
range(bp_pvalues)
# Wykres - hetero
plot(fits_3[[6]], which = 1)
plot(fits_3[[3]])

plot_data <- data.frame(
  fitted = fitted(fits_3[[6]]),
  residuals = residuals(fits_3[[6]])
)
plot_data$residuals

library(ggplot2)
ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Reszty vs wartości dopasowane",
    subtitle = "Yield ~ Temp_centered + I(Temp_centered^2) + Herbicide + Irrigation + Variety + Fertilizer_type",
    x = "Wartości dopasowane",
    y = "Reszty"
  ) +
  theme_minimal(base_size = 14)
library(ggplot2)
res_3 <- residuals(fits_3[[6]])

ggplot(data.frame(res_3), aes(sample = res_3)) +
  stat_qq(size = 1.5) +
  stat_qq_line(linewidth = 0.7, color = "red") +
  labs(
    title = "Wykres kwantylowy",
    x = "Kwantyle teoretyczne",
    y = "Kwantyle empiryczne"
  ) +
  theme_minimal()
vif(pool(fits_3))
bp_pvalues <- sapply(fits, function(fit) {
  bptest(fit)$p.value
})

vifs <- lapply(fits_3, vif)
vifs_last <- lapply(vifs, function(x) x[, "GVIF^(1/(2*Df))"])
vifs_last
vifs_mat <- do.call(cbind, vifs_last)
mean_vif <- rowMeans(vifs_mat)
mean_vif
max_vif <- apply(vifs_mat, 1, max)
max_vif
e <- summary(pool(fits_3))
vif(fits_3[[1]])
# Optymalna temperatura
e$estimate[2]
e$estimate[3]
optimal_centered <- -(e$estimate[2]) / (2 * e$estimate[3])
optimal_centered
sum_temp <- 0
for(i in 1:20) {
  sum_temp <- sum_temp + mean(imputed_values[[i]]$Temperature)
}
mean_temp <- sum_temp/20
optimal_actual <- optimal_centered + mean_temp
optimal_actual
