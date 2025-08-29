# Autorzy: Szymon Wierzejski i Marcin Wojciechowski

install.packages("ggplot2")
install.packages("e1071")
install.packages("fitdistrplus")
install.packages("nortest")

library(ggplot2)
library(e1071)
library(fitdistrplus)

install.packages("cowplot")

# Ustawienie lokalizacij konsoli
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()

# =========================================
#
#       Regresja liniowa
#
# =========================================

b11 <- read.csv("11b_d.csv")

kurs_zamkniecia <- b11$Zamkniecie
kurs_data <- as.Date(b11$Data)


wmg_us_d <- read.csv("wmg_us_d.csv")

kurs_zamkniecia_wmg <- wmg_us_d$Zamkniecie
kurs_data_wmg <- as.Date(wmg_us_d$Data)

# Rekreacja tabeli csv ale tylko z dwóch kolumn
temp_11b <- data.frame(kurs_zamkniecia, kurs_data)
colnames(temp_11b) <- c("kurs", "data")
temp_wmg <- data.frame(kurs_zamkniecia_wmg, kurs_data_wmg)
colnames(temp_wmg) <- c("kurs", "data")

# Spajanie obu spółek na podstawie dat
merged_kursy <- merge(temp_11b, temp_wmg, by = "data")
#merged_kursy

# Wykonywanie operacji na przygotowanych i spojonych danych
m_log_kursu_11b <- log(merged_kursy$kurs.x)
m_log_kursu_wmg <- log(merged_kursy$kurs.y)

m_diff_11b <- diff(m_log_kursu_11b)
m_diff_wmg <- diff(m_log_kursu_wmg)

# Sprawdzanie czy daty w naszych spółkach się pokrywają
identical(kurs_data, kurs_data_wmg)
which(kurs_data != kurs_data_wmg) # bardzo się nie pokrywają

#-----------------------------------------
# Zad 1
# Prosta regresji
#-----------------------------------------

diff_df <- data.frame(diff_11b = m_diff_11b, diff_wmg = m_diff_wmg)


diffs <- ggplot(diff_df, aes(x = diff_11b, y = diff_wmg)) +
  geom_point(colour = "blue", size = .6) +
  ggtitle("Porównanie logarytmicznych zwrotów spółki 11B i WMG.US") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))

diffs
ggsave(
  "img/reg_porownanie.png",
  plot = diffs,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

# estymatory wspolczynnikow
beta_1 <- cov(m_diff_11b, m_diff_wmg) / var(m_diff_11b)
beta_0 <- mean(m_diff_wmg) - mean(m_diff_11b) * beta_1

beta_1
beta_0

# linia regresji na  wykresie (diff_wmg=-0.0008+0.2738 * diff_11b)
regresja <- ggplot(diff_df, aes(x = diff_11b, y = diff_wmg)) +
  geom_point(colour = "blue", size = .6) +
  geom_abline(intercept = beta_0, slope = beta_1, color = "red", size = .6) +
  ggtitle("Regresja liniowa dla spółek 11B i WMG.US") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))
regresja
ggsave(
  "img/reg_regresja.png",
  plot = regresja,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)


lm_diffs <- lm(diff_wmg ~ diff_11b, data = diff_df)
lm_diffs




# reszty (residuals)
residual <- lm_diffs$residuals

# histogram i qq-plot
png(
  "img/reg_hist_i_qq_ver.png",
  width = 7,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 1))
hist(residual, xlab = "Reszta", ylab = "Częstotliwość", main = "Histogram reszt")

qqnorm(residual)
qqline(residual, col = 2)
dev.off()


# trzy testy normalności rozkladu
library(nortest)
library(fitdistrplus)

fn <- fitdist(residual, "norm")
m <- fn$estimate[1]
s <- fn$estimate[2]

fn

# MC z wykorzystaniem statystyki KS/najslabszy
ks.test(residual, "pnorm", m, s, exact = TRUE)
ad.test(residual)

# RSE - blad standardowy reszt (Residual standard error)
rse <- sqrt(sum(residual^2) / (length(m_diff_11b) - 2))
rse

# suma kwadratów reszt
ssr <- sum(lm_diffs$residuals^2)

# wartość oczekiwana kwadratów E(x^2)
sst <- sum((m_diff_wmg - mean(m_diff_wmg))^2)

# Współczynnik determinacji
r_squared <- 1 - (ssr / sst)
r_squared


#-----------------------------------------
# Przeprowadzamy testy na istotność wspóczynników b0, b1
# Wartość statystyki testowej oraz p-value
#-----------------------------------------

# Wydzielamy estymatory współczynników i błędy standardowe
coefficients <- coef(lm_diffs)
standard_errors <- sqrt(diag(vcov(lm_diffs)))

# statystyka t
t_statistics <- coefficients / standard_errors

# Wydzielamy stopnie swobody
df <- lm_diffs$df.residual

# Obliczamy oba p-value
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)


# tworzenie ładnej tabeli wyników:
# esytmatory współczynników, błędy standardowe, stystyki t i p-values
result_summary <- data.frame(
  Coefficients = coefficients,
  Std_Errors = standard_errors,
  T_Statistics = t_statistics,
  P_Values = p_values
)
result_summary

rse
r_squared


# sprawdzamy wyniki z naszymi obliczeniami
lm_diffs_sum <- summary(lm_diffs)
lm_diffs_sum



#-----------------------------------------
# regresja dla uproszczonego modelu (b0=0)
#-----------------------------------------


lm_diffsv2 <- lm(diff_wmg ~ diff_11b - 1, data = diff_df)
lm_diffsv2

# dla uproszczenia kodu używamy summary,
# jak obliczyć wartości bez summary zaprezentowaliśmy powyżej
lm_diffs_sumv2 <- summary(lm_diffsv2)
lm_diffs_sumv2

# porównanie linii regresji pełnego modelu z modelem uproszczonym na  wykresie
# (diff_wmg_uproszczony=0.2733 * diff_11b)
mod_reg <- ggplot(diff_df, aes(x = diff_11b, y = diff_wmg)) +
  geom_point(color="black", size = .6) +
  geom_abline(aes(intercept = beta_0, slope = beta_1, color = "Pełny model"), size = .5) +
  geom_abline(aes(intercept = 0, slope = lm_diffsv2$coefficients, color = "Model uproszczony"), size = .5) +
  ggtitle("Porównanie modelu pełnego i uproszczonego") +
  scale_color_manual(name = "", values = c("blue", "green")) +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        legend.position = "bottom",
        legend.key.width = unit(.3,"cm"),
        legend.key.height = unit(.3,"cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size=6))

ggsave(
  "img/reg_porownanie_regresji.png",
  plot = mod_reg,
  width = 12,
  height = 12,
  units = "cm",
  dpi = 480
)


#-----------------------------------------
# Predykcje obu modelów
#-----------------------------------------
mean_11b <- mean(m_diff_11b)
mean_11b
# Predykcja pełnego modelu
prediction <- beta_0 + beta_1 * mean_11b
prediction
temp_df <- data.frame(diff_11b = mean_11b, diff_wmg = prediction)

# Predykcja modelu uproszczonego
prediction_0 <- 0 + beta_1 * mean_11b
temp_df_0 <- data.frame(diff_11b = mean_11b, diff_wmg = prediction_0)
prediction_0

# naniesienie predykcji dla pełnego modelu na porównanie log zwrotów
pred_full_mod <- ggplot() +
  geom_point(data = diff_df, aes(x = diff_11b, y = diff_wmg), color = "black", size = .6) +
  geom_abline(aes(intercept = beta_0, slope = beta_1), color = "darkgreen", size = .5) +
  geom_point(data = temp_df, aes(x = diff_11b, y = diff_wmg), fill = "green", size = 1.2, colour = "darkgreen", shape = 23, stroke = 1.2) +
  ggtitle("Predykcja pełnego modelu") +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))

ggsave(
  "img/reg_pred_pelny.png",
  plot = pred_full_mod,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)




pred_comp <- ggplot() +
  geom_point(data = diff_df, aes(x = diff_11b, y = diff_wmg), color = "black", size = .6) +
  geom_abline(aes(intercept = beta_0, slope = beta_1), color = "darkgreen", size = .5) +
  geom_point(data = temp_df, aes(x = diff_11b, y = diff_wmg, fill = "Predykcja modelu pełnego"), size = 1.2, shape = 23, stroke = .8) +
  geom_abline(aes(intercept = 0, slope = lm_diffsv2$coefficients), color = "darkred", size = .5) +
  geom_point(data = temp_df_0, aes(x = diff_11b, y = diff_wmg, fill = "Predykcja modelu uproszczonego"), size = 1.2, shape = 23, stroke = .8) +
  ggtitle("Porównanie predykcji modelów; pełnego i uproszczonego") +
  scale_fill_manual(name = "", values = c("green",  "red")) +
  theme(plot.title = element_text(hjust = 0.5, size=10),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        legend.position = "bottom",
        legend.key.width = unit(.3,"cm"),
        legend.key.height = unit(.3,"cm"),
        legend.margin = margin(t = -1, r = 0, b = 0, l = 0),
        legend.text = element_text(size=5)) +
  xlim(c(-0.02, 0.02)) +
  ylim(c(-0.02, 0.02))


ggsave(
  "img/reg_pred_calosc.png",
  plot = pred_comp,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)
