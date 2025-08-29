# Autor: Szymon Wierzejski

install.packages("ggplot2")
install.packages("e1071")
install.packages("fitdistrplus")
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
#     Spółka 11B
#
# =========================================

#-----------------------------------------
# Zad 1
# Ceny akcji spółki na rok 2022
#-----------------------------------------


b11 <- read.csv("11b_d.csv")

kurs_zamkniecia <- b11$Zamkniecie
kurs_data <- as.Date(b11$Data)

b11f <- data.frame(data = kurs_data, zamkniecie = kurs_zamkniecia)

#-----------------------------------------
# Wykres zamknięcia
#-----------------------------------------

wykres_kursu <- ggplot(b11f, aes(x = data, y = zamkniecie, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "Cena zamknięcia (zł)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(b11f$data), max(b11f$data))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_kursu

ggsave(
  "img/Wykres_cen_akcji_11B.png",
  plot = wykres_kursu,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Histogram kursów zamknięcia
#-----------------------------------------

histogram_kursu_ggplot <- ggplot(
  b11f,
  aes(x = zamkniecie, y = after_stat(density))
) +
  geom_histogram(binwidth = 10, fill = "grey", color = "black") +
  labs(title = NULL, x = "Cena zamknięcia (zł)", y = "Gęstość")

hist(kurs_zamkniecia, prob = TRUE, xlab = "Zamknięcie", ylab = "Gęstość")

histogram_kursu_ggplot

ggsave(
  "img/historgram_11B.png",
  plot = histogram_kursu_ggplot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Zad 2
# Statystyki opisowe
#-----------------------------------------

b11_ex <- mean(kurs_zamkniecia)
b11_sd <- sd(kurs_zamkniecia)
b11_skew <- skewness(kurs_zamkniecia)
b11_kurt <- kurtosis(kurs_zamkniecia)

b11_ex
b11_sd
b11_skew
b11_kurt

#-----------------------------------------
# Zad 3
# Estymacja parametrów rozkładu normalnego, log-normalnego i gamma
#-----------------------------------------

dist_norm <- fitdist(kurs_zamkniecia, "norm")
dist_lnorm <- fitdist(kurs_zamkniecia, "lnorm")
dist_gamma <- fitdist(kurs_zamkniecia, "gamma")

dist_norm
dist_lnorm
dist_gamma

#-----------------------------------------
# Zad 4
# Wykresy diagnostyczne
#-----------------------------------------

key <- c("norm", "lnorm", "gamma")
png(
  "img/Wykresy_diagnostyczne.png",
  width = 18,
  height = 18,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 2))
denscomp(list(dist_norm, dist_lnorm, dist_gamma), legendtext = key)
qqcomp(list(dist_norm, dist_lnorm, dist_gamma), legendtext = key)
cdfcomp(list(dist_norm, dist_lnorm, dist_gamma), legendtext = key)
ppcomp(list(dist_norm, dist_lnorm, dist_gamma), legendtext = key)

dev.off()


#-----------------------------------------
# Analiza wartości statystyk
#-----------------------------------------

gofstat(
  list(dist_norm, dist_lnorm, dist_gamma),
  fitnames = key
)

#-----------------------------------------
# Zad 5
# Test hipotezy o równości rozkładów
#-----------------------------------------

iterations <- 10000
n <- length(kurs_zamkniecia)
n

D <- c()

for (i in 1:iterations) {
  y_ln <- rlnorm(n, dist_lnorm$estimate[1], dist_lnorm$estimate[2])
  D[i] <- ks.test(
    y_ln,
    plnorm,
    dist_lnorm$estimate[1],
    dist_lnorm$estimate[2],
    exact = TRUE
  )$statistic
}

# Obliczamy dn_ln, czyli wartosc statystyki D,
# dla danych kurs_zamkniecia i rozkładu F0 wybranego w punkcie
dn_ln <- ks.test(
  kurs_zamkniecia,
  plnorm,
  dist_lnorm$estimate[1],
  dist_lnorm$estimate[2],
  exact = TRUE
)$statistic

dn_ln

png(
  "img/hipoteza_o_rownosci.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
hist(D, prob = TRUE, xlab = "")
points(dn_ln, 0, pch = 19, col = "red")
dev.off()


# Odleglosc dystrybuanty empirycznej dla kurs_zamkniecia,
# oraz dystrybuanty F0 jest istotnie większa od odleglosci obserwowanych
# dla probek tej samej licznosci z rozkladu F0.

p_value_ln <- length(D[D > dn_ln]) / iterations
p_value_ln


alfa <- c(0.05)
p_value_ln <= alfa
# Wartosc p-value jest większa od przyjetego poziomu istotnosci, zatem
# hipoteze o rownosci dystrybuant (F = F0, gdzie F poszukiwany rozklad) odrzucam.
