# Autorzy: Szymon Wierzejski i Marcin Wojciechowski

install.packages("ggplot2")
install.packages("e1071")
install.packages("fitdistrplus")
install.packages("mnormt")
install.packages("ggExtra")

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
#     Analiza łącznego rozkładu log-zwrotów
#
# =========================================

b11 <- read.csv("11b_d.csv")

kurs_zamkniecia <- b11$Zamkniecie
kurs_data <- as.Date(b11$Data)


wmg_us_d <- read.csv("wmg_us_d.csv")

kurs_zamkniecia_wmg <- wmg_us_d$Zamkniecie
kurs_data_wmg <- as.Date(wmg_us_d$Data)


#-----------------------------------------
# Zad A1
# Rozkład norm log-zwortów dla 11B
#-----------------------------------------

log_kursu_11b <- log(kurs_zamkniecia)
#log_kursu_11b
diff_11b <- diff(log_kursu_11b)
#diff_11b

png(
  "img/diff_11b.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
plot(diff_11b)
dev.off()

png(
  "img/diff_11b_histogram.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
hist(diff_11b, prob = TRUE)
dev.off()


dist_11b_norm <- fitdist(diff_11b, "norm")

dist_11b_norm

png(
  "img/diff_11b_wykresy_diagnostyczne.png",
  width = 18,
  height = 18,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 2))
denscomp(list(dist_11b_norm), legendtext = "norm")
qqcomp(list(dist_11b_norm), legendtext = "norm")
cdfcomp(list(dist_11b_norm), legendtext = "norm")
ppcomp(list(dist_11b_norm), legendtext = "norm")
dev.off()

#-----------------------------------------
# Test równości rozkładów dla log-zwrotów 11B
#-----------------------------------------

iterations <- 10000
n <- length(log_kursu_11b)
n

D_11b <- c()

for (i in 1:iterations) {
  y_ln <- rnorm(n, dist_11b_norm$estimate[1], dist_11b_norm$estimate[2])
  D_11b[i] <- ks.test(
    y_ln,
    pnorm,
    dist_11b_norm$estimate[1],
    dist_11b_norm$estimate[2],
    exact = TRUE
  )$statistic
}

# Obliczamy dn_ln, czyli wartosc statystyki D,
# dla danych kurs_zamkniecia i rozkładu F0 wybranego w punkcie
dn_ln_11b <- ks.test(
  diff_11b,
  pnorm,
  dist_11b_norm$estimate[1],
  dist_11b_norm$estimate[2],
  exact = TRUE
)$statistic

dn_ln_11b

png(
  "img/diff_11b_hipoteza_o_rownosci.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
hist(D_11b, prob = TRUE, xlab = "")
points(dn_ln_11b, 0, pch = 19, col = "red")
dev.off()


# Odleglosc dystrybuanty empirycznej dla kurs_zamkniecia,
# oraz dystrybuanty F0 jest istotnie większa od odleglosci obserwowanych
# dla probek tej samej licznosci z rozkladu F0.

p_value_ln <- length(D_11b[D_11b > dn_ln_11b]) / iterations
p_value_ln


alfa <- c(0.05)
p_value_ln <= alfa
# Wartosc p-value jest większa od przyjetego poziomu istotnosci, zatem
# hipoteze o rownosci dystrybuant (F=F0, gdzie F poszukiwany rozklad) odrzucam.


#-----------------------------------------
# Rozkład norm log-zwortów dla WMG
#-----------------------------------------

log_kursu_wmg <- log(kurs_zamkniecia_wmg)

diff_wmg <- diff(log_kursu_wmg)
png(
  "img/diff_wmg.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
plot(diff_wmg)
dev.off()

png(
  "img/diff_wmg_histogram.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
hist(diff_wmg, prob = TRUE)
dev.off()



dist_wmg_norm <- fitdist(diff_wmg, "norm")

dist_wmg_norm

png(
  "img/diff_wmg_wykresy_diagnostyczne.png",
  width = 18,
  height = 18,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 2))
denscomp(list(dist_wmg_norm), legendtext = "norm")
qqcomp(list(dist_wmg_norm), legendtext = "norm")
cdfcomp(list(dist_wmg_norm), legendtext = "norm")
ppcomp(list(dist_wmg_norm), legendtext = "norm")
dev.off()

#-----------------------------------------
# Test równości rozkładów dla log-zwrotów WMG
#-----------------------------------------
iterations <- 10000
n <- length(log_kursu_wmg)
n

D_wmg <- c()

for (i in 1:iterations) {
  y_ln <- rnorm(n, dist_wmg_norm$estimate[1], dist_wmg_norm$estimate[2])
  D_wmg[i] <- ks.test(
    y_ln,
    pnorm,
    dist_wmg_norm$estimate[1],
    dist_wmg_norm$estimate[2],
    exact = TRUE
  )$statistic
}

# Obliczamy dn_ln, czyli wartosc statystyki D,
# dla danych kurs_zamkniecia i rozkładu F0 wybranego w punkcie
dn_ln_wmg <- ks.test(
  diff_wmg,
  pnorm,
  dist_wmg_norm$estimate[1],
  dist_wmg_norm$estimate[2],
  exact = TRUE
)$statistic

dn_ln_wmg

png(
  "img/diff_wmg_hipoteza_o_rownosci.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
hist(D_wmg, prob = TRUE, xlab = "")
points(dn_ln_wmg, 0, pch = 19, col = "red")
dev.off()


# Odleglosc dystrybuanty empirycznej dla kurs_zamkniecia,
# oraz dystrybuanty F0 jest istotnie większa od odleglosci obserwowanych
# dla probek tej samej licznosci z rozkladu F0.

p_value_ln <- length(D_wmg[D_wmg > dn_ln_wmg]) / iterations
p_value_ln


alfa <- c(0.05)
p_value_ln <= alfa
# Wartosc p-value jest większa od przyjetego poziomu istotnosci, zatem
# hipoteze o rownosci dystrybuant (F=F0, gdzie F poszukiwany rozklad) odrzucam.



#-----------------------------------------
# Zad B1
# Wykres rozrzutu z histogramami roskładów brzegowych
#-----------------------------------------

# Sprawdzanie czy daty w naszych spółkach się pokrywają
identical(kurs_data, kurs_data_wmg)
which(kurs_data != kurs_data_wmg) # bardzo się nie pokrywają


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


# Tworzenie wykresów na podstawie danych
library(ggExtra)
merged_logi <- data.frame(m_diff_wmg, m_diff_11b)
#merged_logi
l <- ggplot(merged_logi, aes(x = m_diff_wmg, y = m_diff_11b)) +
  geom_point()
l
rozrzut <- ggMarginal(l, type = "histogram")
rozrzut
ggsave(
  "img/rozrzut_z_histogramami.png",
  plot = rozrzut,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)


#-----------------------------------------
# Zad B2
# estymacja wektora srednich, macierzy kowariancji, macierzy korelacji
#-----------------------------------------
library(mnormt)

mu <- colMeans(merged_logi) # wektor średnich
mu
Sigma <- cov(merged_logi) # estymator nieobciazony

n <- nrow(merged_logi)
n
Sigma_ob <- (n - 1) * cov(merged_logi) / n # estymator obciążony

Sigma
Sigma_ob

P <- cor(merged_logi) # macierz korelacji
P

#-----------------------------------------
# Zad B3
# wykres gestosci
#-----------------------------------------
library(mnormt)

s1 <- s2 <- 1 # odchylenia standardowe
x <- seq(-0.15 * s1, 0.15 * s1, 0.005)
y <- seq(-0.15 * s2, 0.15 * s2, 0.005)

# gestosc rozkladu normalnego o sredniej mu i macierzy kowariancji S
f <- function(x, y) dmnorm(cbind(x, y), mu, Sigma)
z <- outer(x, y, f)
# Funkcja outer tworzy macierz wynikową z
# przez aplikację funkcji f na wszystkie kombinacje x i y.

# Wykres jednowymiarowy dla B11
diff_11b_dens <- dnorm(x, mean = mu[2], sd = sqrt(Sigma[2, 2]))

# Wykres jednowymiarowy dla WMG
diff_wmg_dens <- dnorm(y, mean = mu[1], sd = sqrt(Sigma[1, 1]))

# Rysowanie wykresów jednowymiarowych
diff_11b_jednowymiarowy <- ggplot() +
  geom_line(aes(x = x, y = diff_11b_dens), color = "blue") +
  ggtitle("Gęstość jednowymiarowa dla 11B")
diff_11b_jednowymiarowy

diff_wmg_jednowymiarowy <- ggplot() +
  geom_line(aes(x = y, y = diff_wmg_dens), color = "blue") +
  ggtitle("Gęstość jednowymiarowa dla WMG")
diff_wmg_jednowymiarowy

ggsave(
  "img/diff_11b_wykres_jednowymiarowy.png",
  plot = diff_11b_jednowymiarowy,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

ggsave(
  "img/diff_wmg_wykres_jednowymiarowy.png",
  plot = diff_wmg_jednowymiarowy,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

# Połączenie obu wykresów na jeden plik png
library(cowplot)
diff_merged_jednowymiarowy <- plot_grid(
  diff_11b_jednowymiarowy,
  diff_wmg_jednowymiarowy
)
diff_merged_jednowymiarowy

ggsave(
  "img/diff_wykresy_jednowymiarowe.png",
  plot = diff_merged_jednowymiarowy,
  width = 18,
  height = 6,
  units = "cm",
  dpi = 480
)



# wykres gestosci
png(
  "img/diff_gestosc_laczona.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
persp(x, y, z, theta = 30, phi = 30, col = "lightblue", main = "Gęstość łączna")
dev.off()
# lub dokladniejszy wykres
png(
  "img/diff_gestosc_laczona_detailed.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
persp(x, y, z,
  theta = -30, phi = 25,
  shade = 0.75, col = "lightblue", expand = 0.5, r = 2,
  ltheta = 25, ticktype = "detailed", main = "Gęstość łączna"
)
dev.off()




probka_rozrzut <- MASS::mvrnorm(n, mu = mu, Sigma = Sigma)
#probka_rozrzut
png(
  "img/diff_wykresy_rozrzutu.png",
  width = 12,
  height = 9,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 2))
plot(merged_logi, xlim = c(-0.15, 0.15), ylim = c(-0.10, 0.10))
plot(probka_rozrzut, xlim = c(-0.15, 0.15), ylim = c(-0.10, 0.10))
dev.off()


png(
  "img/diff_porownanie_wykresow_rozrzutu.png",
  width = 12,
  height = 9,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
plot(merged_logi,
  xlim = c(-0.15, 0.15),
  ylim = c(-0.10, 0.10),
  col = "blue",
  main = "Porównanie dwóch zbiorów danych",
  xlab = "X",
  ylab = "Y"
)

points(probka_rozrzut,
  col = "black"
)

legend("topleft",
  legend = c("Diff kursów", "Wygenerowana próbka"),
  col = c("blue", "black"),
  pch = 16,
  cex = 0.8
)
dev.off()
