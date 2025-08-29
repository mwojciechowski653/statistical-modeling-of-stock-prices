#Autor: Marcin Wojciechowski

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
#     Spółka WMG.US
#
# =========================================

#-----------------------------------------
# Zad 1
# Ceny akcji spółki na rok 2022
#-----------------------------------------

wmg_us_d <- read.csv("wmg_us_d.csv")

kurs_zamkniecia_wmg <- wmg_us_d$Zamkniecie
kurs_data_wmg <- as.Date(wmg_us_d$Data)

#-----------------------------------------
# Wykres zamknięcia
#-----------------------------------------

kurs_zamkniecia_wmg_do_wykresow <- data.frame(data = kurs_data_wmg, zamkniecie = kurs_zamkniecia_wmg)

wykres_kursu <- ggplot(kurs_zamkniecia_wmg_do_wykresow, aes(x = data, y = zamkniecie, group = 1)) +
  geom_line(color = "darkblue") +
  labs(x = NULL, y = "Cena zamkniecia (w $)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", limits = c(min(kurs_zamkniecia_wmg_do_wykresow$data), max(kurs_zamkniecia_wmg_do_wykresow$data))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust = -0.2))

wykres_kursu

ggsave("Wykres_cen_akcji_WMG.png", plot = wykres_kursu, width = 12, height = 9, units = "cm", dpi = 480)

#---------------------------------------
# Histogramy z użyciem ggplot() i hist()
#---------------------------------------

histogram_kursu_ggplot <- ggplot(kurs_zamkniecia_wmg_do_wykresow, aes(x = zamkniecie, y = after_stat(density))) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = "Histogram cen zamknięcia", x = "Cena zamknięcia", y = "Gęstość")

histogram_kursu_ggplot
png(
  "img/Histogram_kursu_zamkniecia_WMG.png",
  width = 12,
  height = 9,
  pointsize = 9,
  units = "cm",
  res = 480
)
histogram_kursu <- hist(kurs_zamkniecia_wmg, prob = TRUE, xlab = "Cena zamknięcia", ylab = "Gęstość")
dev.off()

#-----------------------------------------
# Zad 2
# Statystyki opisowe
#-----------------------------------------

wmg_srednia <- mean(kurs_zamkniecia_wmg)
wmg_srednia
wmg_odchylenie <- sd(kurs_zamkniecia_wmg)
wmg_odchylenie
wmg_skosnosc <- skewness(kurs_zamkniecia_wmg)
wmg_skosnosc
wmg_kurtoza <- kurtosis(kurs_zamkniecia_wmg)
wmg_kurtoza

#-----------------------------------------
# Zad 3
# Estymacja parametrów rozkładu normalnego, log-normalnego i gamma
#-----------------------------------------

rozklad_norm <- fitdist(kurs_zamkniecia_wmg, "norm")
rozklad_norm
rozklad_lnorm <- fitdist(kurs_zamkniecia_wmg, "lnorm")
rozklad_lnorm
rozklad_gamma <- fitdist(kurs_zamkniecia_wmg, "gamma")
rozklad_gamma

#-----------------------------------------
# Zad 4
# Wykresy diagnostyczne
#-----------------------------------------

plot.legend <- c("norm", "lognorm", "gamma")
par(mfrow = c(2, 2))

denscomp(list(rozklad_norm, rozklad_lnorm, rozklad_gamma), legendtext = plot.legend)
qqcomp(list(rozklad_norm, rozklad_lnorm, rozklad_gamma), legendtext = plot.legend)
cdfcomp(list(rozklad_norm, rozklad_lnorm, rozklad_gamma), legendtext = plot.legend)
ppcomp(list(rozklad_norm, rozklad_lnorm, rozklad_gamma), legendtext = plot.legend)

#-----------------------------------------
# Analiza wartości statystyk
#-----------------------------------------

gofstat(list(rozklad_norm, rozklad_lnorm, rozklad_gamma),
  fitnames = c("norm", "lnorm", "gamma")
)

# Na podstawie powyższych danych wybrałem rozkład log-normalny

#-----------------------------------------
# Zad 5
# Test hipotezy o równości rozkładów
#-----------------------------------------

# Testuję hipoteze
# H0: F = lnorm(3.41)(F0) przeciwko hipotezie H1: F nie jest równy lnorm(3.41)(F0)

# 1. Rozkład statystyki Dn (metoda MC).
# Generuję N = 10000 próbek liczności n = 100 z rozkładu F0 = lnorm(3.41)
# i obliczam odległość dystrybuant empirycznych od rozkładu F0 (wartość statystyki Dn).

N <- 10000
n <- 100

D <- c()

for (i in 1:N) {
  Probka <- rlnorm(n, rozklad_lnorm$estimate[1], rozklad_lnorm$estimate[2])
  D[i] <- ks.test(Probka, plnorm, rozklad_lnorm$estimate[1], rozklad_lnorm$estimate[2], exact = TRUE)$statistic
}

# 2. Obliczam dn - wartość statystyki dla próby kurs_zamkniecia_wmg i F0
dn <- ks.test(kurs_zamkniecia_wmg, plnorm, rozklad_lnorm$estimate[[1]], rozklad_lnorm$estimate[[2]], exact = TRUE)$statistic
dn

# D na histogramie
par(mfrow = c(1, 1))
hist(D, prob = TRUE)
points(dn, 0, pch = 19, col = "darkblue")

# 3. Obliczam p-value
p_value <- length(D[D > dn]) / N
p_value

# Przyjmuję poziom istotnosci 5%
alfa <- 0.05
p_value <= alfa

# Na poziomie istotności 5%, nie ma podstaw do odrzucenia hipotezy zerowej (F0 = lnorm(3.41)),
