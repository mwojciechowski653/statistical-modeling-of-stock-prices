# Statistical Modeling of Stock Prices

> Polish version below / Wersja polska poniżej

## Project Overview

This project analyzes **stock prices from 2022 of two publicly traded companies**: [11 bit studios](https://11bitstudios.com/pl/) and [Warner Music Group](https://www.wmg.com/) using **R** – from data preparation and **log‑return** calculation, through tests of compliance with the normal distribution and one-/two‑dimensional distribution fitting, to **linear regression** and results **visualization**. The repository includes complete R scripts and a **LaTeX/PDF** report.

### Main components

> Note: The report, file names and code are in Polish, as this project was originally created for a university course.

- single‑stock analyses (`src/11b_analiza_cen_spolki.R`, `src/wmg_us_analiza_cen_spolki.R`),
- **joint distribution of log‑returns** (`src/analiza_lacznego_rozkladu_log_zwrotow.R`),
- **linear regression** and residual diagnostics (`src/regresja_liniowa.R`),
- tests of compliance with the normal distribution (e.g., Anderson–Darling, Lilliefors via `nortest`), skewness/kurtosis (`e1071`), and distribution fitting (`fitdistrplus`).

Input data is included in the repo (`src/11b_d.csv, src/wmg_us_d.csv`), and data visualizations are saved to `src/img/`.

## Tech Stack

- **Language**: R
- **Report**: LaTeX → PDF
- **R packages**: `ggplot2`, `ggExtra`, `cowplot`, `fitdistrplus`, `nortest`, `e1071`, `mnormt`, `rstudioapi`

Install packages (in R):

```r
install.packages(c("ggplot2", "ggExtra", "cowplot", "fitdistrplus", "nortest", "e1071", "mnormt", "rstudioapi"))
```

## Running the project

1. Clone the repository:

   ```bash
   git clone https://github.com/mwojciechowski653/statistical-modeling-of-stock-prices.git
   cd statistical-modeling-of-stock-prices
   ```

2. Enter directory with R files

   ```bash
   cd src
   ```

3. Install R (4.x recommended) and optionally RStudio.
4. Install the required packages (see above).
5. Run selected R scripts (examples):

   ```r
   # Single‑stock analysis
   source("11b_analiza_cen_spolki.R")
   source("wmg_us_analiza_cen_spolki.R")

   # Joint distribution of log‑returns
   source("analiza_lacznego_rozkladu_log_zwrotow.R")

   # Linear regression + diagnostics
   source("regresja_liniowa.R")
   ```

6. (Optional) Build the LaTeX report:
   ```bash
   cd latex
   pdflatex podsumowanie_projektu.tex
   ```
   The ready PDF is also included in the repo: **`podsumowanie_projektu.pdf`**.

## Directory Structure

```
statistical-modeling-of-stock-prices/
├── latex/
│   ├── img/                                          # Images needed for .tex file
│   └── podsumowanie_projektu.tex                     # .tex file of report
├── src/
│   ├── img/                                          # Directory where images are created from .R files
│   ├── 11b_analiza_cen_spolki.R                      # Stock analyses of 11B
│   ├── 11b_d.csv                                     # Data of 11B
│   ├── analiza_lacznego_rozkladu_log_zwrotow.R       # Analyses of joint distribution of log-returns
│   ├── regresja_liniowa.R                            # Creating and analyzing linear regression
│   ├── wmg_us_analiza_cen_spolki.R                   # Stock analyses of WMG.US
│   └── wmg_us_d.csv                                  # Data of WMG.US
├── LICENSE                                           # Project license
├── podsumowanie_projektu.pdf                         # Report summarizing whole project
└── README.md                                         # Description of project
```

## Authors

- **Szymon Wierzejski**  
  [GitHub](https://github.com/Simikao)
- **Marcin Wojciechowski**  
  [GitHub](https://github.com/mwojciechowski653)

## License

This project is licensed under the **MIT** license.
The full license text can be found in the [LICENSE](LICENSE) file.

In short: you are free to use, copy, modify, and distribute this code under the MIT terms. The software is provided “as is”, without any warranty.

---

# Statistical Modeling of Stock Prices wersja po polsku

## Opis projektu

Projekt przedstawia analizę **cen akcji dwóch spółek giełdowych**: [11 bit studios](https://11bitstudios.com/pl/) i [Warner Music Group](https://www.wmg.com/) za 2022 rok z wykorzystaniem języka **R** – od przygotowania danych i wyznaczenia **log‑zwrotów**, przez testy zgodności z rozkładem normalnym i dopasowanie rozkładów (jedno- i dwuwymiarowych), po **regresję liniową** i **wizualizację** wyników. Repozytorium zawiera kompletne skrypty R oraz raport w **LaTeX/PDF**.

### Główne elementy analizy

- analiza pojedynczych spółek (`src/11b_analiza_cen_spolki.R`, `src/wmg_us_analiza_cen_spolki.R`),
- analiza **łącznego rozkładu log‑zwrotów** dla dwóch spółek (`src/analiza_lacznego_rozkladu_log_zwrotow.R`),
- **regresja liniowa** i diagnoza reszt (`src/regresja_liniowa.R`),
- testy zgodności z rozkładem normalnym (np. Anderson–Darling, Lilliefors; pakiet `nortest`), skośność/kurtoza (`e1071`), dopasowanie rozkładów (`fitdistrplus`).

Dane wejściowe znajdują się w repozytorium (`src/11b_d.csv, src/wmg_us_d.csv`), a wykresy zapisywane są do katalogu `src/img/`.

## Stos technologiczny

- **Język**: R
- **Raport**: LaTeX → PDF
- **Pakiety R**: `ggplot2`, `ggExtra`, `cowplot`, `fitdistrplus`, `nortest`, `e1071`, `mnormt`, `rstudioapi`

Instalacja pakietów (w R):

```r
install.packages(c("ggplot2", "ggExtra", "cowplot", "fitdistrplus", "nortest", "e1071", "mnormt", "rstudioapi"))
```

## Uruchomienie projektu

1. Sklonuj repozytorium:

   ```bash
   git clone https://github.com/mwojciechowski653/statistical-modeling-of-stock-prices.git
   cd statistical-modeling-of-stock-prices
   ```

2. Wejdź do folderu z plikami R

   ```bash
   cd src
   ```

3. Zainstaluj R (zalecane 4.x) oraz – opcjonalnie – RStudio.
4. Zainstaluj wymagane pakiety (patrz wyżej).
5. Uruchom wybrane skrypty R (przykłady):

   ```r
   # Analiza spółek
   source("11b_analiza_cen_spolki.R")
   source("wmg_us_analiza_cen_spolki.R")

   # Łączny rozkład log‑zwrotów (dwuwymiarowy)
   source("analiza_lacznego_rozkladu_log_zwrotow.R")

   # Regresja liniowa + diagnostyka
   source("regresja_liniowa.R")
   ```

6. (Opcjonalnie) Zbuduj raport LaTeX:
   ```bash
   cd latex
   pdflatex podsumowanie_projektu.tex
   ```
   Gotowy PDF znajdziesz także w repozytorium: **`podsumowanie_projektu.pdf`**.

## Struktura katalogu

```
statistical-modeling-of-stock-prices/
├── latex/
│   ├── img/                                          # Zdjęcia potrzebne dla pliku .tex
│   └── podsumowanie_projektu.tex                     # Plik .tex raportu
├── src/
│   ├── img/                                          # Folder w którym zapisują się wykresy z plików .R
│   ├── 11b_analiza_cen_spolki.R                      # Analiza cen spółki 11B
│   ├── 11b_d.csv                                     # Dane spółki 11B
│   ├── analiza_lacznego_rozkladu_log_zwrotow.R       # Analiza łącznego rozkładu log-zwrotów spółek
│   ├── regresja_liniowa.R                            # Stworzenie regresji liniowej i jej badania
│   ├── wmg_us_analiza_cen_spolki.R                   # Analiza cen spółki WMG.US
│   └── wmg_us_d.csv                                  # Dane spółki WMG.US
├── LICENSE                                           # Licencja projektu
├── podsumowanie_projektu.pdf                         # Raport podsumowujący projekt
└── README.md                                         # Opis projektu
```

## Autorzy

- **Szymon Wierzejski**  
  [GitHub](https://github.com/Simikao)
- **Marcin Wojciechowski**  
  [GitHub](https://github.com/mwojciechowski653)

## Licencja

Projekt jest udostępniany na licencji **MIT**.  
Pełny tekst licencji znajdziesz w pliku [LICENSE](LICENSE).

W skrócie: możesz używać, kopiować, modyfikować i rozpowszechniać ten kod na warunkach MIT. Oprogramowanie dostarczane jest „tak jak jest”, bez żadnych gwarancji.
