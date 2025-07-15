# Instalando Latex

## Install basic TeX/LaTeX system from miktex.org
## Then in console, Click on-the-fly as Always in Missing packages settings MiKTeX
## install.packages("tinytex") from RStudio to knit from Markdown

# Librerías
library(readr)
library(tidyverse)

library(survey)
library(srvyr)

# Descarga de Bases de datos
dir.create("bd")

## ENADEL 2024 (7.0 MB)
download.file("https://www.subtrab.gob.cl/wp-content/uploads/2025/05/ENADEL_2024_externa.csv", "bd/ENADEL_2024_externa.csv", mode="wb")

## Documento metodológico (1.3 MB)
download.file("https://www.subtrab.gob.cl/wp-content/uploads/2025/05/Documento-Metodologico-ENADEL-2024.pdf", "bd/Documento-Metodologico-ENADEL-2024.pdf", mode="wb")

## Libro de códigos (146 KB)
download.file("https://www.subtrab.gob.cl/wp-content/uploads/2025/05/Libro_de_codigos_ENADEL_2024.xlsx", "bd/Libro_de_codigos_ENADEL_2024.xlsx", mode="wb")

# Cálculos
ENADEL_2024 <- read_csv("bd/ENADEL_2024_externa.csv")

ENADEL_2024 %>% #el n muestral = 5965
  select(a2) %>%
  summarise(count = n())

# Factor de expasi+on a la muestra
svydsgn <- ENADEL_2024 %>% as_survey_design(weights = fact_emp, 
                                            strata = estrato)
                                            #ids = id_directorio) # no viene  dado

#total de trabajadores
svytotal(~b6_1, svydsgn, na.rm = TRUE) 

#total de trabajadores por región (validado)
svyby(~b6_1, ~region, svydsgn, svytotal, na.rm = TRUE, vartype= c("se", "cv")) |>
  mutate(`Distribución %` = b6_1/sum(b6_1) *100,
         `N° trabajadores` = b6_1,
         `Región` = region) |>
  arrange(desc(`Distribución %`)) |>
  select(`Región`, `N° trabajadores`, `Distribución %`, se, cv)
  
#total de trabajadores por cod_CIIU_seccion (por validar)*
svyby(~b6_1, ~a1_cod_CIIU_seccion, svydsgn, svytotal, na.rm = TRUE, vartype= c("se", "cv")) |>
  mutate(`Distribución %` = b6_1/sum(b6_1) *100,
         `N° trabajadores` = b6_1,
         `CIIU4.CL` = a1_cod_CIIU_seccion) |>
  arrange(`CIIU4.CL`) |>
  select(`CIIU4.CL`, `N° trabajadores`, `Distribución %`, se, cv)

# Distribución de empresas (E) y trabajadores (T) por región

# Distribución de empresas y trabajadores según tamaño de ventas de la empresa

# Distribución de empresas por sector económico
