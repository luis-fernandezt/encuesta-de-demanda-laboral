# Instalando Latex

## Install basic TeX/LaTeX system from miktex.org
## Then in console, Click on-the-fly as Always in Missing packages settings MiKTeX
## install.packages("tinytex") from RStudio to knit from Markdown

# Librerías
library(readr)
library(tidyverse)

library(survey)
library(srvyr)
library(car)

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

# Factor de expasion a la muestra
# Según el documento metodológico de ENADEL 2024, el diseño muestral es probabilístico y estratificado, con estratos definidos por las 16 regiones y 12 agrupaciones de ramas de actividad económica, pero no se menciona un nivel de conglomerado específico (clusters). Por tanto, no hay una variable conglomerado oficial en los datos.

svydsgn <- ENADEL_2024 %>% as_survey_design(ids = 1,
                                            weights = fact_emp, 
                                            strata = estrato)

#total de trabajadores
svydsgn |>
  summarise(estimacion = survey_total(b6_1, vartype = c("cv","se"), 
                                      na.rm = TRUE),
            n = unweighted(n()))

# Distribución de empresas por región
nro_empresas <- svydsgn |>
  group_by(region) |>
  summarise(`N° empresas` = survey_total(vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° empresas`/sum(`N° empresas`) *100,
           Región = region) |>
  arrange(desc(`N° empresas`)) |>
  select(Región, `Tamaño muestral`, `N° empresas`, `Distribución %`)

total_pais <- 
nro_empresas |>
  summarise(`Región` = 0,
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° empresas`= sum(`N° empresas`),
            `Distribución %` = sum(`Distribución %`))


# Unir tabla principal con totales
nro_empresas_final <- bind_rows(nro_empresas, total_pais)
nro_empresas_final$Región[17] <- c("Total")

knitr::kable(nro_empresas_final, 
             digits=1,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

rm(total_pais, nro_empresas)

#total de trabajadores por región

nro_trabajadores <- 
svydsgn |>
  group_by(region) |>
  summarise(`N° trabajadores` = survey_total(b6_1, vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° trabajadores`/sum(`N° trabajadores`) *100,
         Región = region) |>
  arrange(desc(`N° trabajadores`)) |>
  select(Región, `Tamaño muestral`, `N° trabajadores`, `Distribución %`)

total_pais <- 
  nro_trabajadores |>
  summarise(`Región` = 0,
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° trabajadores`= sum(`N° trabajadores`),
            `Distribución %` = sum(`Distribución %`))

# Unir tabla principal con totales
nro_trabajadores_final <- bind_rows(nro_trabajadores, total_pais)
nro_trabajadores_final$Región[17] <- c("Total")

knitr::kable(nro_trabajadores_final, 
             digits=1,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

rm(total_pais, nro_trabajadores)

#total de trabajadores por cod_CIIU_seccion

# Distribución de empresas (E) y trabajadores (T) por región

# Distribución de empresas y trabajadores según tamaño de ventas de la empresa

# Distribución de empresas por sector económico
