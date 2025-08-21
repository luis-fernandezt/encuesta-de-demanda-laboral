# Instalando Latex

## Install basic TeX/LaTeX system from miktex.org
## Then in console, Click on-the-fly as Always in Missing packages settings MiKTeX
## install.packages("tinytex") from RStudio to knit from Markdown

# Librerías ####
library(readr)
library(tidyverse)

library(survey)
library(srvyr)
library(car)

# Bases de datos ####
dir.create("bd")

## ENADEL 2024 (7.0 MB)
download.file("https://www.subtrab.gob.cl/wp-content/uploads/2025/05/ENADEL_2024_externa.csv", "bd/ENADEL_2024_externa.csv", mode="wb")

## Documento metodológico (1.3 MB)
download.file("https://www.subtrab.gob.cl/wp-content/uploads/2025/05/Documento-Metodologico-ENADEL-2024.pdf", "bd/Documento-Metodologico-ENADEL-2024.pdf", mode="wb")

## Libro de códigos (146 KB)
download.file("https://www.subtrab.gob.cl/wp-content/uploads/2025/05/Libro_de_codigos_ENADEL_2024.xlsx", "bd/Libro_de_codigos_ENADEL_2024.xlsx", mode="wb")

## Informe de resultados 2024 (3.8 MB)
download.file("https://www.subtrab.gob.cl/wp-content/uploads/2025/06/Informe-Final-de-Resultados.pdf", "bd/Informe-Final-de-Resultados.pdf", mode="wb")

# Cálculos
ENADEL_2024 <- read_csv("bd/ENADEL_2024_externa.csv")

ENADEL_2024 <- mutate(ENADEL_2024, 
                      a1_cod_CIIU_seccion_agrupada = car::recode(ENADEL_2024$a1_cod_CIIU_seccion, 
                                                                 '"A" = "A";
                                                                 "C" = "C";
                                                                 "D":"E" = "D-E";
                                                                 "F" = "F";
                                                                 "G" = "G";
                                                                 "H" = "H";
                                                                 "I" = "I";
                                                                 "J" = "J";
                                                                 "K" = "K";
                                                                 "L" = "L";
                                                                 "M":"N" = "M-N";
                                                                 "R":"S" = "R-S";
                                                                 else = "Varias*"'))

names(ENADEL_2024)
# Aplicar Factor de expansion ####
# Según el documento metodológico de ENADEL 2024, el diseño muestral es probabilístico y estratificado, con estratos definidos por las 16 regiones y 12 agrupaciones de ramas de actividad económica, pero no se menciona un nivel de conglomerado específico (clusters). Por tanto, no hay una variable conglomerado oficial en los datos.

svydsgn <- ENADEL_2024 %>% as_survey_design(ids = idempresa_ficticio, # o usar ids = 1
                                            weights = fact_emp, 
                                            strata = estrato)

# Estimación de Totales ####
total_trab <- svydsgn |> summarise(estimacion = survey_total(b6_1, vartype = c("cv", "se"), na.rm = TRUE),
                                   n = unweighted(n()))

cv1 <- as.numeric(total_trab[2]) #CV trabajadores

total_empr <- svydsgn |>  summarise(estimacion = survey_total(vartype = c("cv", "se"), na.rm = TRUE),
                                    n = unweighted(n()))

cv2 <- as.numeric(total_empr[2]) #CV empresas

# Empresas por región ####
nro_empresas <- svydsgn |>
  group_by(region) |>
  summarise(`N° empresas` = survey_total(vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° empresas`/sum(`N° empresas`) *100,
           Región = region,
         `CV` = `N° empresas_cv`) |>
  arrange(desc(`N° empresas`)) |>
  select(Región, `Tamaño muestral`, `N° empresas`, `Distribución %`, `CV`)

total_pais <- 
nro_empresas |>
  summarise(`Región` = 0,
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° empresas`= sum(`N° empresas`),
            `Distribución %` = sum(`Distribución %`),
            `CV` = as.numeric(cv1))


# Unir tabla principal con totales
nro_empresas_final <- bind_rows(nro_empresas, total_pais)
nro_empresas_final$Región[17] <- c("Total")

knitr::kable(nro_empresas_final, 
             digits=2,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

#write_csv(nro_empresas_final, "bd/pais_empresas.csv")
rm(total_pais, nro_empresas)

#Trabajadores por región ####

nro_trabajadores <- 
svydsgn |>
  group_by(region) |>
  summarise(`N° trabajadores` = survey_total(b6_1, vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° trabajadores`/sum(`N° trabajadores`) *100,
         Región = region,
         `CV` = `N° trabajadores_cv`) |>
  arrange(desc(`N° trabajadores`)) |>
  select(Región, `Tamaño muestral`, `N° trabajadores`, `Distribución %`, `CV`)

total_pais <- 
  nro_trabajadores |>
  summarise(`Región` = 0,
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° trabajadores`= sum(`N° trabajadores`),
            `Distribución %` = sum(`Distribución %`),
            `CV` = as.numeric(cv2))

# Unir tabla principal con totales
nro_trabajadores_final <- bind_rows(nro_trabajadores, total_pais)
nro_trabajadores_final$Región[17] <- c("Total")

knitr::kable(nro_trabajadores_final, 
             digits=2,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

#write_csv(nro_trabajadores_final, "bd/pais_trabajadores.csv")

rm(total_pais, nro_trabajadores)

# Empresas por CIIU####

nro_empresas_CIIU <- svydsgn |>
  group_by(a1_cod_CIIU_seccion) |>
  summarise(`N° empresas` = survey_total(vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° empresas`/sum(`N° empresas`) *100,
         `CIIU Seccion` = a1_cod_CIIU_seccion,
         `CV` = `N° empresas_cv`) |>
  arrange(desc(`N° empresas`)) |>
  select(`CIIU Seccion`, `Tamaño muestral`, `N° empresas`, `Distribución %`, `CV`)

total_pais <- 
  nro_empresas_CIIU |>
  summarise(`CIIU Seccion` = "Total",
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° empresas`= sum(`N° empresas`),
            `Distribución %` = sum(`Distribución %`),
            `CV` = as.numeric(cv1))
             


# Unir tabla principal con totales
nro_empresas_CIIU_final <- bind_rows(nro_empresas_CIIU, total_pais)

knitr::kable(nro_empresas_CIIU_final, 
             digits=2,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

#write_csv(nro_empresas_CIIU_final, "bd/pais_empresas_ciuu.csv")

rm(total_pais, nro_empresas_CIIU)


# Trabajadores por CIIU ####

nro_trabajadores_ciuu <- 
  svydsgn |>
  group_by(a1_cod_CIIU_seccion) |>
  summarise(`N° trabajadores` = survey_total(b6_1, vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° trabajadores`/sum(`N° trabajadores`) *100,
         `CIIU Seccion` = a1_cod_CIIU_seccion,
         `CV` = `N° trabajadores_cv`) |>
  arrange(desc(`N° trabajadores`)) |>
  select(`CIIU Seccion`, `Tamaño muestral`, `N° trabajadores`, `Distribución %`, `CV`)

total_pais <- 
  nro_trabajadores_ciuu |>
  summarise(`CIIU Seccion` = "Total",
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° trabajadores`= sum(`N° trabajadores`),
            `Distribución %` = sum(`Distribución %`),
            `CV` = as.numeric(cv2))

# Unir tabla principal con totales
nro_trabajadores_ciuu_final <- bind_rows(nro_trabajadores_ciuu, total_pais)

knitr::kable(nro_trabajadores_ciuu_final, 
             digits=2,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

#write_csv(nro_trabajadores_ciuu_final, "bd/pais_trabajadores_ciuu.csv")

rm(total_pais, nro_trabajadores_ciuu)

# Filtro region ######
reg_filtro <- 14

# Empresas por region ####


empr_reg1 <- svydsgn |>
  filter(region == reg_filtro) |>
  group_by(a1_glosa_CIIU_seccion) |>
  summarise(`N° empresas` = survey_total(vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° empresas`/sum(`N° empresas`) *100,
         `CIIU Seccion` = a1_glosa_CIIU_seccion,
         `CV` = `N° empresas_cv`) |>
  arrange(desc(`N° empresas`)) |>
  select(`CIIU Seccion`, `Tamaño muestral`, `N° empresas`, `Distribución %`, `CV`)

cv_temp <- (svydsgn |> 
  filter(region == reg_filtro) |>
  summarise(survey_total(vartype = c("cv"),
                         na.rm = TRUE), n = unweighted(n())))[2]

empr_reg2 <- empr_reg1 |>
  summarise(`CIIU Seccion` = "Total",
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° empresas`= sum(`N° empresas`),
            `Distribución %` = sum(`Distribución %`),
            `CV` = as.numeric(cv_temp))

nro_empr_reg <- bind_rows(empr_reg1, empr_reg2)

knitr::kable(nro_empr_reg, 
             digits=2,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

#write_csv(nro_empr_reg, "bd/r10_empresas.csv")
#write_csv(nro_empr_reg, "bd/r14_empresas.csv")

rm(empr_reg, empr_reg1, empr_reg2, cv_temp, temp1, temp2, temp)

# Trabajadores por region segun CIIU ####
trab_ciuu_reg1 <-
  svydsgn |>
  filter(region == reg_filtro) |>
  group_by(a1_glosa_CIIU_seccion) |>
  summarise(`N° trabajadores` = survey_total(b6_1, vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `N° trabajadores`/sum(`N° trabajadores`) *100,
         `CIIU Seccion` = a1_glosa_CIIU_seccion,
         `CV` = `N° trabajadores_cv`) |>
  arrange(desc(`N° trabajadores`)) |>
  select(`CIIU Seccion`, `Tamaño muestral`, `N° trabajadores`, `Distribución %`, `CV`)

cv_temp <- (svydsgn |> 
              filter(region == reg_filtro) |>
              summarise(survey_total(b6_1, vartype = c("cv"),
                                     na.rm = TRUE), n = unweighted(n())))[2]

trab_ciuu_reg2 <- 
  trab_ciuu_reg1 |>
  summarise(`CIIU Seccion` = "Total",
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `N° trabajadores`= sum(`N° trabajadores`),
            `Distribución %` = sum(`Distribución %`),
            `CV` = as.numeric(cv_temp))

# Unir tabla principal con totales
nro_trab_ciuu_reg <- bind_rows(trab_ciuu_reg1, trab_ciuu_reg2)

knitr::kable(nro_trab_ciuu_reg, 
             digits=2,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

#write_csv(nro_trab_ciuu_reg, "bd/r10_trabajadores_ciuu.csv")
#write_csv(nro_trab_ciuu_reg, "bd/r14_trabajadores_ciuu.csv")

rm(trab_ciuu_reg, trab_ciuu_reg2, trab_ciuu_reg1, cv_temp )

# Vacantes ####
temp <- 
  svydsgn |>
  filter(region == reg_filtro) |>
  group_by(a1_glosa_CIIU_seccion) |>
  summarise(`Estimación` = survey_total(b7_1, vartype = "cv"),
            `Tamaño muestral` = unweighted(n())) |>
  mutate(`Distribución %` = `Estimación`/sum(`Estimación`) *100,
         `CIIU Seccion` = a1_glosa_CIIU_seccion,
         `CV` = `Estimación_cv`) |>
  arrange(desc(`Estimación`)) |>
  select(`CIIU Seccion`, `Tamaño muestral`, `Estimación`, `Distribución %`, `CV`)

cv_temp <- (svydsgn |> 
              filter(region == reg_filtro) |>
              summarise(survey_total(b7_1, vartype = c("cv"),
                                     na.rm = TRUE), n = unweighted(n())))[2]

vacantes2 <- 
  temp |>
  summarise(`CIIU Seccion` = "Total",
            `Tamaño muestral` = sum(`Tamaño muestral`),
            `Estimación`= sum(`Estimación`),
            `Distribución %` = sum(`Distribución %`),
            `CV` = as.numeric(cv_temp))

# Unir tabla principal con totales
temp2 <- bind_rows(temp, vacantes2)

knitr::kable(temp2, 
             digits=2,
             align=rep('c', 5),
             format.args = list(decimal.mark = ',', big.mark = "."))

#write_csv(temp2, "bd/r10_vacantes.csv")
#write_csv(temp2, "bd/r14_vacantes.csv")

rm(trab_ciuu_reg, trab_ciuu_reg2, trab_ciuu_reg1, cv_temp )


R.version$version.string
