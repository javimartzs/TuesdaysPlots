library(esadeecpol)

#' Importamos los microdatos de la EPA --------------------------------------
setwd('input/EPA')
files <- list.files(pattern = '\\.csv$')
data <- data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE)
rm(files)
setwd('../../')

#' Fusionamos para obtener la variable trimestre -----------------------------
ciclos <- readxl::read_excel('input/EPA/Registro.xlsx', sheet = 'Tablas5')
data <- merge(data, ciclos, by = 'ciclo')
rm(ciclos)

data <- data |> 
    mutate(sexo1 = ifelse(sexo1 == 1, 'Hombre', 'Mujer')) |> 
    mutate()

#' Evolución del empleo en millones de personas -------------------------------

ocu <- data |> 
    filter(aoi %in% c(3:4)) |> 
    group_by(fecha) |> 
    summarise(ocupados = sum(factorel)/1000000)

openxlsx::write.xlsx(ocu, 'output/ocupados.xlsx')

#' Tasa de paro y Tasa de temporalidad ----------------------------------------

temp <- data |> 
    filter(aoi %in% c(3:4)) |> 
    filter(situ %in% c(7:8)) |> 
    group_by(fecha, ducon1) |> 
    summarise(total = sum(factorel)) |> 
    mutate(pct = total / sum(total)*100) |> 
    filter(ducon1 == 6) |> 
    select(fecha, temporalidad = pct)

paro <- data |> 
    filter(aoi %in% c(3:6)) |> 
    mutate(parados = ifelse(aoi >= 5, 1, 0)) |> 
    group_by(fecha) |> 
    summarise(paro = (weighted.mean(parados, factorel))*100)

temp_paro <- merge(temp, paro)

openxlsx::write.xlsx(temp_paro, 'output/temp_paro.xlsx')
#' Evolución de la temporalidad pública y privada -----------------------------
#' Evolución del empleo publico y el empleo privado ---------------------------
#' Razones por las que una persona no ha trabajado (generos) ------------------
#' Personas del empleo al desempleo por sectores ------------------------------
#' Distribución de genero por tipo de sector ----------------------------------

dist <- data |> 
    filter(aoi %in% c(3:4)) |> 
    filter(!is.na(sexo1)) |> 
    filter(fecha == '2023T2') |> 
    group_by(act1, sexo1) |> 
    summarise(total = sum(factorel)) |> 
    mutate(pct = total / sum(total)) |> 
    mutate(sexo1 = ifelse(sexo1 == 1, 'Hombre', 'Mujer')) |> 
    mutate(sector = case_match(act1,
            0 ~ 'Agricultura, ganaderia, silvicultura y pesca',
            1 ~ 'Industria alimentaria, textil, cuero, madera y papel',
            2 ~ 'Industrias extractivas',
            3 ~ 'Industrias extractivas',
            4 ~ 'Construcción',
            5 ~ 'Comercio al por mayor y al por menor',
            6 ~ 'Transporte y almacenamiento. Información y comunicaciones',
            7 ~ 'Intermediación financiera, seguros, actividades inmobiliarias',
            8 ~ 'Administración Pública, educación y actividades sanitarias',
            9 ~ 'Otros servicios')) |> 
    ungroup() |> 
    select(sexo1, sector, pct)

openxlsx::write.xlsx(dist, 'output/genero_sector.xlsx')


nacimiento <- data |> 
    filter(aoi %in% c(3:4)) |> 
    filter(rznotb %in% c(2:3)) |> 
    group_by(fecha, sexo1) |> 
    summarise(total = sum(factorel)) |> 
    mutate(sexo1 = ifelse(sexo1==1, 'Hombre', 'Mujer')) |> 
    select(fecha, sexo1, total) |> 
    pivot_wider(names_from = sexo1, values_from = total)

openxlsx::write.xlsx(nacimiento, 'output/nacimiento_total.xlsx')
