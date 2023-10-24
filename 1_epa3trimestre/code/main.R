library(esadeecpol)


#' Importamos los microdatos de la EPA --------------------------------------
dir <- getwd()
setwd('1_epa3trimestre/input')
files <- list.files(pattern = '\\.csv$')
data <- data.table::rbindlist(lapply(files, data.table::fread), fill = TRUE)
ciclos <- readxl::read_excel('Registro.xlsx', sheet = 'Tablas5')
data <- merge(data, ciclos, by = 'ciclo')
setwd(dir)
rm(ciclos, files, dir)
#' ----------------------------------------------------------------------------


#' COLUMNA DE LA ENCUESTA DE POBLACIÓN ACTIVA 2023T3
#' ----------------------------------------------------------------------------
setwd('1_epa3trimestre')

#' 1) Como ha evolucionado los ocupados y los contratos laborales -------------
ocu <- data |> 
    filter(aoi %in% c(3:4)) |> 
    group_by(fecha) |> 
    summarise(ocupados = sum(factorel)/1000000)

contr <- data |> 
    filter(aoi %in% c(3:4)) |> 
    filter(situ %in% c(7:8)) |> 
    group_by(fecha, ducon1) |> 
    summarise(total = sum(factorel)/1000000) |> 
    mutate(ducon1 = ifelse(ducon1 == 1, 'Indefinido', 'Temporal')) |> 
    pivot_wider(names_from = ducon1, values_from = total)

t <- merge(ocu, contr)
write.csv(t, 'output/ocupados.csv', row.names = F)
openxlsx::write.xlsx(t, 'output/ocupados.xlsx')
#' ----------------------------------------------------------------------------


#' 2) Composición de los asalariados con contrato indefindo -------------------

indef <- data |> 
    filter(aoi %in% c(3:4)) |> 
    filter(situ %in% c(7:8)) |> 
    filter(ducon1 == 1) |> 
    group_by(fecha, ducon2) |> 
    summarise(total = sum(factorel)) |> 
    mutate(ducon2 = ifelse(ducon2 == 1, 'Permanente', 'Discontinuo')) |> 
    pivot_wider(names_from = ducon2, values_from = total)

write.csv(indef, 'output/composicion_indef.csv', row.names = F)

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
#' ----------------------------------------------------------------------------


#' Evolución de la temporalidad pública y privada -----------------------------
#' Composición de los contratos indefinidos -----------------------------------



