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
#' 
devtools::install_github('esadeecpol/esadeecpol', force = TRUE)
print_colors()
