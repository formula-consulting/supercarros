# Script con las funciones de trabajo
source("scripts/functions.R")

# Importando url históricas
url_historicas <- read_rds("data/url_historicas.rds")

# url nuevas
url_fetch_by_year <- get_url_by_years()

# comprobando cuáles de las urls nuevas no están en el histórico 
url_nuevas <- url_fetch_by_year[!url_fetch_by_year %in% url_historicas]

# getting new data
data_nueva <- map(url_nuevas, ~get_cars_data(.x))


# old_way for the first time

data_nueva <- vector(length = length(url_nuevas), mode = "list")

for (i in seq_along(url_nuevas)) {
  
  data_nueva[[i]] <- get_cars_data(url_nuevas[i])
  
  percent <- round(i/length(url_nuevas), 3) * 100
  
  print(paste0("Iteración: ", i,"; ", percent, "%"))
}

data_nueva <- bind_rows(data_nueva)

# data histórica
data_historica <- read_rds("data/data_historica.rds")

# agregando la data nueva al histórico
data_historica <- bind_rows(data_historica, data_nueva)

# agregando url_nuevas al histórico
url_historicas <- c(url_historicas, url_nuevas) %>% unique()

# guardando archivos históricos
write_rds(url_historicas, "data/url_historicas.rds")
write_rds(data_historica, "data/data_historica.rds")

# data en excel
xlsx::write.xlsx(data_historica, "data/data_sc.xls")
