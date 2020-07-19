source("scripts/functions.R")

url_historicas <- read_rds("data/url_historicas.rds")

url_nuevas <- get_url_anuncios()

url_nuevas <- url_nuevas[!url_nuevas %in% url_historicas]


data_nueva <- map(url_nuevas, ~get_cars_data(.x))

data_nueva <- bind_rows(data_nueva)



data_historica <- read_rds("data/data_historica.rds")

data_historica <- bind_rows(data_historica, data_nueva)
url_historicas <- c(url_historicas, url_nuevas)

# 
write_rds(url_historicas, "data/url_historicas.rds")
write_rds(data_historica, "data/data_historica.rds")

xlsx::write.xlsx(data_historica, "data/data_sc.xls")


