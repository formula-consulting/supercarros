library(tidyverse)
library(rvest)
library(lubridate)
library(polite)

# extrae url de los anuncios
get_url_anuncios <- function() {
  
  pages_recientes <- paste0("https://www.supercarros.com/?recentsPage=",
                 0:99)
  
  pages_todos <- paste0("https://www.supercarros.com/carros/?PagingPageSkip=",
                       1:41)
  
  html_recientes <- map(pages_recientes, read_html)
  
  html_todos <- map(pages_todos, read_html)
  
  url_recientes <- map(
    html_recientes,
    ~html_nodes(.x, "#RecentsContainer ul li.normal a") %>%
      html_attr("href")
    ) %>% unlist()
  
  url_todos <- map(
    html_todos,
    ~html_nodes(.x, "#bigsearch-results-inner-results ul li.normal a") %>% 
      html_attr("href")
  ) %>% unlist()
  
  urls_carros <- unique(c(url_recientes, url_todos))
  
  return(urls_carros)
}

get_url_by_years <- function(start_year = 2000, end_year = 2021) {
  
  get_pages <- function(year) {
    url <- paste0("https://www.supercarros.com/carros/", "?YearFrom=", year, "&YearTo=", year)
    
    pages_skyps <- read_html(url) %>% 
      html_nodes("#UpperCounter2") %>% 
      html_text() %>% 
      parse_number() %>% 
      `/`(24) %>% 
      ceiling()
    
    pages_skyps <- seq(0, length.out = pages_skyps)
    
    pages <- paste0(url, "&PagingPageSkip=", pages_skyps)
    
    return(pages)
  }
  
  get_urls <- function(page_url) {
    
    html_page <- read_html(page_url)
    
    url_anuncios <- html_nodes(html_page, "#bigsearch-results-inner-results ul li.normal a") %>% 
      html_attr("href")
    
    return(url_anuncios)
    
  }
  
  year_sequence <- seq(start_year, end_year)
  
  page_url <- map(year_sequence, get_pages) %>% unlist()
  
  url_anuncios <- map(page_url, get_urls) %>% 
    unlist()
  
  return(url_anuncios)
  
}

# extrae data de los vehículos
get_cars_data <- function(url_anuncio) {
  
  html_anuncio <- read_html(paste0("https://www.supercarros.com", url_anuncio))
  
  # scraping date
  scraping_date <- Sys.Date()
  
  # marca modelo año
  marca_modelo_year <-  html_anuncio %>% 
    html_nodes("#detail-ad-header h1") %>% 
    html_text()
  
  # precio 
  precio <- html_anuncio %>% 
    html_nodes("#detail-ad-header h3") %>% 
    html_text()
  
  # transmisión
  transmision <- html_anuncio %>% 
    html_nodes("tr:nth-child(6) td:nth-child(2)") %>% 
    html_text()
  
  
  # tracción
  traccion <- html_anuncio %>% 
    html_nodes("tr:nth-child(7) td:nth-child(2)") %>% 
    html_text()
  
  # Combustible
  combustible <- html_anuncio %>% 
    html_nodes(".detail-ad-info-specs-block strong:nth-child(4)") %>% 
    html_text()
  
  # uso
  uso <- html_anuncio %>% 
    html_nodes("tr:nth-child(4) td:nth-child(4)") %>% 
    html_text()
  
  # color
  color <- html_anuncio %>% 
    html_nodes("tr:nth-child(3) td:nth-child(2)") %>% 
    html_text()
  
  # Cilindros motor
  cilindros <- html_anuncio %>% 
    html_nodes("tr:nth-child(2) td+ td") %>% 
    html_text()
  
  # Color interior
  color_interior <- html_anuncio %>% 
    html_nodes("tr:nth-child(4) td:nth-child(2)") %>% 
    html_text()
  
  # tipo de vehículo
 tipo_vehiculo <- html_anuncio %>% 
    html_nodes("tr:nth-child(3) td:nth-child(4)") %>% 
    html_text()
  
  # puertas
  puertas <- html_anuncio %>% 
    html_nodes("tr:nth-child(6) td:nth-child(4)") %>% 
    html_text()
  
  # pasajeros
  pasajeros <- html_anuncio %>% 
    html_nodes("tr:nth-child(7) td:nth-child(4)") %>% 
    html_text()
  
  # accesorios
  accesorios <- html_anuncio %>% 
    html_nodes(".detail-ad-info-specs-block ul li") %>% 
    html_text() %>% 
    paste(collapse = "; ")
  
  # ubicación gps
  ubicacion_gps <-  html_anuncio %>% 
    html_nodes(".map iframe") %>% 
    html_attr("src") %>% 
    str_extract("[0-9]+\\.[0-9]+,-[0-9]+.[0-9]+")
  
  # vendedor
  vendedor <- html_anuncio %>% 
    html_nodes("#detail-right h3") %>% 
    html_text()
  
  # detalles vendedor
  detalles_vendedor <- html_anuncio %>% 
    html_nodes("#detail-right ul li") %>% 
    html_text() %>% 
    str_subset(':') %>% 
    paste0(collapse = " --;;-- ")
  
  # detalle dirección
  detalles_direccion <- html_anuncio %>% 
    html_nodes(".address") %>% 
    html_text()
  
  url <- url_anuncio
  
  data_anuncio <- tibble::tibble(
    scraping_date, marca_modelo_year, precio, transmision, traccion,
    combustible, color, cilindros, color_interior, tipo_vehiculo, puertas,
    pasajeros, uso, accesorios, vendedor, detalles_vendedor, detalles_direccion, 
    ubicacion_gps, url
    )
  
  return(data_anuncio)
}
