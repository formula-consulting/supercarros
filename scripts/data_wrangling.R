# paquetes 
library(tidyverse)
library(rvest)


read_html("https://www.supercarros.com/") %>% 
  html_nodes("tr:nth-child(2) td+ td option") %>% 
  html_text()

marcas <- '
<option value="36">Acura</option>
<option value="29">Alfa Romeo</option>
<option value="19">Audi</option>
<option value="591">Autiny</option>
<option value="577">Baic</option>
<option value="473">Bajaj</option>
<option value="157">Bentley</option>
<option value="17">BMW</option>
<option value="283">Brilliance</option>
<option value="39">Buick</option><
option value="303">BYD</option>
<option value="23">Cadillac</option>
<option value="145">Chana</option>
<option value="310">Changan</option>
<option value="187">Chery</option>
<option value="34">Chevrolet</option>
<option value="32">Chrysler</option>
<option value="31">Citroen</option>
<option value="55">Daewoo</option>
<option value="35">Daihatsu</option>
<option value="189">DFM</option>
<option value="46">Dodge</option>
<option value="186">Dongfeng</option>
<option value="78">Ferrari</option>
<option value="58">Fiat</option>
<option value="45">Ford</option>
<option value="47">GMC</option>
<option value="583">Higer</option>
<option value="27">Honda</option>
<option value="102">Hummer</option>
<option value="42">Hyundai</option>
<option value="53">Infiniti</option>
<option value="44">Isuzu</option>
<option value="180">Jac</option>
<option value="18">Jaguar</option>
<option value="64">Jeep</option>
<option value="216">Jin-Bei</option>
<option value="167">JMC</option>
<option value="622">Kaiyun</option>
<option value="49">Kia</option>
<option value="90">Lamborghini</option>
<option value="37">Land Rover</option>
<option value="605">Landwind</option>
<option value="33">Lexus</option>
<option value="62">Lincoln</option>
<option value="357">Linfan</option>
<option value="381">Lotus</option>
<option value="123">Mahindra</option>
<option value="141">Maserati</option>
<option value="322">Maxus</option>
<option value="40">Mazda</option>
<option value="532">McLaren</option>
<option value="24">Mercedes-Benz</option>
<option value="76">Mercury</option>
<option value="103">MG</option>
<option value="114">Mini</option>
<option value="21">Mitsubishi</option>
<option value="28">Nissan</option>
<option value="41">Peugeot</option>
<option value="69">Plymouth</option>
<option value="38">Pontiac</option>
<option value="25">Porsche</option>
<option value="618">Ram</option>
<option value="20">Renault</option>
<option value="264">Rolls Royce</option>
<option value="107">Samsung</option>
<option value="262">Saturn</option>
<option value="77">Seat</option>
<option value="399">Shineray</option>
<option value="22">Skoda</option>
<option value="422">Smart</option>
<option value="59">SsangYong</option>
<option value="63">Subaru</option>
<option value="30">Suzuki</option>
<option value="566">Tesla</option>
<option value="26">Toyota</option>
<option value="612">Victory</option>
<option value="43">Volkswagen</option>
<option value="54">Volvo</option>'


marcas <- str_remove_all(marcas, "\n")
marcas <- str_remove_all(marcas, '<option value=\"[0-9]+\">')

marcas <- str_replace_all(marcas, '</option>', ", ")

marcas <- str_split(marcas, ", ") %>% unlist()

data_sc <- read_rds("data/data_historica.rds")

pattern_marcas <- paste(marcas, collapse = "|")



data_sc <- 
  data_sc %>% 
  mutate(marca = str_extract(marca_modelo_year, pattern = pattern_marcas),
         year = str_extract(marca_modelo_year, "[0-9]+$"),
         modelo = str_remove(marca_modelo_year, pattern = pattern_marcas),
         modelo = str_remove(modelo, " [0-9]+$"),
         modelo = str_trim(modelo)) %>%
  separate(precio, into = c("moneda", "precio"), sep = " ") %>%
  separate(ubicacion_gps, into = c("lng", "lat"), sep = ",") %>% 
  mutate(
    precio = parse_number(precio),
    year = parse_number(year)) %>%
  rowid_to_column(var = "id") %>% 
  separate_rows(accesorios, sep = "; ") %>% 
    mutate(temp = 1, accesorios = ifelse(accesorios == '', "sin accesorios", accesorios)) %>% 
    filter( str_detect(accesorios, '^[0-9\\.]+$', negate = TRUE) | accesorios == '') %>% 
    filter(str_detect(accesorios, "^$", negate = TRUE)) %>% 
    pivot_wider(names_from = accesorios, values_from = temp, values_fill = 0) %>% 
    select(id, scraping_date, marca, modelo, year, moneda, precio, everything())

data_sc <- data_sc %>% 
  mutate(
    direccion = str_extract(detalles_vendedor, "Ciudad:.+"),
    direccion = str_remove(direccion, "--;;--")) %>% 
  separate_rows(detalles_vendedor, sep = "--;;--") %>% 
  mutate(detalles_vendedor = str_squish(detalles_vendedor)) %>% 
  filter(str_detect(detalles_vendedor, "^Tel:|^WhatsApp:|^Tel 2:|^Cel:")) %>% 
  separate(detalles_vendedor, c("contacto", "valor"), sep = ": ") %>% 
  pivot_wider(names_from = contacto, values_from = valor) %>% 
  janitor::clean_names() 

data_sc <- data_sc %>% 
  select(id, scraping_date, marca, modelo, tipo_vehiculo, year, moneda, precio, 
         transmision, traccion, combustible, motor = cilindros, color,  
         color_interior, puertas, pasajeros, uso, vendedor, lng, lat, 
         direccion, detalles_direccion, tel, whats_app, tel_2, cel, url, 
         everything()) %>% 
  mutate(
    tel = map_chr(tel, ~paste(.x, collapse = ":")),
    whats_app = map_chr(whats_app, ~paste(.x, collapse = ":")),
    tel_2 = map_chr(tel_2, ~paste(.x, collapse = ":")),
    cel = map_chr(cel, ~paste(.x, collapse = ":"))) 
  

xlsx::write.xlsx(data_sc, "data/clean_data/data_sc.xls")
