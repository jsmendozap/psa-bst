pacman::p_load(tidyverse, stringi, sf, qgisprocess, furrr, units, magrittr,
               readxl, geojsonsf, rmapshaper)

## Lectura de archivos

formato <- function(x){
  stri_trans_general(str = x, id = 'Latin-ASCII') %>%
    str_to_lower(locale = 'es')
}

bst <- st_read(dsn = 'Shapes.gpkg', layer = 'Bosque_Seco_Tropical') %>%
  st_transform(crs = 'epsg:9377')

municipios <- st_read('Shapes.gpkg', layer = 'Municipios_Colombia') %>%
  st_transform(crs = 9377) %>%
  select(2, 4) %>%
  arrange(nombre_dep, nom_munici) %>%
  mutate(across(.cols = 1:2, .fns = ~formato(.x))) %>%
  rename(municipio = 1, departamento = 2)

bst <- st_intersection(x = bst, y = municipios) %>%
  group_by(departamento, municipio) %>%
  summarise(n = n()) %>%
  select('departamento', 'municipio', 'geom') %>%
  mutate(mun = str_c(municipio, departamento, sep = ', ')) %>%
  ungroup()

bst$area_bst <- st_area(bst) %>% set_units('ha')

## Selección de municipios que contienen bst

municipios %<>%
  mutate(mun = str_c(municipio, departamento, sep = ', '),
         area_mun = st_area(.) %>% set_units('ha')) %>%
  filter(mun %in% unique(bst$mun))

bst$prop_area_mun <- as.numeric(bst$area_bst/municipios$area_mun) * 100 

## Agregando información de cultivos

cultivos <- read.csv("Cultivos.csv", sep = ';', fileEncoding = 'latin1') %>%
  mutate(across(.cols = 1:3, .fns = ~formato(.x))) %>% 
  filter(str_c(municipio, departamento, sep = ', ') %in% municipios$mun &
         año == 2018) %>%
  select(1:3, 5) %>% 
  group_by(departamento, municipio) %>%
  mutate(prop_area_cult = area_sembrada_ha/sum(area_sembrada_ha)) %>%
  summarise(n_cultivos = n(), area_sembrada = sum(area_sembrada_ha),
            prop_area_cult = max(prop_area_cult), cultivo = cultivo[1]) %>%
  rbind(data.frame(departamento = c('atlantico', 'la guajira', 'norte de santander'),
                   municipio = c('puerto colombia', 'uribia', 'jurisdiccion de ocaña'),
                   n_cultivos = rep(-999, 3), area_sembrada = rep(-999, 3), 
                   prop_area_cult = rep(-999, 3), cultivo = 'No aplica')) %>%
  arrange(departamento, municipio)

bst %<>%
  left_join(cultivos, by = join_by('departamento', 'municipio')) %>% 
  arrange(departamento, municipio)

rm(cultivos)

## Información de avicultura, ganaderia y forestal

source('Funciones/Procesamiento.R')

plan(multisession, workers = 6)

aptitud <- future_map2_dfr(.x = str_subset(st_layers('Shapes.gpkg')$name, 'Aptitud'),
                           .y = c('avicultura', 'ganaderia', 'forestal'),
                           .f = ~procesamiento(archivo = .x, nombre = .y)) %>%
           pivot_wider(names_from = 3, values_from = 4:5)

bst %<>%
  left_join(aptitud, by = join_by('departamento', 'municipio'))

rm(aptitud)

## Retirando los PNN

pnn <- st_read(dsn = 'Shapes.gpkg', layer = 'PNN') %>%
  st_transform(crs = 'epsg:9377')

diferencia <- grep('difference', qgis_algorithms()$algorithm, value = T)[1]
resultado <- qgis_run_algorithm(algorithm = diferencia,
                                INPUT = bst,
                                OVERLAY = pnn,
                                OUTPUT = tempdir())

bst <- st_as_sf(resultado)

rm(list = c("diferencia", "resultado", "pnn"))

## Flujos de caja y selección de actividad

source('Funciones/Costo_oportunidad.R')

psa <- calculo_costo(bd = bst) %>%
  arrange(utilidad, desc(area))

# Guardando resultados parte 1

regiones <- st_read('Shapes.gpkg', layer = 'regiones') %>%
  mutate(region = formato(REGION)) %>%
  select(3)

psa %>%
  st_transform(crs = 4326) %>%
  left_join(y = bst %>% select(1:2, 4) %>% st_drop_geometry(),
            by = join_by('departamento', 'municipio')) %>%
  st_join(y = regiones, join = st_intersects) %>% 
  st_drop_geometry() %>% 
  distinct(mun, .keep_all = T) %>%
  write.csv(file = 'resumen.csv', row.names = F)

save(municipios, bst, psa, regiones, file = 'resultados.rda')

rm(municipios, bst)
st_write(obj = psa, dsn = 'Shapes.gpkg', layer = 'costo_oportunidad_ha', append = T)

## Simplificando geometrias 

psa %<>%
  st_transform(crs = 4326) %>%
  sf_geojson() %>%
  ms_simplify(keep_shapes = T, explode = T) %>%
  geojson_sf()

## Guardando resultados parte 2

st_write(obj = psa, dsn = 'Resultados.geojson')
