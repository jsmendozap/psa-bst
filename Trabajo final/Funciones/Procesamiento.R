## Función de procesamiento

procesamiento <- function(archivo, nombre){
  
  archivo <- st_read(dsn = 'Shapes.gpkg', layer = archivo)
  
  if(st_crs(archivo)$epsg != st_crs(bst)$epsg){
    archivo <- st_transform(x = archivo, crs = st_crs(bst))
  }
  
  ## Intersección y organización de resultados
  aptitud <- st_intersection(x = bst, y = archivo) %>%
    group_by(departamento, municipio, aptitud, mun) %>%
    summarise(n = n()) %>%
    select(-n) %>%
    mutate(actividad = nombre)
  
  ## Cálculo de áreas y proporciones
  aptitud$area <- st_area(aptitud) %>% set_units('ha')
  
  mun <- municipios %>% 
    st_drop_geometry() %>%
    select(mun, area_mun)
  
  aptitud <- aptitud %>%
    left_join(mun, by = join_by(mun)) %>%
    mutate(prop_area = as.numeric(area/area_mun)) %>%
    group_by(departamento, municipio) %>%
    slice_max(prop_area) 
  
  aptitud <- aptitud %>% st_drop_geometry() %>% select('actividad', 'aptitud', 'prop_area')
  rm('archivo', 'mun')
  
  return(aptitud)
}
