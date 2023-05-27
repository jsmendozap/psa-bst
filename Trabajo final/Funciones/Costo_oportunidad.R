calculo_costo <- function(bd){
  
  nombres <- c('cultivos', 'avicultura', 'ganaderia', 'forestal')
  
  flujos_caja <- read_excel('flujos_caja.xlsx') %>%
    rename_with(.fn = ~paste('utilidad', nombres, sep = '_'), .cols = c(7, 11, 15, 19)) %>%  
    select(1:2, starts_with('utilidad_')) %>%
    rename(departamento = 1) %>%
    mutate(across(.cols = 1:2, .fns = ~formato(.x))) %>%
    pivot_longer(cols = 3:6, names_to = 'actividad',  values_to = 'utilidad') %>%
    mutate(actividad = str_sub(actividad, start = 10), 
           utilidad = utilidad/4500) %>%
    left_join(select(bst, 1:3), by = join_by(departamento, municipio))
  
  resultado <- bd %>% 
    select(1:2, 8, 10:16) %>% 
    rename_with(.fn = ~str_sub(., start = 6), .cols = c(3, 7:9)) %>%    
    pivot_longer(cols = 3:9, names_to = c('.value', 'actividad'), names_sep = '_') %>% 
    mutate(aptitud = formato(aptitud))
    
  no_cultivos <- resultado %>% filter(aptitud == 'aptitud alta') %>% 
    left_join(flujos_caja %>% st_drop_geometry() %>% select(-geom),
              by = join_by('departamento', 'municipio', 'actividad')) %>% 
    select(-6) %>%
    group_by(departamento, municipio) %>%
    slice_max(utilidad)
  
  resultado %<>%
    filter(actividad == 'cult') %>%
    mutate(actividad = 'cultivos') %>%
    left_join(flujos_caja %>% st_drop_geometry() %>% select(-geom),
              by = join_by('departamento', 'municipio', 'actividad')) %>%
    anti_join(y = no_cultivos %>% st_drop_geometry(),
              by = join_by('departamento', 'municipio')) %>% 
    select(-6) %>%
    bind_rows(no_cultivos) %>%
    mutate(c_oportunidad = cut(x = utilidad,
                               breaks = quantile(utilidad, probs = seq(0, 1, 0.25)),
                               labels = c('Bajo', 'Medio', 'Alto', 'Muy Alto'),
                               include.lowest = T)) %>%
    filter(!municipio %in% c('uribia', 'puerto colombia')) %>%
    arrange(utilidad) 
  
  rm(list = c('flujos_caja', 'no_cultivos', 'nombres'))
  return(resultado)
}
