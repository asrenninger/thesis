########################################################
## Wrangling the data
## 8 years of health
########################################################

library(tidyverse)
library(janitor)
library(sf)
library(tigris)
library(tidycensus)
library(geofacet)
library(scales)
library(glue)
library(fs)

## important variables
vars <- c("FIPS", "State", "County",
          "Years of Potential Life Lost Rate", "% Fair/Poor",
          "Physically Unhealthy Days", "Mentally Unhealthy Days")


## read in the data
left <- map_df(2010:2018, function(x){
  
  if (x < 2015) {
    glue("data/rwj/{x}.csv") %>%
      read_csv(n_max = 3142) %>% 
      rename(`Years of Potential Life Lost Rate` = `YPLL Rate`) %>%
      select(vars) %>%
      mutate(year = x) } 
  else {
    glue("data/rwj/{x}.csv") %>%
      read_csv(n_max = 3142) %>%
      select(vars) %>%
      mutate(year = x)
  }
  
})

left <-
  left  %>% 
  clean_names() %>% 
  rename(GEOID = fips)

## attach geographies
counties <- counties(cb = TRUE, class = 'sf')
counties <- 
  counties %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(counties) %>% 
  transmute(GEOID, X, Y, geometry) %>%
  st_as_sf()

## grab zonal statistics
files <- dir_ls("data/usgs")
files <- files[str_detect(files, ".txt")]

landcover <- map_df(files, function(x){
  read_csv(x) %>%
    transmute(GEOID, 
              variable = x,
              value = AREA) %>%
    mutate(variable = str_remove_all(variable, "data/usgs/"),
           variable = str_remove_all(variable, ".txt"))
})

landcover <- 
  landcover %>%
  left_join(counties) %>%
  mutate(area = units::drop_units(st_area(geometry))) %>% 
  st_as_sf() %>%
  transmute(GEOID, 
            variable,
            value = value / area) %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = variable, values_from = value)

## join census data
vars <- load_variables(2018, 'acs5')
test <- vars %>% filter(str_detect(str_to_lower(label), "gini"))

labs <- tribble(~variable, ~name,
                "B01001_001", "population",
                "B06011_001", "median_income",
                "B06009_005", "college_degree",
                "B19083_001", "gini",
                "B02001_002", "white",
                "B02001_003", "black",
                "C18120_002", "labor_force",
                "C18120_003", "employed",
                "C18120_006", "unemployed",
                "B27010_017", "health_insurance_1",
                "B27010_033", "health_insurance_2",
                "B27010_050", "health_insurance_3",
                "B27010_066", "health_insurance_4",
                "B27010_001", "health_insurance_total")

# demography <- 
#   reduce(
#     map(2014:2018, 
#         function(x){
#           get_acs(geography = 'county', variables = labs$variable, year = x) %>%
#             mutate(year = x )
#         }), 
#     rbind
#   )

demography <- get_acs(geography = 'county', variables = labs$variable, year = 2018)

right <- 
  demography %>% 
  left_join(labs) %>% 
  transmute(GEOID, 
            variable = name,
            value = estimate) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  left_join(counties) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry),
         area = units::set_units(area, km2)) %>%
  transmute(GEOID, 
            population, 
            density = units::drop_units(population / area), 
            nonwhite = white / population,
            college = college_degree / population, 
            unemployment = unemployed / labor_force,
            income = median_income,
            gini,
            uninsured = (health_insurance_1 + health_insurance_2 + health_insurance_3 + health_insurance_4) / health_insurance_total,
            X, Y, 
            geometry)

full <- 
  left %>%
  filter(year == 2018) %>% 
  left_join(right) %>%
  filter(!str_detect(state, "Alaska|Hawaii")) %>%
  st_as_sf()

## use spatial weights for imputation
play <- 
  full %>%
  st_transform(2163) %>% 
  arrange(Y, X) %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname)) %>%
  select(-X, -Y) %>%
  st_as_sf() 

frst_degree <- 
  play %>% 
  st_touches() %>% 
  tibble() %>%
  rownames_to_column() %>%
  clean_names() %>%
  unnest(x) %>%
  transmute(row_id = as.numeric(rowname),
            col_id = x)

## one way
scnd_degree <- 
  frst_degree %>%
  rename(id = row_id,
         row_id = col_id) %>%
  left_join(frst_degree) %>%
  transmute(id = id,
            row_id = col_id) %>%
  left_join(frst_degree) %>%
  transmute(id = id,
            rowname = col_id) %>%
  group_by(id) %>%
  distinct(rowname, .keep_all = TRUE) %>%
  ungroup() %>%
  left_join(play) %>%
  st_as_sf()

## another way
scnd_degree <-
  frst_degree %>%
  rename(id = row_id,
         row_id = col_id) %>%
  left_join(frst_degree) %>%
  transmute(id = id,
            rowname = col_id) %>%
  group_by(id) %>%
  distinct(rowname, .keep_all = TRUE) %>%
  ungroup() %>%
  left_join(play) %>%
  st_as_sf()

geom <- st_geometry(scnd_degree)

final <- 
  scnd_degree %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(scnd_degree) %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  mutate(x = abs(X - mean(X)),
         y = abs(Y - mean(Y))) %>%
  mutate(score = (x + y) / 2) %>%
  ungroup() %>% 
  mutate(geometry = geom) %>%
  st_as_sf()

## packages for a quick demonstration
library(scico)
library(gganimate)

## make a background
background <- 
  full %>%
  st_transform(2163) %>%
  st_union() %>%
  st_combine()

## animate it
windows <- 
  ggplot(final) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = score), show.legend = FALSE, size = 0, colour = NA) +
  scale_fill_scico(palette = 'buda', direction = -1) +
  transition_manual(id) +
  ease_aes('linear') +
  theme_map()

anim_save(windows, filename = "gwr.gif", fps = 3)

## grab data from around each cell
nn_interpolate <- function(data, depth = 5) {
  
  play <-
    data %>%
    st_transform(2163) %>% 
    arrange(Y, X) %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname)) %>%
    select(-X, -Y) %>%
    st_as_sf() 
  
  crosswalk <- 
    play %>% 
    transmute(id = rowname, 
              GEOID)
    
  frst_degree <- 
    play %>% 
    st_touches() %>% 
    tibble() %>%
    rownames_to_column() %>%
    clean_names() %>%
    unnest(x) %>%
    transmute(row_id = as.numeric(rowname),
              col_id = x)
  
  scnd_degree <- frst_degree %>% 
    rename(id = row_id,
           row_id = col_id) %>%
    left_join(frst_degree) 
  
  for (i in 1:depth) {
    
    scnd_degree <-
      scnd_degree %>%
      transmute(id = id,
                row_id = col_id) %>%
      left_join(frst_degree)
    
  }
  
  final <-
    scnd_degree %>%
    transmute(id = id,
              rowname = col_id) %>%
    group_by(id) %>%
    distinct(rowname, .keep_all = TRUE) %>%
    ungroup() %>%
    left_join(play) %>%
    st_as_sf() %>% 
    select(-rowname) %>%
    st_drop_geometry() %>%
    group_by(id, year) %>%
    summarise_if(is.numeric, ~mean(.x, na.rm = TRUE)) %>% 
    ungroup() %>%
    left_join(crosswalk) %>%
    select(-id) %>%
    select(GEOID, everything())
  
  return(final)
  
}

## smoothing
infill <- nn_interpolate(full, 3) 
