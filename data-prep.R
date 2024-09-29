library(tidyverse)
library(reactable)
library(tidycensus)
library(censusapi)
library(furrr)
library(parallel)
library(doParallel)
library(sf)
library(arrow)

#* create 'res_prop' which is residential properties only

# load property data
properties <- arrow::read_parquet("data/properties.parquet")

# filter for residential properties only
res_prop <-
  properties %>%
  filter(
    category_code %in% c(
      1, # residential
      2, # multi family
      14 # apartments > 4 units
    )
  )

#* add lat longs to res_prop

# load properties file geodatabase
opa_sf <- sf::st_read("data/opa_properties_public.geojson")

# extract property IDs
prop_ids <-
  res_prop |>
  select(parcel_number)

# get property lat longs
prop_lat_long <-
  bind_cols(
    prop_ids %>% arrange(parcel_number),
    opa_sf %>%
      arrange(parcel_number) %>%
      mutate(category_code = as.numeric(trimws(category_code))) %>%
      filter(
        category_code %in% c(
          1, # residential
          2, # multi family
          14 # apartments > 4 units
        )
      ) %>%
      sf::st_as_sf(coords = "geometry") %>%
      sf::st_coordinates() %>%
      as_tibble()
  ) %>%
  rename(
    longitude = X,
    lattitude = Y
  )

# join lat long back to res props
res_prop <-
  res_prop %>%
  left_join(., prop_lat_long,
    by = join_by("parcel_number")
  )

#* save res_prop
write_parquet(res_prop, "data/res_prop.parquet")

#* census tracts
# load data
phl_tracts <- tigris::tracts("PA", year = 2022, county = "Philadelphia")

# save data
sfarrow::st_write_parquet(phl_tracts, "data/phl_tracts.parquet")
