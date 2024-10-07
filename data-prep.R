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

#* pre-calculate all adjacent tracts

source("helper-functions.R")
res_prop <- arrow::read_parquet("res_prop.parquet")
phl_tracts <- sfarrow::st_read_parquet("phl_tracts.parquet")

all_tracts <-
  sort(unique(res_prop$census_tract))

get_all_touching_tracts <-
  function(tract, all_tracts) {
    index_tract <-
      all_tracts |>
      filter(NAME %in% c(
        tract,
        # allow for mismatched tracts
        tract + 0.01,
        tract + 0.02
      ))

    intersect_test <-
      bind_cols(
        sf::st_touches(x = all_tracts$geometry, y = index_tract$geometry, sparse = FALSE) |>
          as_tibble() |>
          rename(adjacent_tracts = 1),
        all_tracts
      )

    touching_tracts <-
      intersect_test |>
      filter(adjacent_tracts == TRUE) %>%
      mutate(input_tract = tract) %>%
      select(input_tract, adjacent_tract = NAME)
    # pull(NAME)

    return(touching_tracts)
  }

touching_tracts <-
  purrr::map_dfr(all_tracts[1:315], ~ get_all_touching_tracts(tract = .x, all_tracts = phl_tracts))

write_parquet(touching_tracts, "touching_tracts.parquet")
