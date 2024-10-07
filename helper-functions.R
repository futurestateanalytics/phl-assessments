library(tidyverse)
library(MatchIt)
library(highcharter)
library(reactable)
library(leaflet)
library(duckdb)
library(bslib)
library(bsicons)
library(shiny)
library(shinycssloaders)
library(crosstalk)
library(shinyjs)
library(plotly)
library(shinyWidgets)
library(randomForest)
library(showtext)
library(sysfonts)
library(showtextdb)
library(pool)


my_theme <- bs_theme(
  bg = "#fbf7f5",
  fg = "#404248",
  primary = "#3b7f4d",
  info = "rgb(178, 191, 201)",
  font_scale = 0.9,
  base_font = font_google("Space Mono"),
  code_font = font_google("Space Mono"),
  quiet = TRUE
)

# install_duck_ext <-
#   function(connection) {
#     on.exit(poolReturn(connection))

#     # Install and load the DuckDB extension
#     DBI::dbExecute(connection, "INSTALL httpfs;")
#     DBI::dbExecute(connection, "LOAD httpfs;")
#   }

#* app functions
get_loc_names <-
  function(location, connection) {
    on.exit(pool::poolReturn(connection))

    # DuckDB can read files from folder
    # prop_path <- "https://phl-assessments.nyc3.digitaloceanspaces.com/res_prop.parquet"
    prop_path <- "res_prop.parquet"

    # SQL statement to perform data aggregation
    # String interpolation is used to inject dynamic string parts into the query
    loc_query <- str_interp("
      select
        *
      from
        read_parquet('${prop_path}')
      where
        lower(location) like ('%${tolower(location)}%')
      order by
        location asc
        ")

    # Run the query with DuckDB in memory

    res <- dbGetQuery(
      conn = connection,
      statement = loc_query
    )

    return(res)
  }

#* viz functions ----------------------

hc_pal <- c(
  "#FF6347", "#1e90ff", "#E18727FF",
  "#20854EFF", "#7876B1FF", "#6F99ADFF", "#FFDC91FF", "#EE4C97FF"
)

get_data_dict_table <-
  function(connection) {
    on.exit(poolReturn(connection))

    # DuckDB can read files from folder by using a glob pattern
    field_path <- here::here("fields.parquet")

    # SQL statement to perform data aggregation
    # String interpolation is used to inject dynamic string parts into the query
    fields_query <- str_interp("
      select
        *
      from
        read_parquet('${field_path}')
        ")

    # Run the query with DuckDB in memory
    res <- dbGetQuery(
      conn = connection,
      statement = fields_query
    )
    return(res)
  }

#* assessment functions ---------------------
get_prop_asssessment <-
  function(parcel, connection) {
    on.exit(poolReturn(connection))

    # DuckDB can read files from folder
    # assessment_path <- "https://phl-assessments.nyc3.digitaloceanspaces.com/assessments.parquet"
    assessment_path <- "assessments.parquet"

    # SQL statement to perform data aggregation
    # String interpolation is used to inject dynamic string parts into the query
    assessment_query <- str_interp("
      select
        parcel_number,
        year,
        market_value
      from
        read_parquet('${assessment_path}')
      where
        parcel_number = '${parcel}'
        ")

    # Run the query with DuckDB in memory
    res <- dbGetQuery(
      conn = connection,
      statement = assessment_query
    )
    return(res)
  }

get_prop_assessment_plot <-
  function(data, location) {
    data |>
      hchart(
        type = "line",
        name = "Year",
        hcaes(
          x = year,
          y = market_value,
          dollar_value = scales::dollar(market_value),
        )
      ) |>
      hc_plotOptions(
        line = list(
          color = hc_pal[4],
          marker = list(
            fillColor = hc_pal[4],
            lineColor = hc_pal[4]
          )
        )
      ) %>%
      hc_xAxis(title = list(text = "")) |>
      hc_subtitle(text = location) %>%
      hc_yAxis(
        title = list(text = "Assessed Value"),
        labels = list(format = "${value}")
      ) %>%
      hc_tooltip(
        shared = F,
        headerFormat = "",
        pointFormat = "{point.year}</br><b>Assessed Value</b>: {point.dollar_value}"
      )
  }

# get_assessment_compare_plot <-
#   function(data) {
#     data |>
#       # arrange(desc(market_value)) |>
#       hchart(
#         type = "bar",
#         hcaes(
#           x = location, y = round(market_value), dollar_value = scales::dollar(round(market_value)),
#           color = ifelse(type == "Input", hc_pal[1], hc_pal[2])
#         )
#       ) |>
#       hc_xAxis(title = list(text = "")) |>
#       hc_yAxis(
#         title = list(text = "Assessed Value")
#       ) %>%
#       hc_tooltip(
#         shared = F,
#         headerFormat = "",
#         pointFormat = "{point.location}</br><b>Market value</b>: {point.dollar_value}"
#       )
#   }

get_assessment_compare_plotly <-
  function(data) {
    p <-
      data |>
      ggplot(aes(
        x = reorder(location, market_value), y = market_value,
        text = paste0(location, ": ", scales::dollar(market_value)),
        fill = type
      )) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::dollar) +
      scale_fill_manual(values = hc_pal) +
      labs(x = "", y = "Assessed Value") +
      theme_light() +
      theme(
        legend.position = "none",
        text = element_text(size = 15, family = "Courier"),
        panel.background = element_rect(
          fill = "#fbf7f5",
          colour = "#fbf7f5"
        ),
        plot.background = element_rect(fill = "#fbf7f5")
      )

    # t <- list(
    #   family = "Consolas",
    #   size = 14
    # )

    plotly::ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(align = "right")) %>%
      highlight(on = "plotly_click", off = "plotly_doubleclick") %>%
      highlight(on = "plotly_selected", off = "plotly_deselect")
  }


#* property matching functions ---------------------

params <- c(
  "parcel_number",
  "location",
  "category_code",
  "market_value",
  "census_tract",
  "lattitude",
  "longitude",
  "central_air",
  "exterior_condition",
  "interior_condition",
  "number_of_bedrooms",
  "number_stories",
  "quality_grade",
  "total_area",
  "total_livable_area",
  "view_type",
  "year_built"
)

get_index_property <-
  function(data, selected_address) {
    data %>%
      filter(location == selected_address)

    #   on.exit(poolReturn(connection))

    #   # DuckDB can read files from folder
    #   # prop_path <- "https://phl-assessments.nyc3.digitaloceanspaces.com/res_prop.parquet"
    #   prop_path <- arrow::read_parquet("res_prop.parquet")

    #   # SQL statement to perform data aggregation
    #   # String interpolation is used to inject dynamic string parts into the query
    #   prop_query <- str_interp("
    #     select
    #       *
    #     from
    #       read_parquet('${prop_path}')
    #     where
    #       location = '${location}'
    #     -- sometimes a property is associated with > 1 parcel number, so only return one (see 2616 S 18TH ST as example)
    #     qualify
    #       parcel_number = min(parcel_number) over(partition by location order by location)
    #       ")

    #   # Run the query with DuckDB in memory
    #   res <- dbGetQuery(
    #     conn = connection,
    #     statement = prop_query
    #   )
    #   return(res)
  }

select_matching_params <-
  function(data, params) {
    data |>
      select(all_of(params)) |>
      as_tibble()
  }

# get_touching_tracts <-
#   function(index_property, all_tracts) {
#     index_tract <-
#       all_tracts |>
#       filter(NAME %in% c(
#         index_property$census_tract,
#         # allow for mismatched tracts
#         index_property$census_tract + 0.01,
#         index_property$census_tract + 0.02
#       ))

#     intersect_test <-
#       bind_cols(
#         sf::st_touches(x = all_tracts$geometry, y = index_tract$geometry, sparse = FALSE) |>
#           as_tibble() |>
#           rename(adjacent_tracts = 1),
#         all_tracts
#       )

#     touching_tracts <-
#       intersect_test |>
#       filter(adjacent_tracts == TRUE) |>
#       pull(NAME)

#     return(touching_tracts)
#   }

load_touching_tracts <-
  function() {
    arrow::read_parquet("touching_tracts.parquet")
    # sfarrow::st_read_parquet("https://phl-assessments.nyc3.digitaloceanspaces.com/phl_tracts.parquet")
  }

get_matching_tracts <-
  function(index_property, data) {
    data %>%
      filter(input_tract == index_property$census_tract) %>%
      select(adjacent_tract) %>%
      pull(.)
  }

get_match_universe <-
  function(index_property, matching_tracts, connection) {
    on.exit(poolReturn(connection))

    index_property_category_code <-
      index_property %>%
      select(category_code) %>%
      pull(category_code)

    # DuckDB can read files from folder
    # prop_path <- "https://phl-assessments.nyc3.digitaloceanspaces.com/res_prop.parquet"
    prop_path <- "res_prop.parquet"

    # SQL statement to perform data aggregation
    # String interpolation is used to inject dynamic string parts into the query
    prop_query <- str_interp("
      select
        *
      from
        read_parquet('${prop_path}')
      where
        -- sometimes a property is associated with > 1 parcel number, so only return one (see 2616 S 18TH ST as example)
        census_tract in (${matching_tracts})
        -- restrict potential matches to same property type
        and category_code = (${index_property_category_code})
      qualify
        parcel_number = min(parcel_number) over(partition by location order by location)
      ")

    # Run the query with DuckDB in memory
    res <- dbGetQuery(
      conn = connection,
      statement = prop_query
    )

    return(res)
  }

find_matching_properties <-
  function(match_universe, index_property, matching_tracts, n_matches = 25) {
    property_prep <-
      match_universe |>
      select(all_of(names(index_property))) |>
      mutate(
        match_ind = ifelse(parcel_number == index_property$parcel_number, 1, 0)
      ) |>
      select(match_ind, everything()) %>%
      # remove rows with missing values
      na.omit()

    set.seed(354)
    prop_match <-
      matchit(
        match_ind ~ exterior_condition + interior_condition + number_of_bedrooms +
          number_stories + quality_grade + total_area + total_livable_area + view_type + year_built + category_code,
        # lattitude + longitude,
        data = property_prep,
        method = "nearest",
        distance = "randomforest",
        exact = "category_code",
        antiexact = "parcel_number",
        ratio = n_matches,
        replace = FALSE
      )

    tidy_match <-
      match.data(prop_match) %>%
      mutate(
        subclass = as.numeric(as.character(subclass)),
        type = ifelse(match_ind == 0, "Match", "Input")
      ) |>
      left_join(index_property |> select(parcel_number, category_code, location, market_value, lattitude, longitude)) |>
      arrange(
        desc(match_ind),
        distance
      ) |>
      distinct() %>%
      mutate(match_num = dplyr::row_number() - 1)

    return(tidy_match)
  }
