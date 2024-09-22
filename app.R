source("helpers.R")

thematic::thematic_shiny()

link_gh <- tags$a(
  shiny::icon("github"), "View on GitHub",
  href = "https://github.com/futurestateanalytics/phl-assessments",
  target = "_blank",
  align = "center"
)

ui <- page_sidebar(
  title = "Philly Property Assessment Explorer",
  theme = my_theme,
  sidebar = sidebar(
    open = list(desktop = "open", mobile = "always-above"),
    shiny::h5("Start Here"),
    textInput(
      inputId = "address_input",
      label = shiny::strong("Step 1 - Enter Address"),
      value = "",
      placeholder = "ex: 123 Market St"
    ),
    shiny::actionButton(
      "goButton",
      "Lookup Address"
    ),
    selectInput(
      inputId = "address_select",
      label = shiny::strong("Step 2 - Confirm Address"),
      choices = " "
    ),
    shiny::p(shiny::strong("Step 3 - Find Matches")),
    bslib::input_task_button(
      "get_matches",
      "Find Matches"
    ),
    shiny::hr(),
    # shiny::conditionalPanel(
    #   condition = "input.get_matches > 0",
    htmltools::browsable(
      tagList(
        tags$button(
          tagList(fontawesome::fa("download"), "Download Table (csv)"),
          onclick = "Reactable.downloadDataCSV('matches-table', 'property_comps.csv')"
        ),
      )
      # )
    ),
    shiny::hr(),
    shiny::h5("Learn More"),
    actionButton(
      "about_button",
      "About"
    ),
    actionButton(
      "methods_and_data",
      "Methods & Data"
    ),
    actionButton(
      "disclaimer",
      "Disclaimer"
    ),
    HTML("<span align = 'center'> A side project by</br><a href='https://www.futurestateanalytics.io'>Future State Analytics</a></span>"),
    link_gh,
  ),
  layout_columns(
    col_widths = c(4, 4, 4, 12, 3),
    row_heights = c(2, 1.5, 0.5),
    card(
      card_header(
        "Matched Properties",
        tooltip(
          bs_icon("info-circle"),
          "Map showing the input property (red circle) and matched properties located in adjacent census tracts (blue circles)"
        )
      ),
      full_screen = TRUE,
      fill = TRUE,
      leafletOutput("map")
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Assessment Comparison",
        tooltip(
          bs_icon("info-circle"),
          "Compares the property assessment of the input propery (red) relative to the matches (blue)"
        )
      ),
      plotly::plotlyOutput("assessment_compare_plot")
    ),
    card(
      full_screen = TRUE,
      fill = TRUE,
      card_header(
        "Assessment Trend",
        tooltip(
          bs_icon("info-circle"),
          "Property assessment over time"
        )
      ),
      highchartOutput("assessment_plot")
    ),
    card(
      full_screen = TRUE,
      fill = TRUE,
      card_header(
        "Matches Detail",
        tooltip(
          bs_icon("info-circle"),
          HTML(
            "Detailed information on the matched properties identifed by the algorithm.
            Note the Input Property will be in the first row.
            A full list if variables and column definitions can be found at the <a href='https://metadata.phila.gov/#home/datasetdetails/5543865f20583086178c4ee5/representationdetails/55d624fdad35c7e854cb21a4/'>OPA properties metadata catalogue.</a>
            "
          )
        )
      ),
      reactableOutput("matches2")
    )
  ),
)

server <- function(input, output) {
  observeEvent(input$goButton, {
    address_result <- get_loc_names(input$address_input)
    updateSelectizeInput(getDefaultReactiveDomain(), "address_select",
      label = "Step 2 - Confirm Address",
      choices = address_result %>% pull(location),
      selected = head(address_result, 1)
    )
  })

  index_property <-
    eventReactive(input$get_matches, {
      get_index_property(input$address_select) |>
        select_matching_params(params)
    })

  matching_tracts <-
    reactive(
      get_touching_tracts(index_property(), get_phl_tracts())
    )

  match_universe <-
    reactive(
      get_match_universe(
        index_property = index_property(),
        matching_tracts =
          paste0(
            c(index_property()$census_tract, matching_tracts()),
            collapse = ","
          )
      )
    )

  matches <-
    reactive(
      find_matching_properties(
        match_universe = match_universe(),
        index_property = index_property(),
        matching_tracts = matching_tracts()
      )
    )

  matches_shared <-
    reactive(
      crosstalk::SharedData$new(
        matches() %>%
          select(-c(match_ind, weights, subclass)) %>%
          select(
            type, location, market_value, central_air:year_built,
            similarity = distance, match_num, parcel_number, lattitude, longitude
          ),
        ~location
      )
    )

  output$matches2 <-
    renderReactable({
      matches_shared() %>%
        reactable(.,
          searchable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          defaultPageSize = 30,
          elementId = "matches-table",
          selection = "multiple",
          onClick = "select",
          rowStyle = list(cursor = "pointer"),
          style = list(fontSize = "0.875rem"),
          columns = list(
            parcel_number = colDef(show = FALSE),
            lattitude = colDef(show = FALSE),
            longitude = colDef(show = FALSE),
            match_num = colDef(show = FALSE),
            type = colDef(name = "Type"),
            location = colDef(name = "Address"),
            market_value = colDef(name = "Assessment", format = colFormat(currency = "USD", separators = TRUE, digits = 0)),
            central_air = colDef(name = "Central Air"),
            exterior_condition = colDef(name = "Ext. Cond."),
            interior_condition = colDef(name = "Int. Cond."),
            number_of_bedrooms = colDef(name = "Bedrooms"),
            number_stories = colDef(name = "Stories"),
            quality_grade = colDef(name = "Quality"),
            total_area = colDef(name = "Tot. Area"),
            total_livable_area = colDef(name = "Livable Area"),
            view_type = colDef(name = "View"),
            year_built = colDef(name = "Year")
          )
        )
    })

  output$map <-
    renderLeaflet({
      leaflet(matches_shared()) |>
        addTiles() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~lattitude,
          # opacity = .8,
          stroke = FALSE,
          fillOpacity = 0.75,
          radius = ~ ifelse(type == "Match", 8, 12),
          color = ~ ifelse(type != "Match", "tomato", "dodgerblue"),
          popup = ~ paste(
            location, "</br>",
            scales::dollar(market_value), "</br>",
            glue::glue('<a href="https://www.google.com/maps/place/{location},+Philadelphia,+PA">Google Maps</a>')
          )
        )
    })

  output$assessment_plot <-
    renderHighchart({
      get_prop_asssessment(index_property()$parcel_number) %>%
        get_prop_assessment_plot(., location = input$address_select)
    })

  output$assessment_compare_plot <-
    plotly::renderPlotly({
      get_assessment_compare_plotly(matches_shared())
    })

  # About & Help modals
  observeEvent(input$about_button, {
    showModal(
      modalDialog(
        tags$h3("About"),
        tags$p("The purpose of this app is to make the residential property assessments appeals process more efficient."),
        tags$p("The app can save you time by finding up to 25 comps automatically using a matching algorithm, and returns some relevant information."),
        tags$p("You can review the characteristics of the matched properties and how their assessments compare to yours.
          If you choose to appeal, the matched properties, if appropriate, can be used as comparisons."),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  observeEvent(input$methods_and_data, {
    showModal(
      modalDialog(
        HTML(
          "<h3>Methods</h3>
          <p>
            Once a user enters a property into the search field and clicks 'Find Matches', the underlying data is filtered to only include properties in the surrounding census tracts. This reduced list of properties forms a more relevant 'universe' of potential matches.
          </p>
          <p>
            Then, to find comprable properties within this reduced list, a matching algorithm is implemented using <a href='https://github.com/kosukeimai/MatchIt'>MatchIt</a>. A 'nearest neighbors' approach finds comprable properties by simultaneously analyzing multiple variables in the OPA dataset:
              <ul>
                <li>Exterior Condition</li>
                <li>Interior Condition</li>
                <li>Bedrooms</li>
                <li>Stories Built</li>
                <li>Quality Grade Built</li>
                <li>Total Area</li>
                <li>Total Livable Area</li>
                <li>Year Built</li>
              </ul>
            A full list if variables and column definitions can be found at the <a href='https://metadata.phila.gov/#home/datasetdetails/5543865f20583086178c4ee5/representationdetails/55d624fdad35c7e854cb21a4/'>OPA properties metadata catalogue.</a>
          </p>
          <h3>Data</h3>
          <p>Property assessment and geospatial data is publicly available and is sourced from the
          <a href='https://opendataphilly.org/datasets/philadelphia-properties-and-assessment-history/#:~:text=Philadelphia%20Properties%20and%20Assessment%20History.%20Some%20of,to%20contact%20OPA%20to%20report%20the%20issue.'>Philadelphia Properties and Assessment History</a>
            which is maintained by OpenDataPhilly. Additional census tract data is obtained from
          <a href='https://github.com/walkerke/tigris'>Tigris</a>.</p>
        "
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  observeEvent(input$disclaimer, {
    showModal(
      modalDialog(
        HTML(
          "<h3>Disclaimer</h3>
            This app is not affiliated in any way with the Phila Office of Property Assessmment or OpenDataPhilly.
            There is no guarantee any of the resulting 'matched' properties were used in determining the input property's market value."
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
}

shinyApp(ui, server)
