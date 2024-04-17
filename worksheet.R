library(bslib)
library(shiny)
library(crosstalk)
library(plotly)
library(leaflet)

# Creates the "filter link" between the controls and plots
dat <- SharedData$new(dplyr::sample_n(diamonds, 1000))

# Sidebar elements (e.g., filter controls)
filters <- list(
  filter_select("cut", "Cut", dat, ~cut),
  filter_select("color", "Color", dat, ~color),
  filter_select("clarity", "Clarity", dat, ~clarity)
)

# plotly visuals
plots <- list(
  plot_ly(dat) |> add_histogram(x = ~price),
  plot_ly(dat) |> add_histogram(x = ~carat),
  plot_ly(dat) |> add_histogram(x = ~cut, color = ~clarity)
)
plots <- lapply(plots, \(x) config(x, displayModeBar = FALSE))

# map filter and visual
quake_dat <- SharedData$new(quakes)
map_filter <- filter_slider("mag", "Magnitude", quake_dat, ~mag)
map_quakes <- leaflet(quake_dat) |>
  addTiles() |>
  addCircleMarkers()


sidebar_diamonds <- layout_sidebar(
  sidebar = filters[[1]],
  plots[[1]]
)

sidebar_quakes <- layout_sidebar(
  sidebar = map_filter,
  map_quakes
)

page_fillable(
  sidebar_diamonds,
  card(
    card_header("Earthquakes"),
    sidebar_quakes
  )
)



accordion_filters <- accordion(
  accordion_panel(
    "Dropdowns", icon = bsicons::bs_icon("menu-app"),
    !!!filters
  ),
  accordion_panel(
    "Numerical", icon = bsicons::bs_icon("sliders"),
    filter_slider("depth", "Depth", dat, ~depth),
    filter_slider("table", "Table", dat, ~table)
  )
)

card(
  card_header("Groups of diamond filters"),
  layout_sidebar(    
    sidebar = accordion_filters,
    plots[[1]]
  )
)



shinyApp(
  page_navbar(
    title = "Conditional sidebar",
    id = "nav",
    sidebar = sidebar(
      conditionalPanel(
        "input.nav === 'Page 1'",
        "Page 1 sidebar"
      ),
      conditionalPanel(
        "input.nav === 'Page 2'",
        "Page 2 sidebar"
      )
    ),
    nav_panel("Page 1", "Page 1 contents"),
    nav_panel("Page 2", "Page 2 contents")
  ),
  server = function(...) {
    # no server logic required
  }
)

################################################################
library(bslib)
library(shiny)
library(crosstalk)
library(plotly)
library(leaflet)
library(shinyWidgets)

# Creates the "filter link" between the controls and plots
dat <- SharedData$new(dplyr::sample_n(diamonds, 1000))
data <- data.frame(c(2021,2022, 2023, 2024))
names(data) <- "Year"


# Sidebar elements (e.g., filter controls)
filters <- list(
  filter_select("cut", "Cut", dat, ~cut)
#   filter_select("year", "Year", dat, ~Year)
#   filter_select("color", "Color", dat, ~color),
#   filter_select("clarity", "Clarity", dat, ~clarity)
)

# plotly visuals
plots <- list(
  plot_ly(dat) |> add_histogram(x = ~price),
  plot_ly(dat) |> add_histogram(x = ~carat),
  plot_ly(dat) |> add_histogram(x = ~cut, color = ~clarity)
)
plots <- lapply(plots, \(x) config(x, displayModeBar = FALSE))

# map filter and visual
quake_dat <- SharedData$new(quakes)
map_filter <- filter_slider("mag", "Magnitude", quake_dat, ~mag)
map_quakes <- leaflet(quake_dat) |>
  addTiles() |>
  addCircleMarkers()



accordion_filters <- accordion(
  accordion_panel(
    "Overview selection", 
    icon = bsicons::bs_icon("hand-index-thumb"),
    imageOutput("myImage"),
    virtualSelectInput(
      inputId = "selected_years",
      label = "Assessment Year:",
      choices = data$Year,
      selected = 2023,
      multiple = FALSE,
      width = "100%",
      search = TRUE,
      optionsCount = 5
    )
    # !!!filters
  ),
  accordion_panel(
    "Mixed fisheries", 
    icon = bsicons::bs_icon("sliders"),
    filter_slider("depth", "Depth", dat, ~depth)
    # filter_slider("table", "Table", dat, ~table)
  ),
  accordion_panel(
    "Bycatch", 
    icon = bsicons::bs_icon("activity"),
    filter_slider("depth", "Depth", dat, ~depth)
    # filter_slider("table", "Table", dat, ~table)
  ),
  accordion_panel(
    "VMS", 
    icon = bsicons::bs_icon("globe2"),
    filter_slider("depth", "Depth", dat, ~depth)
    # filter_slider("table", "Table", dat, ~table)
  ),
  accordion_panel(
    "Stock status", 
    icon = bsicons::bs_icon("clipboard-data"),
    filter_slider("depth", "Depth", dat, ~depth)
    # filter_slider("table", "Table", dat, ~table)
  )
)



shinyApp(
  page_navbar(
    title = "fisheriesXplorer",
    id = "nav",
    sidebar = sidebar(
        accordion_filters,
        width = "30%"
    #   conditionalPanel(
    #     "input.nav === 'Page 1'",
    #     "Page 1 sidebar"
    #   ),
    #   conditionalPanel(
    #     "input.nav === 'Page 2'",
    #     "Page 2 sidebar"
    #   )
    )
    # nav_panel("Page 1", "Page 1 contents"),
    # nav_panel("Page 2", "Page 2 contents")
  ),
  server = function(input, output) {
      output$myImage <- renderImage(
          {
              list(src = "map.png", width = "100%")
          },
          deleteFile = FALSE
      )
      # no server logic required
  }
)
