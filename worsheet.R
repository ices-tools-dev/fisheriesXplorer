x <- load(file = "D:/GitHub_2023/fisheriesXplorer/data/catch_trends.rda")
names(catch_trends)
catch_trends$AssessmentYear <- 2022
save(catch_trends, file = "D:/GitHub_2023/fisheriesXplorer/data/catch_trends.rda")




library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(),
  fluidRow(
    column(
      12,
      card(
        card_body(
          fluidRow(
            column(
              6,
              actionButton(
                "button1",
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image1.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            ),
            column(
              6,
              actionButton(
                "button2",
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image2.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            )
          ),
          fluidRow(
            column(
              6,
              actionButton(
                "button3",
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image3.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            ),
            column(
              6,
              actionButton(
                "button4",
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image4.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            )
          ),
          fluidRow(
            column(
              6,
              actionButton(
                "button5",
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image5.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            ),
            column(
              6,
              actionButton(
                "button6",
                label = NULL,
                icon = icon("image", lib = "font-awesome"),
                style = "background: url('path/to/your/image6.jpg') no-repeat center center; background-size: cover; height: 100px; width: 100px;"
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$button1, {
    showNotification("Button 1 clicked!")
  })
  observeEvent(input$button2, {
    showNotification("Button 2 clicked!")
  })
  observeEvent(input$button3, {
    showNotification("Button 3 clicked!")
  })
  observeEvent(input$button4, {
    showNotification("Button 4 clicked!")
  })
  observeEvent(input$button5, {
    showNotification("Button 5 clicked!")
  })
  observeEvent(input$button6, {
    showNotification("Button 6 clicked!")
  })
}

shinyApp(ui, server)

library(shiny)
library(shinyBS)
library(bslib)

ui <- fluidPage(
  card(
    card_header("ShinyBS tooltips"),
    card_body(
      actionButton("btn", "On hover"),
   tipify(actionButton("btn2", "On click"), "Hello again! This is a click-able pop-up", placement="bottom", trigger = "hover")
    )
  )
  #  titlePanel("ShinyBS tooltips"),
  #  actionButton("btn", "On hover"),
  #  tipify(actionButton("btn2", "On click"), "Hello again! This is a click-able pop-up", placement="bottom", trigger = "hover")
  )

server <- function(input, output, session) {
  addTooltip(session=session,id="btn",title="Hello! This is a hover pop-up. You'll have to click to see the next one.")
}

shinyApp(ui, server)



bslib::card(
  bslib::card_header(
    bslib::tooltip(
      span("Card title ", bsicons::bs_icon("question-circle-fill")),
      "Additional info",
      placement = "right"
    )
  ),
  "Card body content..."
)
bslib::tooltip(
  shiny::actionButton("btn", "A button"),
  "A message"
)



library(shiny)
library(bslib)

ui <- page_navbar(
  sidebar = list(
    actionButton("insert_popover", "Insert popover"),
    actionButton("insert_tooltip", "Insert tooltip")
  ),
  id = "navbar",
  nav_panel("Home", "Home page")
)

server <- function(input, output, session) {
  observeEvent(input$insert_popover, {
    nav_insert(
      id = "navbar",
      nav = nav_panel(
        "Popover",
          popover(
            trigger = actionButton(
              "show_popover", 
              "Click here for popover"
            ),
            "Popover message",
            title = "Popover title"
          )
      )
    )
  })
  
  observeEvent(input$insert_tooltip, {
    nav_insert(
      id = "navbar",
      nav = nav_panel(
        "Tooltip",
        tooltip(
          trigger = actionButton(
            "show_tooltip", 
            "Click here for tooltip"
          ),
          "Tooltip message"
        )
      )
    )
  })
}
shinyApp(ui, server)

library(bslib)
card(
  card_header(
    tooltip(
      span("Card title ", bsicons::bs_icon("question-circle-fill")),
      "Additional info",
      placement = "right"
    )
  ),
  "Card body content..."
)
tooltip(
  shiny::actionButton("btn", "A button"),
  "A message"
)


library(shiny)
library(bslib)
library(palmerpenguins)
library(ggplot2)

ui <- page_fillable(
  card(
    card_header(
      "Penguin body mass",
      tooltip(
        bsicons::bs_icon("question-circle"),
        "Mass measured in grams.",
        placement = "right"
      ),
      popover(
        bsicons::bs_icon("gear", class = "ms-auto"),
        selectInput("yvar", "Split by", c("sex", "species", "island")),
        selectInput("color", "Color by", c("species", "island", "sex"), "island"),
        title = "Plot settings",
      ),
      class = "d-flex align-items-center gap-1"
    ),
    plotOutput("plt"),
    card_footer(
      "Source: Gorman KB, Williams TD, Fraser WR (2014).",
      popover(
        a("Learn more", href = "#"),
        markdown(
          "Originally published in: Gorman KB, Williams TD, Fraser WR (2014) Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus Pygoscelis). PLoS ONE 9(3): e90081. [doi:10.1371/journal.pone.0090081](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0090081)"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$plt <- renderPlot({
    ggplot(penguins, aes(x = body_mass_g, y = !!sym(input$yvar), fill = !!sym(input$color))) +
      ggridges::geom_density_ridges(scale = 0.9, alpha = 0.5) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      ggokabeito::scale_fill_okabe_ito() +
      theme_minimal(base_size = 24) +
      theme(legend.position = "top")
  })
}

shinyApp(ui, server)


library(shiny)

ui <- fluidPage(
  actionButton("myButton", "Click me", style = "color: #fff; background-color: #007bff; border-color: #007bff;", title = "This is a tooltip")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)



library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .custom-tooltip {
        position: relative;
        display: inline-block;
      }

      .custom-tooltip .tooltiptext {
        visibility: hidden;
        width: 220px;
        background-color: #555;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        position: absolute;
        z-index: 1;
        bottom: 125%;
        left: 50%;
        margin-left: -60px;
        opacity: 0;
        transition: opacity 0.1s;
      }

      .custom-tooltip:hover .tooltiptext {
        visibility: visible;
        opacity: 1;
      }
    "))
  ),
  
  fluidRow(
    column(3, 
           div(class = "custom-tooltip",
               actionButton("myButton", "Hover over me", class = "btn btn-primary", title = HTML("<b> Custom </br> <i> tooltip</b></i>")),
               div(class = "tooltiptext", "This is a custom tooltip")
           )
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)



library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .custom-tooltip {
        position: relative;
        display: inline-block;
      }

      .custom-tooltip .tooltiptext {
        visibility: hidden;
        width: 120px;
        background-color: #555;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        position: absolute;
        z-index: 1;
        bottom: 125%;
        left: 50%;
        margin-left: -60px;
        opacity: 0;
        transition: opacity 0.3s;
      }

      .custom-tooltip:hover .tooltiptext {
        visibility: visible;
        opacity: 1;
      }
    "))
  ),
  
  fluidRow(
    column(3, 
           div(class = "custom-tooltip",
               actionButton("myButton", "Hover over me", class = "btn btn-primary", title = ""),
               div(class = "tooltiptext", "This is a custom tooltip")
           )
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

# add customizable tooltip to actionButton in Shiny app
library(shiny)
library(shinyjs)


  

# Install necessary packages
install.packages("shiny")
install.packages("shinyBS")

# Load libraries
library(shiny)
library(shinyBS)

# Define UI
ui <- fluidPage(
  # Add an action button
  actionButton("my_button", "Click Me"),
  
  # Add a tooltip to the action button
  bsTooltip("my_button", "This is a customizable hover text", 
            placement = "top", 
            trigger = "hover")
)

# Define server logic (if any)
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)




# Load libraries
library(shiny)

# Define UI
ui <- fluidPage(
  # Add custom CSS for the tooltip
  tags$head(
    tags$style(HTML("
      .tooltip {
        position: relative;
        display: inline-block;
        cursor: pointer;
      }
      .tooltip .tooltiptext {
        visibility: hidden;
        width: 160px;
        background-color: black;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        position: absolute;
        z-index: 1;
        bottom: 125%; 
        left: 50%;
        margin-left: -80px;
        opacity: 0;
        transition: opacity 0.3s;
      }
      .tooltip:hover .tooltiptext {
        visibility: visible;
        opacity: 1;
      }
    "))
  ),
  
  # Add an action button with a tooltip
  div(class = "tooltip",
      actionButton("my_button", "Click Me"),
      div(class = "tooltiptext", "This is a customizable hover text")
  )
)

# Define server logic (if any)
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)



# Load libraries
library(shiny)

# Define UI
ui <- fluidPage(
  # Add an action button with a title attribute for hover text
  tags$button(id = "my_button", type = "button", class = "btn btn-default", title = HTML("<b><i><font size=4> Headline advice </b></i>"), "Click Me")
)

# Define server logic (if any)
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)


# Load libraries
library(shiny)

# Define UI
# Load libraries
library(shiny)

# Define UI
ui <- fluidPage(
  # Add custom CSS for the tooltip
  tags$head(
    tags$style(HTML("
      .tooltip-container {
        position: relative;
        display: inline-block;
      }
      .tooltip-container .tooltip-text {
        visibility: hidden;
        width: 200px;
        background-color: black;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 10px;
        font-size: 16px;
        position: absolute;
        z-index: 1;
        bottom: 125%; /* Adjust as necessary */
        left: 50%;
        margin-left: -100px;
        opacity: 0;
        transition: opacity 0.3s;
      }
      .tooltip-container:hover .tooltip-text {
        visibility: visible;
        opacity: 1;
      }
    "))
  ),
  
  # Add an action button with a custom tooltip
  div(class = "tooltip-container",
      actionButton("my_button", "Click Me"),
      div(class = "tooltip-text", "This is a customizable and larger hover text")
  )
)

# Define server logic (if any)
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)











# Load libraries
library(shiny)

# Define UI
ui <- fluidPage(
  # Include custom CSS for tooltip styling
  tags$head(
    tags$style(HTML("
      .tooltip-container {
        position: relative;
        display: inline-block;
      }
      .tooltip-container .tooltip-text {
        visibility: hidden;
        width: 200px;
        background-color: black;
        color: #fff;
        text-align: center;
        border-radius: 6px;
        padding: 10px;
        font-size: 16px;
        position: absolute;
        z-index: 1;
        bottom: 125%; /* Adjust this value to position the tooltip correctly */
        left: 50%;
        margin-left: -100px;
        opacity: 0;
        transition: opacity 0.3s;
      }
      .tooltip-container:hover .tooltip-text {
        visibility: visible;
        opacity: 1;
      }
      .custom-button {
        background: url('www/research.png') no-repeat center center;
        background-size: cover;
        height: 150px;
        width: 150px;
        border: none;
      }
    "))
  ),
  
  # Create the button with custom tooltip
  div(class = "tooltip-container",
      actionButton(
        inputId = "overview_btn",
        label = NULL,
        style = "background: none; border: none;",
        class = "custom-button"
      ),
      div(class = "tooltip-text", "Overview")
  )
)

# Define server logic (if any)
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)


# Load libraries
# Load libraries
library(shiny)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .image-button {
        position: relative;
        display: inline-block;
        cursor: pointer;
        background-image: url('https://via.placeholder.com/150'); /* Replace with your image URL */
        background-size: cover;
        height: 150px;
        width: 150px;
        border: none;
      }
      .image-button:hover .tooltip {
        visibility: visible;
        opacity: 1;
      }
      .tooltip {
        visibility: hidden;
        width: 160px;
        background-color: red;
        color: white;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        font-size: 14px;
        position: absolute;
        z-index: 1;
        top: 100%; /* Adjust as necessary */
        left: 50%;
        transform: translateX(-50%);
        opacity: 0;
        transition: opacity 0.3s;
      }
    "))
  ),
  
  # Create the image button with hover text
  div(class = "image-button",
      div(class = "tooltip", "Hover Text")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Server logic (if any)
}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .image-button {
        position: relative;
        display: inline-block;
        cursor: pointer;
        background-image: url('www/research.png'); /* Replace with your image URL */
        background-size: cover;
        height: 150px;
        width: 150px;
        border: none;
      }
      .image-button:hover .tooltip {
        visibility: visible;
        opacity: 1;
      }
      .tooltip {
        visibility: hidden;
        width: 160px;
        background-color: black;
        color: white;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        font-size: 14px;
        position: absolute;
        z-index: 1;
        top: 100%; /* Adjust as necessary */
        left: 50%;
        transform: translateX(-50%);
        opacity: 0;
        transition: opacity 0.3s;
      }
    "))
  ),
  
  # Create the image button with hover text
  div(class = "image-button", id = "my_button",
      div(class = "tooltip", "Hover Text")
  )
)

# Define server logic
server <- function(input, output, session) {
  observe(input$my_button, {
    print("Image button clicked!")
  })
}

# Run the application
shinyApp(ui = ui, server = server)



library(shiny)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .image-button {
        position: relative;
        display: inline-block;
        cursor: pointer;
        background-image: url('https://via.placeholder.com/150'); /* Replace with your image URL */
        background-size: cover;
        height: 150px;
        width: 150px;
        border: none;
      }
      .image-button:hover .tooltip {
        visibility: visible;
        opacity: 1;
      }
      .tooltip {
        visibility: hidden;
        width: 160px;
        background-color: black;
        color: white;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        font-size: 14px;
        position: absolute;
        z-index: 1;
        top: 100%; /* Adjust as necessary */
        left: 50%;
        transform: translateX(-50%);
        opacity: 0;
        transition: opacity 0.3s;
      }
    "))
  ),
  
  # Create the image button with hover text
  div(class = "image-button", id = "my_button",
      div(class = "tooltip", "Hover Text")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Bind click event using shinyjs
  shinyjs::onclick("my_button", expr = {
    print("Image button clicked!")
  })
}

# Run the application
shinyApp(ui = ui, server = server)




library(shiny)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .image-button {
        position: relative;
        display: inline-block;
        cursor: pointer;
        background-size: cover;
        height: 150px;
        width: 150px;
        border: none;
        margin: 10px;
      }
      .image-button:hover .tooltip {
        visibility: visible;
        opacity: 1;
      }
      .tooltip {
        visibility: hidden;
        width: 160px;
        background-color: black;
        color: white;
        text-align: center;
        border-radius: 6px;
        padding: 5px;
        font-size: 14px;
        position: absolute;
        z-index: 1;
        top: 100%; /* Adjust as necessary */
        left: 50%;
        transform: translateX(-50%);
        opacity: 0;
        transition: opacity 0.3s;
      }
    "))
  ),
  
  # Create multiple image buttons with hover text
  div(
    class = "image-button", id = "button1",
    style = "background-image: url('https://via.placeholder.com/150/FF5733/FFFFFF');",
    div(class = "tooltip", "Button 1")
  ),
  div(
    class = "image-button", id = "button2",
    style = "background-image: url('https://via.placeholder.com/150/33FF57/FFFFFF');",
    div(class = "tooltip", "Button 2")
  ),
  div(
    class = "image-button", id = "button3",
    style = "background-image: url('https://via.placeholder.com/150/5733FF/FFFFFF');",
    div(class = "tooltip", "Button 3")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Bind click event for each button using shinyjs
  shinyjs::onclick("button1", expr = {
    print("Button 1 clicked!")
    # Add your server-side logic here for Button 1
  })
  shinyjs::onclick("button2", expr = {
    print("Button 2 clicked!")
    # Add your server-side logic here for Button 2
  })
  shinyjs::onclick("button3", expr = {
    print("Button 3 clicked!")
    # Add your server-side logic here for Button 3
  })
}

# Run the application
shinyApp(ui = ui, server = server)


library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)

# Load data
shape_eco <- st_read(dsn = "D:/GitHub_2023/fisheriesXplorer/2022_NrS_FO_VMS_effort", 
        layer = "2022_NrS_FO_VMS_effort")


# Create a leaflet map
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = shape_eco, 
              fillColor = "blue", 
              fillOpacity = 0.5, 
              color = "black", 
              weight = 1) %>%
  addDrawToolbar(
    targetGroup = "draw",
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
  )



data <- MASS::Cars93[20:49, c("Manufacturer", "Model", "MPG.city", "MPG.highway")]
library(reactable)
library(reactablefmtr)
## Stack text from one column with another column:
reactable(
data,
columns = list(
Manufacturer = colDef(name = "Manufacturer/Model",
                      cell = merge_column(data, merged_name = "Model"
                      )),
Model = colDef(show = FALSE)))

## Control the appearance of both the current and merged columns:
reactable(
data,
columns = list(
Manufacturer = colDef(name = "Manufacturer/Model",
                      cell = merge_column(data,
                                          merged_name = "Model",
                                          merged_size = 16,
                                          merged_color = "blue",
                                          merged_style = "italic",
                                          size = 18,
                                          color = "red"
                                          )),
Model = colDef(show = FALSE)))

## Combine both numeric and non-numeric columns together:
reactable(
data,
columns = list(
Model = colDef(name = "Model/MPG Highway",
                  cell = merge_column(data,
                                      merged_name = "MPG.highway",
                                      merged_position = "below",
                                      merged_size = 20,
                                      merged_color = "green"
)),
MPG.highway = colDef(show = FALSE),
MPG.city = colDef(show = FALSE)))

devtools::document()
