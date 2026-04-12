library(shiny)
library(shinyWidgets)

world_map <- map_data("world")

# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy",
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# Please load your data first

ui <- fluidPage(

  # Application title
  titlePanel("ggpaintr demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "ggplot", pickerInput("ggplot+3+2+2", paste0(
            "x",
            ": "
          ), choices = c(
            "long", "lat", "group", "order", "region",
            "subregion"
          ), selected = "", multiple = TRUE, options = pickerOptions(maxOptions = 1)),
          pickerInput("ggplot+3+3", paste0("y", ": "),
            choices = c(
              "long",
              "lat", "group", "order", "region", "subregion"
            ), selected = "",
            multiple = TRUE, options = pickerOptions(maxOptions = 1)
          )
        ),
        tabPanel("geom_polygon1", pickerInput("geom_polygon1+2+2",
          paste0("group", ": "),
          choices = c(
            "long", "lat", "group",
            "order", "region", "subregion"
          ), selected = "", multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
        ), pickerInput("geom_polygon1+2+3",
          paste0("fill", ": "),
          choices = c(
            "long", "lat", "group",
            "order", "region", "subregion"
          ), selected = "", multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
        )), tabPanel(
          "geom_polygon2",
          pickerInput("geom_polygon2+2+2", paste0("group", ": "),
            choices = c(
              "long", "lat", "group", "order", "region",
              "subregion"
            ), selected = "", multiple = TRUE, options = pickerOptions(maxOptions = 1)
          ),
          pickerInput("geom_polygon2+2+3", paste0("fill", ": "),
            choices = c(
              "long", "lat", "group", "order", "region",
              "subregion"
            ), selected = "", multiple = TRUE, options = pickerOptions(maxOptions = 1)
          )
        ),
        tabPanel(
          "geom_text", pickerInput("geom_text+3+2", paste0(
            "x",
            ": "
          ),
          choices = c("region", "long", "lat"), selected = "",
          multiple = TRUE, options = pickerOptions(maxOptions = 1)
          ),
          pickerInput("geom_text+3+3", paste0("y", ": "), choices = c(
            "region",
            "long", "lat"
          ), selected = "", multiple = TRUE, options = pickerOptions(maxOptions = 1)),
          pickerInput("geom_text+3+4", paste0("label", ": "), choices = c(
            "region",
            "long", "lat"
          ), selected = "", multiple = TRUE, options = pickerOptions(maxOptions = 1)),
          numericInput("geom_text+4", paste0(
            "num for ", "size",
            ":"
          ), NA), numericInput("geom_text+5", paste0(
            "num for ",
            "hjust", ":"
          ), NA)
        ), tabPanel("scale_fill_viridis_d"),
        tabPanel("theme_void"), tabPanel("theme", textInput(
          "theme+2",
          paste0("text for ", "legend.position", ":")
        ))
      ),
      actionButton("draw", "click to draw the plot"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("outputPlot")
    )
  )
)

server <- function(input, output) {
  observe({
    p_expr <- parse_expr('expr(ggplot(data = some.eu.maps, aes(x = .data[[input[["ggplot+3+2+2"]]]] +
    1, y = .data[[input[["ggplot+3+3"]]]])) +
  geom_polygon(aes(group = .data[[input[["geom_polygon1+2+2"]]]],
    fill = .data[[input[["geom_polygon1+2+3"]]]])) +
  geom_polygon(aes(group = .data[[input[["geom_polygon2+2+2"]]]],
    fill = .data[[input[["geom_polygon2+2+3"]]]])) +
  geom_text(data = region.lab.data, aes(x = .data[[input[["geom_text+3+2"]]]],
    y = .data[[input[["geom_text+3+3"]]]], label = .data[[input[["geom_text+3+4"]]]]),
    size = input[["geom_text+4"]], hjust = input[["geom_text+5"]]) +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = input[["theme+2"]]))')

    p_expr_active <- expr_remove_empty_input(p_expr, input)

    output$outputPlot <- renderPlot({
      ptr_assemble_plot(expr_plot_eval(p_expr_active))
    })
  }) %>% bindEvent(input$draw)
}

shinyApp(ui, server)
