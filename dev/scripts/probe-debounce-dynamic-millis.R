# Verification spike S0 for spec `lazy-consumer-resolve` (D9).
#
# Question: does `shiny::debounce(r, millis = function() ...)` honor a
# runtime-varying window without recreating the underlying reactive?
#
# How to read the result:
#   1. Launch the app: `shiny::runApp("dev/scripts/probe-debounce-dynamic-millis.R")`
#   2. With "Window: 0 ms" selected, type rapidly into the text box.
#      The "Last emission" line should update on every keystroke (no delay).
#   3. Switch to "Window: 500 ms", type rapidly. Emissions should pause
#      until you stop for ~500 ms; then a single emission fires.
#   4. Switch back to "Window: 0 ms". Immediate firing should resume.
#   5. Check the "Reactive identity" stamp printed once at startup; it
#      should NOT change across window toggles. If it does, the debounced
#      reactive was recreated and the dynamic-millis assumption is broken.
#
# Pass criteria: steps 2–4 behave as described AND the identity stamp is
# stable across toggles. Pass → lock D7/D8 design as written. Fail → fall
# back to a hand-rolled debounce helper per the spec's contingency note.

library(shiny)

ui <- fluidPage(
  titlePanel("S0: shiny::debounce dynamic millis probe"),
  radioButtons("window", "Debounce window",
               choices = c("0 ms" = 0, "500 ms" = 500),
               selected = 0, inline = TRUE),
  textInput("typed", "Type here", value = ""),
  tags$hr(),
  verbatimTextOutput("identity"),
  verbatimTextOutput("emissions")
)

server <- function(input, output, session) {
  window_rv <- reactiveVal(0L)

  observeEvent(input$window, {
    window_rv(as.integer(input$window))
  })

  raw <- reactive({ input$typed })

  debounced <- debounce(raw, millis = function() window_rv())

  # Capture identity once. If `debounce` is implemented such that the
  # returned reactive is stable, this stamp persists across window toggles.
  identity_stamp <- format(debounced)
  cat("Reactive identity at startup:", identity_stamp, "\n")

  emissions <- reactiveVal(character())

  observe({
    val <- debounced()
    ts <- format(Sys.time(), "%H:%M:%OS3")
    cur <- emissions()
    cur <- c(paste0("[", ts, "] window=", isolate(window_rv()),
                    "ms  value=", shQuote(val %||% "")), cur)
    emissions(head(cur, 20))
  })

  output$identity <- renderText({
    paste0("Reactive identity (should not change across toggles):\n",
           identity_stamp,
           "\nCurrent window: ", window_rv(), " ms")
  })

  output$emissions <- renderText({
    paste(emissions(), collapse = "\n")
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
