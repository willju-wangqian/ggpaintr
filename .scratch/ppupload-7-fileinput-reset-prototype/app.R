# =============================================================================
# PROTOTYPE — THROWAWAY. ggpaintr ppUpload item #7 (fileInput visual reset).
# Delete me once the question below is answered & folded into ptr_setup_source_uis.
#
# QUESTION (the load-bearing empirical unknown, handoff §5 + §6 CDP caveat):
#   When a fileInput lives inside a uiOutput and the server re-renders that
#   uiOutput with the SAME inputId, does the displayed filename pill actually
#   CLEAR in a real browser? And does a STATIC sibling textInput survive the
#   re-render untouched? (chromote/CDP cannot prove this — eyeball it manually.)
#
# MIRRORS the chosen design (handoff approach a):
#   UI  = tagList(uiOutput("file_slot"), textInput("shortcut"))   <- textInput STATIC
#   svr = output$file_slot <- renderUI({ <bump>; fileInput("upload", ...) })
#   trigger = RISING EDGE only (shortcut empty -> nonempty bumps; clearing does NOT)
#
# NO ggpaintr machinery. No persistence. In-memory only.
# Run:  R -e 'shiny::runApp(".scratch/ppupload-7-fileinput-reset-prototype", launch.browser=TRUE)'
# =============================================================================

library(shiny)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: system-ui, sans-serif; max-width: 760px; margin: 24px auto; }
    .slot { border: 1px solid #cdd; padding: 12px; border-radius: 6px; margin-bottom: 8px; }
    .log  { background:#0b1021; color:#9fe; padding:10px; border-radius:6px;
            font-family: ui-monospace, monospace; white-space:pre-wrap; font-size:12px; }
    h3 { margin: 18px 0 6px; }
    .chk { background:#fffbe6; border:1px solid #e6d98a; padding:12px; border-radius:6px; }
    .chk li { margin: 6px 0; }
  "))),
  titlePanel("PROTOTYPE — ppUpload #7 fileInput visual reset"),

  div(class = "chk",
    tags$b("Eyeball checklist (do these in a real browser, watch the file pill):"),
    tags$ol(
      tags$li("Pick a file (e.g. /tmp/mt.csv) -> the file widget shows its name. STATE shows upload$name."),
      tags$li("Type any non-empty text in 'shortcut' -> on the FIRST char the file widget should CLEAR its displayed filename (rising-edge re-render). Keep typing -> textbox is NOT disrupted (static)."),
      tags$li("Now pick a file again -> the textbox auto-clears (file->text) BUT the file widget must KEEP showing the just-picked name (asymmetric: clearing the textbox must NOT re-render the file widget).")
    )
  ),

  h3("Source widget (fileInput inside a re-renderable uiOutput)"),
  div(class = "slot", uiOutput("file_slot")),

  h3("Shortcut textbox (STATIC — never inside a renderUI)"),
  div(class = "slot",
    textInput("shortcut", label = "Optional dataset name", value = "",
              placeholder = "type a name -> should clear the file widget once")
  ),

  h3("Live state"),
  div(class = "log", verbatimTextOutput("state", placeholder = TRUE)),
  h3("Action log (newest first)"),
  div(class = "log", verbatimTextOutput("logout", placeholder = TRUE))
)

server <- function(input, output, session) {
  bump        <- reactiveVal(0)    # incrementing this re-renders the fileInput uiOutput
  prev_short  <- reactiveVal("")   # for rising-edge detection on the shortcut
  loglines    <- reactiveVal(character(0))
  say <- function(...) loglines(c(paste0(format(Sys.time(), "%H:%M:%OS2"), "  ", ...),
                                   loglines()))

  # --- the source widget: fileInput, SAME id "upload", re-rendered on bump -----
  output$file_slot <- renderUI({
    bump()                                   # dependency: bumping re-renders
    isolate(say("renderUI(file_slot) fired  [bump=", bump(), "]"))
    fileInput("upload", "Choose a data file", accept = ".csv")
  })

  # --- rising edge: shortcut empty -> nonempty  => bump (clear file widget) -----
  #     clearing the shortcut (nonempty -> empty) must NOT bump (asymmetric).
  observeEvent(input$shortcut, {
    cur <- input$shortcut %||% ""
    was <- prev_short()
    if (nzchar(cur) && !nzchar(was)) {
      say("shortcut RISING edge ('", was, "' -> '", cur, "')  => bump")
      bump(bump() + 1)
    } else if (!nzchar(cur) && nzchar(was)) {
      say("shortcut cleared ('", was, "' -> '')  => NO bump (asymmetric)")
    }
    prev_short(cur)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # --- file picked -> clear the sibling textbox (the kept updateTextInput) ------
  observeEvent(input$upload, {
    say("file picked: '", input$upload$name, "'  => updateTextInput(shortcut, '')")
    updateTextInput(session, "shortcut", value = "")
  }, ignoreInit = TRUE)

  output$state <- renderText({
    paste0(
      "input$upload$name : ", (input$upload$name %||% "<none>"), "\n",
      "input$shortcut    : '", (input$shortcut %||% ""), "'\n",
      "prev_short        : '", prev_short(), "'\n",
      "bump (re-renders) : ", bump()
    )
  })
  output$logout <- renderText(paste(loglines(), collapse = "\n"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
