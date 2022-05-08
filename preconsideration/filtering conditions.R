library(shiny)
library(DT)
library(shinydashboard)

parse_search_expressions <- function(data, names_data, search) {
  parse_search_expression <- function(x, name_var, s) {
    if (!nzchar(s)) return(FALSE)
    if (is.numeric(x)) {
      r <- strsplit(s, "...", fixed = TRUE)
      r <- sapply(r, as.numeric)
      out = deparse(bquote((x >= .(r[1])) & (x <= .(r[2]))))
      out = gsub('x', name_var, out)
    } else if (is.factor(x) || is.logical(x)) {
      v <- jsonlite::fromJSON(s)
      out = deparse(bquote(x %in% .(v)))
      out = paste0(name_var, substring(out, 2))
    } else {
      out = deparse(bquote(grepl(.(s), x, fixed = TRUE)))
    }
    out
  }

  result = Map(parse_search_expression, data, names_data, search)
  result = result[sapply(result, is.character)]

  if(length(result) > 0){
    paste0(substitute(data), ' %>% filter(',
           paste(unlist(result), collapse = ', '), ')')
  }
  else{substitute(data)}

}

ui <- fluidPage(
  DTOutput("table"),
  box(title = 'Code', width = 12, uiOutput("expression"))
)

server <- function(input, output, session) {

  output$table <- renderDataTable(iris, filter = "top")
  output$expression <- renderText({
    filter_cond = parse_search_expressions(iris, names(iris), input$table_search_columns)
    filter_cond
  })
}

shinyApp(ui, server)


