suppressMessages(devtools::load_all("/Users/willju/Research/ggpaintr"))

env <- new.env(parent = .GlobalEnv)
env$mtcars <- datasets::mtcars

server_wrapper <- function(input, output, session) {
  shared_reactive <- shiny::reactive(input$n_rows)
  session$userData$ptr_state <- ptr_module_server(
    "plot1",
    formula = paste0(
      "mtcars |> head(num(shared = 'n_rows')) |> ",
      "ggplot(aes(x = wt, y = mpg)) + geom_point()"
    ),
    envir = env,
    shared = list(n_rows = shared_reactive)
  )
}

mod_btn <- paste0("plot1-", ptr_update_data_input_id("ggplot"))
cat("module-scoped click id:", mod_btn, "\n\n")

shiny::testServer(server_wrapper, {
  state <- session$userData$ptr_state

  cat("seed cached rows:",
      nrow(state$resolved_data[["ggplot"]]()), "\n")

  session$setInputs(n_rows = 4L)
  cat("is_stale after shared=4:",
      state$is_stale_env[["ggplot"]](), "\n")

  args <- list()
  args[[mod_btn]] <- 1
  do.call(session$setInputs, args)

  cat("post-click rows:",
      nrow(state$resolved_data[["ggplot"]]()), "\n")
  cat("is_stale after click:",
      state$is_stale_env[["ggplot"]](), "\n")

  session$setInputs(n_rows = 7L)
  cat("is_stale after shared=7:",
      state$is_stale_env[["ggplot"]](), "\n")

  args2 <- list()
  args2[[mod_btn]] <- 2
  do.call(session$setInputs, args2)

  cat("second-click rows:",
      nrow(state$resolved_data[["ggplot"]]()), "\n")
  cat("is_stale after second click:",
      state$is_stale_env[["ggplot"]](), "\n")
})
