suppressMessages(devtools::load_all("/Users/willju/Research/ggpaintr"))

env <- new.env(parent = .GlobalEnv)
env$mtcars <- datasets::mtcars

server_wrapper <- function(input, output, session) {
  session$userData$ptr_state <- ptr_server(
    input, output, session,
    "mtcars |> select(var) |> ggplot(aes(x = var)) + geom_histogram()",
    envir = env
  )
}

shiny::testServer(server_wrapper, {
  state <- session$userData$ptr_state
  obj <- shiny::isolate(state$obj())

  pipeline_var_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
  aes_var_id <- "ggplot_3_2"
  btn_id <- ptr_update_data_input_id("ggplot")

  cat("pipeline_var_id =", pipeline_var_id, "\n")
  cat("aes_var_id      =", aes_var_id, "\n")
  cat("button_id       =", btn_id, "\n\n")

  cat("=== seed cache ===\n")
  seed <- state$resolved_data[["ggplot"]]()
  cat("rows:", nrow(seed), "  cols:", ncol(seed),
      "  names:", paste(head(names(seed), 5), collapse = ","), "\n\n")

  cat("=== set pipeline_var = 'mpg', then click Update Data ===\n")
  args <- list()
  args[[pipeline_var_id]] <- "mpg"
  args[[btn_id]] <- 1
  do.call(session$setInputs, args)

  upd <- state$resolved_data[["ggplot"]]()
  cat("rows:", nrow(upd), "  cols:", ncol(upd),
      "  names:", paste(names(upd), collapse = ","), "\n\n")

  cat("=== inspect runtime expression after pipeline update ===\n")
  args2 <- list()
  args2[[aes_var_id]] <- "mpg"
  args2[["drawButton"]] <- 1
  do.call(session$setInputs, args2)
  rt <- state$runtime()
  cat("ok:", rt$ok, "\n")
  cat("code:\n")
  cat(ptr_extract_code(rt), "\n")
})
