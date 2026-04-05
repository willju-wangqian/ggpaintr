library(ggplot2)
library(dplyr)
source(here("paintr2/paintr2_func.R"))
source(here("paintr2/ui_function.R"))

ggpaintr_basic2 <- function(input_formula) {

  ui <- fluidPage(

    # Application title
    titlePanel("ggpaintr demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        # textAreaInput("formula", label = "Enter the paintr formula",
        #               value = input_formula, rows = 5, placeholder = "Input the formula"),
        # actionButton("enter", "click to enter the formula"),
        uiOutput("controlPanel"),
        actionButton("draw", "click to draw the plot"),
        downloadButton("shinyExport", "export the shiny app"),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("outputPlot"),
        verbatimTextOutput('outputCode')
      )
    )
  )

  server <- function(input, output, session) {

    session$userData$paintr <- reactiveValues(obj = list(NULL))
    session$userData$paintr$obj <- paintr_formula(input_formula)

    observe({
      req(session$userData$paintr$obj)

      session$userData$paintr$var_ui_list <-
        output_embed_var(input, output, session$userData$paintr$obj)
    })

    output$controlPanel <- renderUI({
      req(session$userData$paintr)

      column(12, paintr_get_tab_ui(session$userData$paintr$obj))

    })

    output$shinyExport <- downloadHandler(
      filename = "trial_0.R",
      content = function(file) {

        req(session$userData$paintr$var_ui_list)

        generate_shiny(session$userData$paintr$obj,
                       session$userData$paintr$var_ui_list,
                       file,
                       style = TRUE)

      }
    )

    observe({
      req(session$userData$paintr$obj)

      complete_expr_code <-
        paintr_complete_expr(session$userData$paintr$obj, input)

      output$outputPlot <- renderPlot({

        paintr_get_plot(
          complete_expr_code[['complete_expr_list']],
          envir = complete_expr_code[['eval_env']]
        )

      })

      output$outputCode <- renderText({

        complete_expr_code[['code_text']]

      })

    }) %>% bindEvent(input$draw)
  }

  shinyApp(ui, server)
}

# built-in: ggplot2::diamonds
qdat <- diamonds %>%
  group_by(cut) %>%
  summarize(
    q50 = quantile(price, 0.50),
    q90 = quantile(price, 0.90),
    .groups = "drop"
  )

ggplot(diamonds, aes(carat, price)) +
  # geom_hex(bins = 45) +                                   # layer 1
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +  # layer 2
  facet_wrap(~cut, ncol = 3) +                             # layer 3
  geom_hline(data = qdat, aes(yintercept = q50), linetype = 2) + # layer 4
  geom_hline(data = qdat, aes(yintercept = q90), linetype = 3) + # layer 5
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Diamonds: price vs carat",
    subtitle = "Hex density + LOESS + median/90th percentile lines (by cut)",
    x = "Carat", y = "Price"
  ) +
  theme_minimal(base_size = 12)


ggpaintr_basic2(
  '
  ggplot(data = diamonds, aes(x = var, y = price)) +
    # geom_hex(bins = 45) +                                   # layer 1
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +  # layer 2
    facet_wrap(~var, ncol = num) +                             # layer 3
    geom_hline(data = qdat, aes(yintercept = q50), linetype = 2) + # layer 4
    geom_hline(data = qdat, aes(yintercept = q90), linetype = 3) + # layer 5
    scale_y_continuous(labels = scales::dollar) +
    labs(
      title = text,
      subtitle = text,
      x = text, y = text
    ) +
    theme_minimal(base_size = 12)
  '
)

library(ggplot2)
library(dplyr)
library(nycflights13)

dat <- flights %>%
  filter(!is.na(arr_delay), !is.na(carrier)) %>%
  mutate(arr_delay = pmax(pmin(arr_delay, 180), -60))  # winsorize for readability

top_carriers <- dat %>% count(carrier, sort = TRUE) %>% slice_head(n = 8) %>% pull(carrier)

dat2 <- dat %>% filter(carrier %in% top_carriers)

ggplot(dat2, aes(x = reorder(carrier, arr_delay, median), y = arr_delay)) +
  geom_violin(trim = TRUE, alpha = 0.35) +         # layer 1
  geom_boxplot(width = 0.15, outlier.shape = NA) + # layer 2
  geom_jitter(width = 0.12, alpha = 0.05, size = 0.6) + # layer 3
  stat_summary(fun = mean, geom = "point", size = 2.2) + # layer 4
  geom_hline(yintercept = 0, linetype = 2) +        # layer 5
  labs(
    title = "Arrival delays by carrier (NYC flights)",
    x = "Carrier", y = "Arrival delay (min, clipped to [-60, 180])"
  ) +
  theme_minimal(base_size = 12)

ggpaintr_basic2(
  '
  ggplot(data = dat2, aes(x = reorder(var, X = var, FUN = expr), y = var)) +
  geom_violin(trim = TRUE, alpha = num) +         # layer 1
  geom_boxplot(width = num, outlier.shape = NA) + # layer 2
  geom_jitter(width = 0.12, alpha = num, size = 0.6) + # layer 3
  stat_summary(fun = expr, geom = "point", size = 2.2) + # layer 4
  geom_hline(yintercept = num, linetype = 2) +        # layer 5
  labs(
    title = text,
    x = text
  ) +
  theme_minimal(base_size = num)
  '
)

