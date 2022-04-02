library(dplyr)
library(ggplot2)
library(palmerpenguins)

data('penguins')

ttlist <- list("bill_length_mm", "bill_depth_mm", "island")
names(ttlist) <- c("x", "y",  "color")
pp <- ggplot(data = penguins, do.call(aes_string, ttlist))

pp + 
  geom_point() +
  geom_text(aes_string(x = "bill_length_mm", y = "bill_depth_mm", label = "round(bill_depth_mm, 0)")) +
  facet_grid(as.formula("island~."))



ggnull +
  do.call(geom_point, list(mapping = do.call(aes_string, ttlist), data = penguins)) 


ggnull <- ggplot(data = penguins)
part1 <- do.call(geom_point, list(mapping = do.call(aes_string, ttlist))) 
part2 <- do.call(facet_grid, list(as.formula("island~.")))

part.list <- list(part1, part2)

for(i in seq_along(part.list)) {
  ggnull <- ggnull + part.list[[i]]
}

ggnull

ggnull + part1 + part2


ggplot() + 
  geom_point(do.call(aes_string, ttlist))

pp +
  geom_bar()


iris %>% ggplot(aes(x=.data[["Sepal.Length"]], y=.data[["Sepal.Width"]])) +
  geom_point()


ggplot(data = iris, aes_string(x="Sepal.Length", y="Sepal.Width")) +
  geom_point()


penguins %>% ggplot(aes(x = island, y = flipper_length_mm)) +
  geom_bar(stat = "identity")
penguins %>% ggplot(aes(x = island)) +
  geom_bar()


myselect <- "bar"

geoms <- apropos("geom_")

data <- head(mtcars)
data$NAMES <- data$mpg
data$NAMES[c(1,3)] <- ""



dot_func <- function(...) {
  dots <- list(...)
}


function (aligned, striae, resolution, tmpfile = NULL, ...) 
{
  feature <- value <- NULL
  assert_that(!is.null(aligned), !is.null(striae), msg = "aligned and striae must not be NULL")
  features <- apropos("extract_feature_")
  dots <- list(...)
  values <- features %>% purrr::map_dbl(.f = function(f) {
    fun <- getFromNamespace(f, asNamespace("bulletxtrctr"))
    fun_args <- names(formals(fun))
    matching_args <- dots[names(dots) %in% fun_args]
    if ("aligned" %in% fun_args) {
      matching_args$aligned <- aligned$lands
    }
    if ("striae" %in% fun_args) {
      matching_args$striae <- striae$lines
    }
    if ("resolution" %in% fun_args) {
      matching_args$resolution <- resolution
    }
    res <- do.call(fun, matching_args)
    res
  })
  dframe <- data.frame(feature = gsub("extract_feature_", "", 
                                      features), value = values) %>% spread(feature, value)
  if (!is.null(tmpfile)) {
    if (file.exists(tmpfile)) {
      write.table(dframe, file = tmpfile, sep = ",", append = TRUE, 
                  col.names = FALSE, row.names = FALSE)
    }
    else {
      write.table(dframe, file = tmpfile, sep = ",", append = FALSE, 
                  col.names = TRUE, row.names = FALSE)
    }
  }
  dframe
}


#################
data <- read_csv("data/HSLSEEESAID_sampled.csv")
data %>% ggplot() +
  geom_bar(aes(x=S4JobIndustry, fill=S4LOCATION), position = position_fill())










