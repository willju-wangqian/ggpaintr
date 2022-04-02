library(tidyverse)

data <- read_csv("data/HSLSEEESAID.csv")

str(data)

data <- data %>% mutate(
  S4OCC30EARN = as.numeric(S4OCC30EARN),
  X4X2SES = as.numeric(X4X2SES),
  Weight = as.numeric(Weight)
) %>% filter(S4OCC30EARN >= 50)

set.seed(20220302)
idx <- sample(1:nrow(data), size = 1000, replace = FALSE)
data <- data[idx, ]

write_csv(data, "data/HSLSEEESAID_sampled.csv")

summary(data)

data$Weight %>% is.character()

data %>% ggplot() +
  geom_histogram(aes(x = S4OCC30EARN), binwidth = 1)

summary(data$S4OCC30EARN)

data %>% filter(S4OCC30EARN > 10000) %>% summarise(
  tt = min(S4OCC30EARN),
  n = n()
)


apply(data, 2, function(x) {
  if(is.character(x)) {
    unique(x)
  }
})

dataLolli <- data %>% group_by(S4JOBTEAMWRK, S4ParEd) %>% 
  summarise(
    y = mean(S4OCC30EARN) / 1000
  )

dataLolli %>% ggplot(aes(x=S4JOBTEAMWRK, y=y, group=S4ParEd, fill=S4ParEd, label = round(y, 2))) +
  geom_col(color= "gray20", size = 0.1, 
           alpha = 0.85, width = 0.7, position = position_dodge(width = .7)) + 
  geom_text(position = position_dodge(width = .75), color = "gray20", size = rel(2.5), hjust = 0) + 
  coord_flip()
  


S4JOBTEAMWRK
S4ParEd


