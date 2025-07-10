library(tidyverse)
library(brms)

data <- read_csv("../data/data.csv")

storydata <- data%>%
  filter(!is.na(story))%>%
  mutate(story = ifelse(
    story == "cat" | story == "dog", "cat/dog", "baby birds/goats"
  ))%>%
  mutate(age = scale(age))%>%
  group_by(child, testlanguage, age, story, item, mode, ling)%>%
  summarise(sum = sum(response),
            n = length(response))

bm5 <- brm(sum|trials(n) ~ age * ling + mode + item * age * story + (1|child) + (age + item + story | testlanguage),
           data = storydata,
           chains = 4,
           cores = 4,
           iter = 6000,
           family = binomial(),
           threads = threading(8),
           backend = "cmdstanr",
           control = list(adapt_delta = 0.95, max_treedepth = 20)
)%>%add_criterion("loo")

saveRDS(bm5, "../saves/bm5.rds")


bm5_1 <- brm(sum|trials(n) ~ age * ling + mode + item * story + (1|child) + (age + item + story | testlanguage), 
           data = storydata,
           chains = 4, 
           cores = 4,
           iter = 6000,
           family = binomial(),
           threads = threading(8), 
           backend = "cmdstanr",
           control = list(adapt_delta = 0.95, max_treedepth = 20)
)%>%add_criterion("loo")

saveRDS(bm5_1, "../saves/bm5_1.rds")