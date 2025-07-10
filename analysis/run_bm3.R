library(tidyverse)
library(brms)

data <- read_csv("../data/data.csv")

mdata_item <- data%>%
  mutate(age = scale(age))%>%
  group_by(child, age, ling, mode,testlanguage, item)%>%
  summarise(sum = sum(response),
            n = length(response))

bm3 <- brm(sum|trials(n) ~ age * ling + mode + age*item + (1|child) + (age + item | testlanguage), 
           data = mdata_item,
           chains = 4, 
           cores = 4,
           iter = 6000,
           family = binomial(),
           threads = threading(8), #to speed things up, comment out if not on a cluster
           backend = "cmdstanr",
           control = list(adapt_delta = 0.95, max_treedepth = 20)
)%>%add_criterion("loo")

saveRDS(bm3, "../saves/bm3.rds")