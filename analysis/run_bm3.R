library(tidyverse)
library(brms)

data <- read_csv("../data/data.csv")


mdata <- data%>%
  mutate(age = scale(age))

bm3 <- brm(response ~ age * ling + mode + (1|child) + (age | testlanguage) + (age + testlanguage | item), 
             data = mdata,
             chains = 4, 
             cores = 4,
             iter = 6000,
             family = bernoulli(),
             threads = threading(8), #to speed things up, comment out if not on a cluster
             backend = "cmdstanr",
             control = list(adapt_delta = 0.95, max_treedepth = 20)
)%>%add_criterion("loo")

saveRDS(bm3, "../saves/bm3.rds")