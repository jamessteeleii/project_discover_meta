library(tidyverse)
library(lme4)
library(marginaleffects)

data <- read_csv("all_prior_muscle_DS.csv")

# glimpse(data)
# 
# data <- data |>
#   group_by(study, time, outcome) |>
#   mutate(mean = mean(value, na.rm = TRUE),
#          sd = sd(value, na.rm = TRUE)) |>
#   group_by(study, outcome) |>
#   mutate(baseline_sd = first(sd)) |>
#   ungroup() |>
#   mutate(value_z = (value - mean)/sd)
# 
# 
# 
# model <- lmer(value_z ~ time + (1|study/participant),
#               data = data,
#               REML = TRUE)
# 
# tests <- hypotheses(model, equivalence = c(-0.1,0.1), vcov = "satterthwaite", df = insight::get_df(model)) 
# 
# broom.mixed::tidy(model) |>
#   mutate_at(4:6, round, 4)


model_results <- tibble(term = as.character(),
                        estimate = as.numeric(),
                        std.error = as.numeric(),
                        statistic = as.numeric(),
                        p.value = as.numeric(),
                        s.value = as.numeric(),
                        conf.low = as.numeric(),
                        conf.high = as.numeric(),
                        df = as.integer(),
                        statistic.noninf = as.numeric(),
                        statistic.nonsup = as.numeric(),
                        p.value.noninf = as.numeric(),
                        p.value.nonsup = as.numeric(),
                        p.value.equiv = as.numeric(),
                        muscle = as.character())

data_z <- data[NULL,] |>
  mutate(value_z = as.numeric())

for(i in c(unique(data$study))) {
  
  data_study <- data |>
    filter(study == i)
  
  for(j in c(unique(data_study$outcome))) {
    
    data_outcome <- data_study |>
      filter(outcome == j)
  
  std_lm <- lm(value ~ group,
               data = data_outcome)
  
  std <- summary(std_lm)$sigma
  
  data_outcome <- data_outcome |>
    mutate(
      value_z = (value - mean(value, na.rm=TRUE)) / std
    )
  
  data_z <- bind_rows(data_outcome,
                      data_z)
  
  }
}


model <- lmer(value_z ~ time + (1|study/participant),
              data = data_z,
              REML = TRUE)

tests <- hypotheses(model, equivalence = c(-0.1/12,0.1/12), vcov = "satterthwaite", df = insight::get_df(model))

tidy_model <- broom.mixed::tidy(model) |>
  mutate_at(4:6, round, 4)

estimate <- tests$estimate[2]*12
conf.low <- tests$conf.low[2]*12
conf.high <- tests$conf.high[2]*12


estimate
conf.low
conf.high
 


