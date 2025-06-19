library(tidyverse)
library(lme4)

d <- read_csv('regression_data.csv') %>%
  filter(str_detect(problem_name, '^10'))

m <- glmer(post_test_score ~ (1 | student_id) + used_ai_on_corresponding_item, 
           d, family='binomial')
  
sjPlot::tab_model(m)

xtabs(~post_test_score+used_ai_on_corresponding_item, d)

library(tidyverse)
library(lme4)
library(ggplot2)
library(ggeffects)

# Read and filter data
d <- read_csv('regression_data.csv') %>%
  filter(str_detect(problem_name, '^10'))

# Fit the model
m <- glmer(post_test_score ~ (1 | student_id) + used_ai_on_corresponding_item, 
           data = d, family = 'binomial')

# Get predicted probabilities with confidence intervals
preds <- ggeffect(m, terms = "used_ai_on_corresponding_item") %>%
  as_tibble()

# Create a publication-ready plot
ggplot(preds, aes(x = as.factor(x), y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
  geom_errorbar(width = 0.2, color = "black") +
  labs(x = "Used AI on Corresponding Item",
       y = "Predicted Probability of Post-Test Success",
       title = "Effect of AI Usage on Post-Test Score") +
  coord_cartesian(ylim = c(0.7, 1)) +
  theme_bw(base_size = 14) +
  theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))


