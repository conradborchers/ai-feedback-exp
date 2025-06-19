library(tidyverse)
library(lme4)
library(lmerTest)

d <- read_csv('full_sample_logs_ectel25.csv')
d_join <- read_csv('ai_usage-ectel2025.csv')
d <- d %>% left_join(d_join, by=c('student_id', 'lesson')) %>% janitor::clean_names()

# Lesson completions per student
d %>%
  distinct(anon_student_id, level_lesson) %>%
  count(anon_student_id) %>%
  mutate(mean(n), sd(n))

# Propensity exploration features for testing if we can predict propensity to seek AI help
d_confidence_join <- d %>% 
  filter(str_detect(problem_name, 'How confident are you|experience and skills')) %>%
  group_by(anon_student_id) %>%
  summarize(confidence = mean(as.numeric(input), na.rm=TRUE))

library(dplyr)
library(lubridate)
library(stringr)

# Convert time columns to POSIXct format for time-based calculations
d2 <- d %>%
  mutate(
    problem_start_time = ymd_hms(problem_start_time, tz = "UTC"),
    cf_tool_event_time = ymd_hms(cf_tool_event_time, tz = "UTC"),
    cf_tutor_event_time = ymd_hms(cf_tutor_event_time, tz = "UTC"),
    input_length = ifelse(is.na(input), 0, str_length(input)), # Length of input text
    feedback_length = ifelse(is.na(feedback_text), 0, str_length(feedback_text)) # Length of feedback text
  )

d_student_summary <- d2 %>%
  group_by(anon_student_id) %>%
  summarize(
    total_duration = sum(as.numeric(duration_sec), na.rm = TRUE),
    avg_duration = mean(as.numeric(duration_sec), na.rm = TRUE),
    min_duration = min(as.numeric(duration_sec), na.rm = TRUE),
    max_duration = max(as.numeric(duration_sec), na.rm = TRUE),
    range_duration = max_duration - min_duration,
    
    # Input Length Features
    min_input_length = min(input_length, na.rm = TRUE),
    max_input_length = max(input_length, na.rm = TRUE),
    avg_input_length = mean(input_length, na.rm = TRUE),
    median_input_length = median(input_length, na.rm = TRUE),  # Median input length
    sd_input_length = sd(input_length, na.rm = TRUE),  # Standard deviation of input lengths
    IQR_input_length = IQR(input_length, na.rm = TRUE),  # Interquartile range of input lengths
    total_input_length = sum(input_length, na.rm = TRUE),  # Total input length
    
    # Session-based features
    num_navigation_steps = sum(action == "select page number", na.rm = TRUE),
    avg_time_between_attempts = mean(difftime(lead(problem_start_time), problem_start_time, units = "secs"), na.rm = TRUE),
    
    # Question type features
    num_explain_questions = sum(question_type == "explain", na.rm = TRUE),
    num_predict_questions = sum(question_type == "predict", na.rm = TRUE),
    
    # Response features
    num_generated_responses = sum(response_type == "generated", na.rm = TRUE),
    num_selected_responses = sum(response_type == "selected", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(d_confidence_join)

d <- d %>% 
  filter(!is.na(ai_condition)) %>%
  filter(cf_condition=='Mixed')

# MC item check analysis
d_items <- d %>% 
  filter(response_type=='selected') %>%
  mutate(score = ifelse(outcome=='CORRECT', 1, ifelse(is.na(outcome), NA, 0))) %>%
  filter(!is.na(score)) %>%
  mutate(item = paste(lesson,response_type,question_type)) %>%
  select(anon_student_id, item, score) %>%
  distinct(anon_student_id, item, .keep_all = TRUE)

d_items %>%
  group_by(item) %>% 
  summarize(difficulty = 1-mean(score)) %>%
  arrange(desc(difficulty))

d_lesson_items <- d %>% 
  filter(!is.na(score)) %>%
  select(anon_student_id, lesson,response_type,question_type, score) %>%
  distinct(anon_student_id, lesson,response_type,question_type, .keep_all = TRUE) %>%
  mutate(item = paste(response_type, question_type)) %>%
  select(lesson, anon_student_id, item, score)

lessons <- d_lesson_items$lesson %>% unique() %>% sort()
for (lesson_filter in lessons) {
  cronbachs <- d_lesson_items %>%
    filter(lesson == lesson_filter) %>%
    select(-lesson) %>%
    pivot_wider(names_from = item, values_from = score) %>%
    janitor::clean_names() %>%
    filter(!is.na(selected_explain)) %>%
    filter(!is.na(generated_predict)) %>%
    filter(!is.na(selected_predict)) %>%
    filter(!is.na(generated_explain)) %>%
    select(-anon_student_id) %>%
    psych::alpha()
  print(lesson_filter)
  print(cronbachs$total)
  print(cronbachs$alpha.drop)
}

# Remove unreliable items
d['wordcount'] <- d$input %>% sapply(ngram::wordcount) %>% as.numeric()
d_time_save <- d # for later time analysis
d <- d %>%
  filter(!(lesson=='Determining What Students Know' & response_type=='selected' & question_type=='explain')) %>%
  filter(!(lesson=='Reacting to Errors' & response_type=='selected' & question_type=='explain')) %>%
  filter(!(lesson=='Responding to Negative Self-Talk' & response_type=='selected' & question_type=='predict'))

# TOT response length

d_anova <- d %>%
  filter(posttest) %>%
  group_by(anon_student_id, lesson) %>%
  summarize(score = mean(score, na.rm=TRUE), 
            got_ai=sum(n_received_ai, na.rm=TRUE)>0,
            n_ai_requests=sum(n_received_ai, na.rm=TRUE),
            ai_condition=sum(ai_condition, na.rm=TRUE)>0
  ) %>%
  ungroup()

d_response_length <- d_time_save %>%
  left_join(
    d_anova %>% distinct(anon_student_id, lesson, got_ai),
    by=c('anon_student_id', 'lesson')
  ) %>%
  filter(posttest) %>%
  filter(response_type=='generated') %>%
  group_by(anon_student_id, lesson, got_ai) %>%
  summarize(response_length = mean(wordcount)) %>%
  ungroup()

d_response_length %>%
  group_by(got_ai) %>%
  summarize(mean_response_length = mean(response_length), sd_response_length = sd(response_length)) %>%
  ungroup()

m <- lmer(response_length ~ got_ai + (1 | anon_student_id) + (1 | lesson), d_response_length)
sjPlot::tab_model(m)
t.test(d_response_length$response_length[d_response_length$got_ai], 
       d_response_length$response_length[!d_response_length$got_ai])

# TOT
d_tot <- d_anova %>% distinct(anon_student_id, lesson, got_ai)
d_anova_tot <- d_anova
m <- lmer(scale(score) ~ (1 | lesson) + got_ai + (1|anon_student_id), d_anova) # intent to treat is in ai_condition
performance::icc(m, by_group = TRUE)
sjPlot::tab_model(m) #

xtabs(~got_ai + ai_condition, d_anova)

d_anova %>%
  group_by(lesson, got_ai, ai_condition) %>%
  summarize(mean_score = paste(round(mean(score)*100, 2), '%', sep=''), nn=n()) %>% as.data.frame()

# Propensity score model try to predict for principal stratification
d_prins <- d_anova %>% 
  filter(ai_condition)
ps_model <- glmer(n_ai_requests ~ (1|anon_student_id) + (1|lesson), data = d_prins, family = 'poisson')
d_prins$propensity_score <- predict(ps_model, type = "response")

d_prins <- d_prins %>% 
  group_by(anon_student_id) %>%
  summarize(propensity_score = sum(n_ai_requests)) %>%
  left_join(d_student_summary) %>%
  select(-anon_student_id) %>%
  mutate_all(as.numeric) %>%
  na.omit()

library(caret)
library(glmnet)
library(pROC)

# Step 1: Prepare Data
predictors <- d_prins[, setdiff(names(d_prins), "propensity_score")]
x <- as.matrix(predictors)
y <- d_prins$propensity_score

# Step 2: Set Up Cross-Validation for Hyperparameter Tuning
train_control <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = defaultSummary,
  savePredictions = "final"
)

# Step 3: Define Grid of Hyperparameters (alpha and lambda)
grid <- expand.grid(
  alpha = seq(0, 1, by = 0.2),   # Ridge (0) to Lasso (1)
  lambda = 10^seq(-4, 1, length = 100) 
)

# Step 4: Train Model with Grid Search
set.seed(123)
tuned_model <- train(
  x,
  y,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = grid,
  metric = "MAE"
)

# Step 5: Extract Best Hyperparameters
best_alpha <- tuned_model$bestTune$alpha
best_lambda <- tuned_model$bestTune$lambda

cat("Best alpha:", best_alpha, "\n")
cat("Best lambda:", best_lambda, "\n")

# Step 6: Train the Final Joint Model Using Best Parameters
final_model <- glmnet(x, y, alpha = best_alpha, lambda = best_lambda)

# Step 7: Evaluate Final Model on Full Data (Optional)
# Continuous Performance (RMSE)
final_preds <- predict(final_model, newx = x)
final_rmse <- sqrt(mean((y - final_preds)^2))

# Binary Performance (AUC) using Median Split
median_split <- median(y)
binary_actuals <- ifelse(y > median_split, 1, 0)
final_auc <- roc(binary_actuals, as.vector(final_preds))$auc

cat("Final RMSE on Full Data:", final_rmse, "\n")
cat("Final AUC on Full Data:", final_auc, "\n")

# Prepare student summary dataset for prediction
d_generalize_x <- d_student_summary %>%
  select(-anon_student_id) %>%
  mutate_all(as.numeric) %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))  # Mean imputation

x_generalize <- as.matrix(d_generalize_x)

# Predict propensity for all students
d_student_summary$n_requests_predicted <- predict(final_model, newx = x_generalize, s = best_lambda)

d_student_summary$n_requests_predicted <- ifelse(d_student_summary$n_requests_predicted<0, 0, d_student_summary$n_requests_predicted)

d_join_n_lessons <- d %>%
  group_by(anon_student_id) %>%
  summarize(n_lessons = length(unique(level_lesson))) %>%
  ungroup()

d_student_summary <- d_student_summary %>%
  left_join(d_join_n_lessons, by='anon_student_id') %>%
  mutate(n_requests_predicted = n_requests_predicted/n_lessons) 

d_student_summary$n_requests_predicted_log <- log((d_student_summary$n_requests_predicted + 1e-3))

# Categorize into high vs. low propensity groups (based on median)
median_propensity <- median(d_student_summary$n_requests_predicted, na.rm = TRUE)
d_student_summary$high_ai_stratum <- factor(as.numeric(d_student_summary$n_requests_predicted_log > median_propensity)) # binarize

d_strat_join <- d_student_summary %>% select(anon_student_id, matches('strat|n_requests_predicted'))

library(lme4)
library(ggplot2)
library(ggeffects)
library(performance)
library(sjPlot)

d_tot_strat <- d_anova_tot %>% left_join(d_strat_join) %>% mutate_if(is.numeric, scale)
d_tot_strat$ai_condition <- as.factor(as.numeric(d_tot_strat$ai_condition))
m <- lmer(scale(score) ~ (1 | lesson) + ai_condition*n_requests_predicted_log + (1 | anon_student_id), d_tot_strat) # intent to treat is in ai_condition
performance::icc(m, by_group = TRUE)
sjPlot::tab_model(m) #
plot(m)

d_tot_strat %>%
  mutate(median_prop = median(n_requests_predicted_log)) %>%
  group_by(n_requests_predicted>median_prop, ai_condition) %>%
  summarize(mean(score), sd(score))

preds <- ggpredict(m, terms = c("n_requests_predicted [all]", "ai_condition"))
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "Propensity to Request AI Feedback (SD)",
    y = "Standardized Score (SD)",
    color = "AI Feedback (ITT Condition)",
    fill = "AI Feedback (ITT Condition)"
  ) +
  theme_bw(base_size = 12) +  # APA-compliant font size
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) + 
  xlim(c(-2.5, 3.5)) + 
  ylim(-0.8, 0.9)

