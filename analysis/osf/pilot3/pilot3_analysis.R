# Setup -------------------------------------------------------------------
if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'RColorBrewer', 'extrafont',
              'this.path', 'brms', 'bayestestR', 'rstan', 'posterior', 'parallel', 'doParallel')
p_load(char = pkg.names)

setwd(here())

set.seed(123)

# color palettes

exp_control <- c("#F37121", "#4793AF")
exp_neutral_control <- c("#F37121", "#D3D3D3", "#4793AF")
effect_no <- c("#e74c3c", "#D3D3D3")

theme_custom <- function() {
  theme_minimal(base_family = "Optima") +
    theme(
      axis.text.x = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 1)), 
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      strip.text = element_text(size = 15),
      aspect.ratio = 1,  # Set the aspect ratio here
      panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
      panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
    )
}

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
dodge <- position_dodge(width=0.9)
default_priors <- set_prior("normal(0,1)", class = 'b')

# Load data ---------------------------------------------------------------

data <- read.csv('pilot3_data.csv') %>%
  filter(subject != "") %>%
  arrange(subject, task_name) %>%
  mutate(factor = factor(factor, c("Factor-Included", "Factor-Excluded"), c("experience", "control")))

subjects_all = data %>%
  pull(subject) %>%
  unique()

#find subjects who need to be excluded

attention_exclude <- data %>%
  filter((`task_name` == "attention check 2" & `auxiliary_info1` == "Failure") |
           (`task_name` == "attention check 3" & `auxiliary_info1` == "Incorrect")) %>%
  pull(subject)

events <- read.csv('pilot3_browser_events.csv') %>%
  arrange(subject)

events_subj <- events %>%
  filter(browser_event == "blur") %>%
  group_by(subject) %>%
  summarize(blurs = n())

ggplot(events_subj, aes(x = blurs)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(title = "Blur Histogram", x = "Number of Blurs", y = "Count") +
  theme_custom()

tab_away_exclude <- events_subj %>% 
  filter(blurs > 20) %>%
  pull(subject)

demographics <- read.csv('pilot3_demographics.csv') %>%
  arrange(subject) %>% 
  mutate(total_time = total_time/60000)

ggplot(demographics, aes(x = total_time)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Time Histogram", x = "Minutes", y = "Count") +
  theme_custom()

to_exclude <- union(attention_exclude, tab_away_exclude)

number_subjects <- n_distinct(data$subject)
number_to_exclude <- length(to_exclude)
print(number_subjects)
print(number_to_exclude)

data <- data %>%
  filter(!subject %in% to_exclude)

#font_import(pattern = "Optima", prompt = FALSE)
loadfonts(device = "pdf")

# Affect heuristic ----

affect_data = data %>%
  filter(task_name == "affect heuristic")%>%
  mutate(choice = as.numeric(choice)) 

summary_affect_data <- affect_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("With passage", "without passage"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(summary_affect_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Affect", x = "Condition", y = "How beneficial is natural gas") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

affect_analysis = brm(choice ~ factor,
                      data = affect_data %>% mutate(choice = scale(choice)),
                      save_pars = save_pars(group = F),
                      prior = default_priors)
summarise_draws(affect_analysis)
check_divergences(affect_analysis$fit)
summary(affect_analysis)
hdi(affect_analysis)

# Power analysis (b/c affect heuristic is the smallest effect size)
run_power_analysis = F # change this to T to run the power analysis yourself

if (run_power_analysis) {
  sample_sizes = c(150, 200)
  num_runs_per = 100
  numCores = 5
  registerDoParallel(numCores)
  results_all = vector(mode = 'list', length = length(sample_sizes))
  
  for (i in 1:length(sample_sizes)) {
    sample_size = sample_sizes[i]
  
    new_data_template = affect_data %>%
      distinct(subject) %>%
      slice_sample(n = sample_size, replace = T) %>%
      group_by(subject) %>%
      mutate(instance = row_number()) %>%
      ungroup() %>%
      left_join(affect_data, by = 'subject') %>%
      mutate(subject = str_c(subject, instance, sep = "_"))
  
    post_draws = posterior_predict(affect_analysis,
                                   newdata = new_data_template,
                                   ndraws = num_runs_per,
                                   allow_new_levels = T)
  
    results = foreach(j = 1:num_runs_per, .combine = "rbind") %dopar% {
      new_data = new_data_template
      new_data$introspect_rating = t(post_draws)[,j]
  
      power_analysis = brm(choice ~ factor,
                           data = new_data %>% mutate(choice = scale(choice)),
                           save_pars = save_pars(group = F),
                           prior = default_priors)
  
      power_analysis_hdi = bayestestR::hdi(power_analysis)
      coef_estimate = summary(power_analysis)$fixed$Estimate[2]
      hdi_high = power_analysis_hdi$CI_high[2]
      hdi_low = power_analysis_hdi$CI_low[2]
      list(coef_estimate, hdi_low, hdi_high)
    }
  
    results_df <- as.data.frame(results)
    results_df = data.frame(lapply(results_df, unlist))
    colnames(results_df) <- c("coef_estimate", "hdi_low", "hdi_high")
    rownames(results_df) <- paste0("Run_", 1:num_runs_per)
    results_all[[i]] = results_df %>%
      mutate(hdi_width = hdi_high - hdi_low,
             hdi_significant = hdi_low > .05)
  }
}

# Hindsight ----


hindsight_data = data %>%
  filter(task_name == "hindsight effect")%>%
  filter(stimulus != "comprehension") %>%
  mutate(choice = as.numeric(choice)) 

summary_hindsight_data <- hindsight_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("knowledge of outcome", "no knowledge of outcome"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(summary_hindsight_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Hindsight", x = "Condition", y = "Percent Likelihood of British Victory") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

hindsight_analysis = brm(choice ~ factor,
                         data = hindsight_data %>% mutate(choice = scale(choice)),
                         save_pars = save_pars(group = F),
                         prior = default_priors)
summary(hindsight_analysis)
hdi(hindsight_analysis)

# # Order effect ----
# 
# primacy_data <- data %>%
#   filter(task_name == "primacy order") %>%
#   filter(choice != "")%>%
#   mutate(choice_fac = ifelse(choice == "car1", 
#                          "chose primacy car", 
#                          "chose other car")) %>%
#   mutate(choice_binary = as.numeric(choice_fac == "chose primacy car"))%>%
#   mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"), labels = c('experience', 'control'))) 
# 
# 
# summary_primacy_data <- primacy_data %>%
#   group_by(condition) %>%
#   summarize(
#     mean_choice = mean(choice_binary),
#     se_choice = se.prop(choice_binary),
#     count = n()
#   )
# 
# summary_primacy_data2 <- primacy_data %>%
#   group_by(condition, choice) %>%
#   summarize(
#     count = n(),
#     .groups = 'drop'
#   )
# 
# ggplot(summary_primacy_data, aes(x = condition, y = mean_choice, fill = condition)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Choices of the primacy car", x = "Condition", y = "Percent who chose primacy car") +
#   geom_text(aes(label = paste0("n=", count)), 
#             position = position_dodge(0.9), vjust = -0.5, 
#             family = "Optima") +
#   theme_custom() +
#   scale_fill_manual(values = exp_control)+
#   guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))
# 
# 
# primacy_analysis = brm(choice_binary ~ condition,
#                        data = primacy_data,
#                        family = 'bernoulli',
#                        save_pars = save_pars(group = F),
#                        prior = default_priors)
# summary(primacy_analysis)
# hdi(primacy_analysis, effects = 'all')

# Status quo ----

#When subjects were told the status quo, 
#were they more likely to recommend the 70/30 allocation?


status_quo_data <- data %>%
  filter(task_name == "status_quo") %>%
  filter(stimulus != "comprehension") %>%
  mutate(choice = ifelse(auxiliary_info1 == "Allocate 50% to auto safety and 50% to highway safety status quo: 50/50", 
                         "status quo", 
                         choice)) %>%
  mutate(choice_binary = as.numeric(choice == "status quo"))%>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) 

summary_status_quo_data <- status_quo_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary),
    count = n()
  )

ggplot(summary_status_quo_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  labs(title = "Choices to continue the status quo", x = "Condition", y = "Percent subjects who recommended the status quo") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom() +
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


status_quo_analysis = brm(choice_binary ~ condition,
                        data = status_quo_data,
                        family = 'bernoulli',
                        save_pars = save_pars(group = F),
                        prior = default_priors)
summary(status_quo_analysis)
hdi(status_quo_analysis)


# Sunk cost ----

sunk_cost_data <- data %>%
  filter(task_name == "sunk_cost2 effect") %>% 
  mutate(switched = choice == "Don't Continue Investing",
         switched.num = as.numeric(switched))


percentage_sunk_cost_data <- sunk_cost_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Sunk Cost", "No Sunk Cost"))) %>%
  summarize(
    total_in_condition = n(),  # Total number of subjects in each condition
    switched_count = sum(choice == "Don't Continue Investing")  # Count who chose "Solar-powered pump"
  ) %>%
  mutate(percentage_switched = (switched_count / total_in_condition) * 100)

ggplot(percentage_sunk_cost_data, aes(x = condition, y = percentage_switched, fill = condition)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Who Stopped Investing", x = "Condition", y = "Percentage of Choices to Stop Investing") +
  geom_text(aes(label = paste0("n=", total_in_condition)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

sunk_cost_analysis = brm(switched.num ~ condition,
                        data = sunk_cost_data,
                        family = 'bernoulli',
                        save_pars = save_pars(group = F),
                        prior = default_priors)
summary(sunk_cost_analysis)
hdi(sunk_cost_analysis)

# Save image --------------------------------------------------------------

save.image('pilot3_output.rdata')
