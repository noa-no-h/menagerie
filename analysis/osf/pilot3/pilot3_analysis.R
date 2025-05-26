# Setup -------------------------------------------------------------------
if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'RColorBrewer', 'extrafont',
              'this.path', 'brms', 'bayestestR', 'rstan', 'posterior', 'parallel', 'doParallel')
p_load(char = pkg.names)

setwd(here())

RANDOM_SEED = 123
set.seed(RANDOM_SEED)

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

print(median(demographics$total_time))

to_exclude <- union(attention_exclude, tab_away_exclude)

number_subjects <- n_distinct(data$subject)
number_to_exclude <- length(to_exclude)
print(number_subjects)
print(number_to_exclude)

# Get sample size pre-exclusion
length(unique(data$subject[data$version == 'pilot3a']))
length(unique(data$subject[data$version == 'pilot3b']))

data <- data %>%
  filter(!subject %in% to_exclude,
         !is.na(factor))

number_subjects <- n_distinct(data$subject)
number_to_exclude <- length(to_exclude)
print(number_subjects)
print(number_to_exclude)

length(unique(data$subject[data$version == 'pilot3a']))
length(unique(data$subject[data$version == 'pilot3a' & data$factor == 'experience']))
length(unique(data$subject[data$version == 'pilot3a' & data$factor == 'control']))

length(unique(data$subject[data$version == 'pilot3b']))

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
  labs(title = "", x = "Group", y = "Benefit ratings") +
  theme_custom()+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+
  scale_x_discrete(labels = c("Experimental\n(Passage)", "Control\n(No passage)"))

affect_analysis = brm(choice ~ condition,
                      data = affect_data %>% mutate(choice = scale(choice),
                                                    condition = factor(condition, levels = c("without passage", "With passage"))),
                      save_pars = save_pars(group = F),
                      prior = default_priors,
                      seed=RANDOM_SEED)
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
                           prior = default_priors,
                           seed=RANDOM_SEED)
  
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
  labs(title = "", x = "Group", y = "% chance of British Victory") +
  theme_custom()+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+
  scale_x_discrete(labels = c('Experimental\n(Knows outcome)', "Control\n(Does not know)"))


ggplot(summary_hindsight_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +  # Bar chart with transparency
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), 
                width = 0.2) +  # Error bars
  geom_jitter(data = hindsight_data, aes(x = condition, y = choice), 
              width = 0.2, alpha = 0.6, color = "black", size = 2) +  # Individual data points
  labs(title = "Hindsight", x = "Condition", y = "Percent Likelihood of British Victory") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom() +
  scale_fill_manual(values = exp_control) +
  guides(fill = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

hindsight_analysis = brm(choice ~ condition,
                         data = hindsight_data %>% mutate(choice = scale(choice),
                                                          condition = factor(condition, levels = c("no knowledge of outcome", "knowledge of outcome"))),
                         save_pars = save_pars(group = F),
                         prior = default_priors,
                         seed=RANDOM_SEED)
summary(hindsight_analysis)
hdi(hindsight_analysis)

# Order effect ----


primacy_data <- data %>%
  filter(version == 'pilot3b', task_name == "primacy order", choice %in% c('car1', 'car2')) %>%
  mutate(factor = recode(factor, "F" = "Factor-Included"),
         car_1_or_2 = ifelse(choice == "car1", 1, 0))

primacy_graph_data <- primacy_data %>%
  group_by(choice) %>%
  summarise(count = n()) %>%
  mutate(
    total_count = sum(count),
    proportion = count / total_count,
    percent = proportion * 100,
    se = sqrt(proportion * (1 - proportion) / count) * 100 
  )

ggplot(primacy_graph_data, aes(x = choice, y = percent, fill = choice)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = percent - se, ymax = percent + se),
    width = 0.2,
    color = "black",
    linewidth = 0.5 
  ) +
  scale_x_discrete(labels = c("car1" = "Positive\nfirst", "car2" = "Negative\nfirst"))+
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(
    x = "Within-subject condition",
    y = "Percentage chosen",
    fill = "Car"
  ) +
  theme_custom() +
  guides(fill = FALSE) +
  scale_fill_manual(values = exp_control)


primacy_analysis = brm(car_1_or_2 ~ 1, 
                       primacy_data,
                       family = 'bernoulli',
                       save_pars = save_pars(group = F),
                       seed=RANDOM_SEED)
summary(primacy_analysis)
hdi(primacy_analysis)
summarise_draws(primacy_analysis)
check_divergences(primacy_analysis$fit)


# Status quo ----

#When subjects were told the status quo, 
#were they more likely to recommend the 70/30 allocation?

status_quo_data = data %>%
  filter(task_name == "status_quo") %>%
  filter(stimulus != "comprehension") %>%
  mutate(choice = ifelse(auxiliary_info1 == "Allocate 50% to auto safety and 50% to highway safety status quo: 50/50", 
                         "status quo", 
                         choice))%>%
  mutate(choice_binary = as.numeric(choice == "status quo"))

status_quo_graph_data = status_quo_data %>%
  group_by(factor) %>%
  summarize(
    n = n(),
    p = mean(choice_binary),
    se = sqrt(p * (1 - p) / n),
    lower = p - se,
    upper = p + se,
    percentage = p * 100,
    percentageLower = lower * 100,
    percentageUpper = upper * 100
  )

ggplot(status_quo_graph_data, aes(x = factor, y = percentage, fill = factor)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = percentageLower, ymax = percentageUpper),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  scale_fill_manual(
    values = c("#F37121", "#4793AF"), 
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = c("Experimental\n(Status quo)", "Control\n(No status quo)")) + # Changed the labels here
  labs(
    y = "% who chose 50/50 allocation",
    x = "Group"
  ) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
  theme_custom() 

status_quo_analysis = brm(choice_binary ~ condition,
                        data = status_quo_data,
                        family = 'bernoulli',
                        save_pars = save_pars(group = F),
                        prior = default_priors,
                        seed=RANDOM_SEED)
summary(status_quo_analysis)
hdi(status_quo_analysis)


# Sunk cost ----

sunk_cost_data = data %>%
  filter(task_name == "sunk_cost2 effect") %>% 
  mutate(stayed = choice == "Continue Investing")

sunk_cost_graph_data = sunk_cost_data %>%
  group_by(factor) %>%
  summarize(
    n = n(),
    p = mean(stayed),
    se = sqrt(p * (1 - p) / n),
    lower = p - se,
    upper = p + se,
    percentage = p * 100,
    percentageLower = lower * 100,
    percentageUpper = upper * 100
  )

ggplot(sunk_cost_graph_data, aes(x = factor, y = percentage, fill = factor)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = percentageLower, ymax = percentageUpper),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  scale_fill_manual(
    values = c("#F37121", "#4793AF"), 
    guide = "none"
  ) +
  geom_text(aes(label = paste0("n=", n)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = c("Experimental\n(Sunk cost)", "Control\n(No sunk cost)")) + # Changed the labels here
  labs(
    y = "Percentage staying",
    x = "Group"
  ) +
  theme_custom() 

sunk_cost_analysis = brm(stayed ~ condition,
                        data = sunk_cost_data,
                        family = 'bernoulli',
                        save_pars = save_pars(group = F),
                        prior = default_priors,
                        seed=RANDOM_SEED)
summary(sunk_cost_analysis)
hdi(sunk_cost_analysis)

# Save image --------------------------------------------------------------

save.image('pilot3_output.rdata')
