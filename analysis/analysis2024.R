# Setup -------------------------------------------------------------------
library(extrafont)
require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(sjPlot)
require(magrittr)
require(readr)
library(stringr)
library(RColorBrewer)
library(extrafont)
library(Cairo)
library(tidyr)
library(brms)









setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
dodge <- position_dodge(width=0.9)


# color palettes: hot for included, cool for excluded

#Included vs. Excluded
in_and_ex <- c("#F37121", "#4793AF")

in_neutral_ex <- c("#F37121", "#D3D3D3", "#4793AF")

effect_no <- c("#e74c3c", "#D3D3D3")

#In&Effect, In&NoEffect, Ex&Effect, Ex&NoEffect
four_colors <- c("#f1c40f", "#e74c3c","#9b59b6", "#1abc9c")

#In&Effect, In&NoEffect, Ex
three_colors <- c("#f1c40f", "#e74c3c","#4793AF")

#In&Effect, Ex
two_colors <- c("#f1c40f", "#e74c3c")

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

#font_import(pattern = "Optima", prompt = FALSE)
#loadfonts(device = "pdf")


data <- read.csv('data.csv') %>%
  arrange(subject, task_name) %>%
  mutate(total_time = ifelse(grepl("^total_time", introspect_rating), introspect_rating, NA)) %>%
  mutate(introspect_rating = ifelse(grepl("^total_time", introspect_rating), NA, introspect_rating)) %>%
mutate(introspect_rating = as.numeric(introspect_rating))%>%
  mutate(introspect_rating = if_else(
    introspect_rating != "" & task_name %in% c("associative memory", "availability", 
                                               "decoy effect", "hindsight bias", 
                                               "omission principle", "reference price",
                                               "status_quo", "sunk_cost effect"),
    100 - introspect_rating,
    introspect_rating
  ))

View(data)

data = data %>%
  filter(familiarity != "Yes") %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded")))
  

#find subjects who need to be excluded
  
attention_exclude <- data %>%
  filter((`task_name` == "attention check 2" & `auxiliary_info1` == "Failure") |
           (`task_name` == "attention check 3" & `auxiliary_info1` == "Incorrect")) %>%
    pull(subject)
    
  events <- read.csv('browser_events.csv') %>%
    arrange(subject) %>%
    filter(version == "v5_pilot1")
  
  tab_away_exclude <- events %>%
    filter(browser_event == "blur") %>%
    group_by(subject) %>%
    summarize(blurs = n(), more_than_twelve = as.numeric(n() > 12))%>%
    filter(more_than_twelve == 1) %>%
    pull(subject)
  
  blur_histogram_data <- events %>%
    filter(browser_event == "blur") %>%
    group_by(subject) %>%
    summarize(blurs = n(), more_than_twelve = as.numeric(n() > 12))
    
  
  ##View(attention_exclude)
  #blur histogram
  
  ggplot(blur_histogram_data, aes(x = blurs)) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(title = "Blur Histogram", x = "Number of Blurs", y = "Count") +
    theme_custom()
  
  time_exclude <- data %>%
    filter(total_time != "") %>%
    mutate(total_time = parse_number(total_time)) %>%
    mutate(total_time = total_time/60000)
 
   #View(time_exclude)
  
  ggplot(time_exclude, aes(x = total_time)) +
    geom_histogram(fill = "skyblue", color = "black") +
    labs(title = "Time Histogram", x = "Minutes", y = "Count") +
    theme_custom()
  
  to_exclude <- union(attention_exclude, tab_away_exclude)
  
  
  
  data <- data %>%
    filter(!subject %in% to_exclude)
  
  

p.vals = c()


# 1 anchoring effect✅  -------------------------------------------------
    ## 1.1 Do we see the effect -----------------------------------------------------------------------


    ### Antarctica -----------------------------------------------------------------------


anchor_antarctica_data <- data %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus == "Antarctic Temperature") %>%
  mutate(choice = as.numeric(choice)) 
  

#violin
ggplot(anchor_antarctica_data, aes(x = condition, y = choice)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  labs(title = "Distribution of Estimates by Anchor Presence", x = "Anchor Presence", y = "Estimate") +
  theme_custom()

#bar
summary_anchor_antarctica_data <- anchor_antarctica_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Low Anchor", "No Anchor"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice)
  )


ggplot(summary_anchor_antarctica_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Estimates by Anchor Presence", x = "Anchor Presence", y = "Mean Estimate") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


#analysis

low_anchor <- anchor_antarctica_data %>%
  filter(condition == "Low Anchor") %>%
  pull(choice)

no_anchor <- anchor_antarctica_data %>%
  filter(condition == "No Anchor") %>%
  pull(choice)

t_test_result <- t.test(low_anchor, no_anchor)

print(t_test_result)
#p-value = 0.0007

#brms

# Recode the condition levels
anchor_antarctica_data <- anchor_antarctica_data %>%
  mutate(condition = recode(condition, "Low Anchor" = "Factor-Included", "No Anchor" = "Factor-Excluded"))

# Make sure that "Factor-Included" is the first level
anchor_antarctica_data <- anchor_antarctica_data %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded")))

# Specify the Bayesian mixed model
model <- brm(
  formula = bf(choice ~ condition + (1 + condition | subject) ),
  data = anchor_antarctica_data,
  family = gaussian(),  # Default; specifies a linear model for continuous outcome
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 5), class = "b"),  # Prior on condition effect
    prior(exponential(1), class = "sd")  # Prior on standard deviations of random effects
  ),
  iter = 4000,  # Increase if needed for convergence
  chains = 4,  # Number of MCMC chains
  cores = 4,  # Number of cores for parallel computing
  seed = 123  # Seed for reproducibility
)

# Summarize the model to see the results
summary(model)

# Get marginal effects for the condition variable
marginal_effects_data <- marginal_effects(model, "condition", method = "posterior_epred")
condition_effect <- as.data.frame(marginal_effects_data$condition)

# Plot the marginal effects with credible intervals
ggplot(condition_effect, aes(x = condition, y = estimate__)) +
  geom_bar(stat = "identity", aes(fill = condition), position = position_dodge()) +
  geom_errorbar(
    aes(ymin = lower__, ymax = upper__), width = 0.2, position = position_dodge(0.9)
  ) +
  labs(title = "Estimated Mean Choice by Factor Condition",
       x = "Condition",
       y = "Estimated Mean Choice") +
  scale_fill_manual(values = in_and_ex) +  # Assuming in_and_ex contains desired colors
  theme_custom()+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))




    ### Whale -----------------------------------------------------------------------
anchor_whale_data <- data %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus != "") %>%
  filter(stimulus == "Whale Length")
  
anchor_whale_data$choice <- as.numeric(anchor_whale_data$choice)

summary_anchor_whale_data <- anchor_whale_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = sd(choice)
  )


ggplot(anchor_whale_data, aes(x = condition, y = choice)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  labs(title = "Distribution of Estimates by Anchor Presence", x = "Anchor Presence", y = "Estimate") +
  theme_custom()

#analysis

low_anchor <- anchor_whale_data %>%
  filter(condition == "Low Anchor") %>%
  pull(choice)

no_anchor <- anchor_whale_data %>%
  filter(condition == "No Anchor") %>%
  pull(choice)

t_test_result <- t.test(low_anchor, no_anchor, var.equal = FALSE)

print(t_test_result)
#p-value = 0.83

    ## 1.2 introspection -----------------------------------------------------------------------

#did factor included give higher introspection numbers than factor excluded?

summary_anchoring_data <- data %>%
  filter(task_name == "anchoring") %>%
  filter(introspect_rating != "") %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  group_by(factor) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_anchoring_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Anchoring Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


#"intro_rating(included and lower than mean estimates) 
#> 
#  intro_rating(included and not lower than mean estimates)"

median_antarctica_estimate <- median(anchor_antarctica_data$choice[anchor_antarctica_data$factor == "Factor-Excluded"])

anchoring_subjects <- data %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus == "Antarctic Temperature") %>%
  mutate(choice = as.numeric(choice) ) %>%
  select(subject, factor, choice) %>%
  mutate(less_than_median = ifelse(choice < median_antarctica_estimate, 1, 0)) %>%
  select(subject, less_than_median, factor, choice)

View(anchoring_subjects)

#find subjects who were anchored
subjects_anchored = anchoring_subjects %>%
  group_by(subject) %>%
  filter(less_than_median == 1) %>%
  pull(subject)

# List of all subjects in "Factor-Included"
subjects_included = anchoring_subjects %>%
  filter(factor == "Factor-Included") %>%
  pull(subject) %>%
  unique()

#list of all subjects in "Factor-Excluded"
subjects_excluded = anchoring_subjects %>%
  filter(factor == "Factor-Excluded") %>%
  pull(subject) %>%
  unique()

#list of subjects included and anchored
subjects_included_and_anchored <- intersect(subjects_included, subjects_anchored)


# List of subjects in "Factor-Included" who were not anchored
subjects_included_and_not_anchored = setdiff(subjects_included, subjects_included_and_anchored)


#list of subjects excluded and anchored
subjects_excluded_and_anchored <- intersect(subjects_excluded, subjects_anchored)

#list of subjects excluded and not anchored
subjects_excluded_and_not_anchored = setdiff(subjects_excluded, subjects_excluded_and_anchored)


anchor_data <- data %>%
  filter(task_name == "anchoring") %>%
  filter(any(!is.na(introspect_rating))) %>%
  filter(introspect_rating != "") %>%
  mutate(effect_group = case_when(
    subject %in% subjects_included_and_anchored ~ "Included and Showing Effect",
    subject %in% subjects_included_and_not_anchored ~ "Included and Not Showing Effect",
    subject %in% subjects_excluded_and_anchored ~ "Excluded and Showing Effect",
    subject %in% subjects_excluded_and_not_anchored ~ "Excluded and Not Showing Effect",
      )) %>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Showing Effect", "Included and Not Showing Effect",  "Excluded and Showing Effect", "Excluded and Not Showing Effect")))

##View(anchor_data)

##View(subjects_excluded_and_not_anchored)

summary_anchor_data <- anchor_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )


ggplot(summary_anchor_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Anchor Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = anchor_data))

    ## New version ----

anchoring_introspection <- data %>%
  filter(task_name == "anchoring") %>%
  filter(introspect_rating != "")

anchoring_subjects <- data %>%
  group_by(subject) %>%
  left_join(anchoring_introspection %>% select(subject, introspect_rating), by = "subject") %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus == "Antarctic Temperature") %>%
  summarise(
    choice = as.numeric(choice),
    affected = if_else(choice < median_antarctica_estimate, 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
    factor = recode(factor(factor, levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating.y,
    condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded")) 
  ) %>%
  filter(!is.na(introspect_rating))

#View(anchoring_subjects)

summary_anchoring <- anchoring_subjects %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_anchoring)




ggplot(summary_anchoring, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Anchoring Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE)+
  scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = reference_data))

## brm -----


anchoring_introspection <- data %>%
  filter(task_name == "anchoring") %>%
  filter(introspect_rating != "")

anchoring_subjects <- data %>%
  group_by(subject) %>%
  left_join(anchoring_introspection %>% select(subject, introspect_rating), by = "subject") %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus == "Antarctic Temperature")%>%
  rename(estimate_of_effect_size = introspect_rating.y)%>%
  mutate(distance_from_anchor = abs(as.numeric(choice) + 30))


# Fit the Bayesian mixed-effects model
model <- brm(
  formula = estimate_of_effect_size ~ distance_from_anchor + (1 + factor | subject),
  data = anchoring_subjects,
  family = gaussian(),  # Assuming estimate_of_effect_size is continuous
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 5), class = "b"),  # Prior on fixed effects
    prior(exponential(1), class = "sd")  # Prior on random effects SDs
  ),
  iter = 4000,
  chains = 4,
  cores = 4,
  seed = 123
)

# Summarize the model
summary(model)


# Extract posterior samples for the fixed effect of `distance_from_anchor`
posterior_samples <- posterior_samples(model)

# Plot the posterior distribution of the effect of `distance_from_anchor`
ggplot(posterior_samples, aes(x = b_distance_from_anchor)) +
  geom_density(fill = "NA", alpha = 0.5) +
  labs(
    title = str_wrap("Posterior Distribution: Introspection on Anchor Effect", width = 30),
    x = str_wrap("Effect of Distance from Anchor on Estimate of Effect Size", width = 30),
    y = "Density"
  ) +
  theme_custom()
  

# 2 associative memory effect✅ ----
    ## 2.1 do we see the effect? -----------------------------------------------------------------------

#Did subjects for whom some of the new words were sleep-related 
#more often think they were original words than subjects for whom none of the new words were sleep-related? 

associative_data_only_new <- data %>%
  filter(task_name == "associative memory") %>%
  filter(stimulus!= "") %>% 
  filter(auxiliary_info1 == 'New') %>%
  mutate(false_alarm = ifelse(choice == "Original", 1, 0))

##View(associative_data_only_new)

summary_associative_data_only_new <- associative_data_only_new %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Sleep", "NonSleep"))) %>%
  summarize(
    mean_choice = mean(false_alarm),
    se_choice = se.prop(false_alarm)
  )

ggplot(summary_associative_data_only_new, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Associative Effect", x = "Word Relation", y = "False Alarm Rate") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


associative_choices_ex <- associative_data_only_new %>%
  filter(factor == "Factor-Excluded") %>%
  pull(false_alarm)

associative_choices_in <- associative_data_only_new %>%
  filter(factor == "Factor-Included") %>%
  pull(false_alarm)

#analysis -- is there a better way to do this?
prop_ex <- sum(associative_choices_ex) / length(associative_choices_ex)
prop_in <- sum(associative_choices_in) / length(associative_choices_in)

successes <- c(sum(associative_choices_ex), sum(associative_choices_in))
trials <- c(length(associative_choices_ex), length(associative_choices_in))

test_result <- prop.test(successes, trials, alternative = "less")
print(test_result)


    ## 2.2 introspection -----------------------------------------------------------------------

#did factor included give lower introspection numbers than factor excluded?

summary_associative_data <- data %>%
  filter(task_name == "associative memory") %>%
  filter(introspect_rating != "") %>%
  group_by(factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_associative_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Associative Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


#"intro_rating(included and more false positives than average)
#<
#  intro_rating(included and not more false positives than average)"


associative_data_by_subject <- associative_data_only_new %>%
  filter(task_name == "associative memory") %>%
  group_by(subject) %>%
  summarize(
    total_false_alarms = sum(false_alarm),
    factor = first(factor)
    )

###View(associative_data_by_subject)

#median false alarms among excluded
median_false_alarm_among_excluded <- median(associative_data_by_subject$total_false_alarms[associative_data_by_subject$factor == "Factor-Excluded"])

#find subjects who had higher than  median_false_alarm_among_excluded
subjects_more_false_alarms = associative_data_by_subject %>%
  group_by(subject) %>%
  summarize(higher = total_false_alarms > median_false_alarm_among_excluded) %>%
  filter(higher) %>%
  pull(subject)

# List of all subjects in "Factor-Included"
all_subjects_included = associative_data_by_subject %>%
  filter(factor == "Factor-Included") %>%
  pull(subject) %>%
  unique()

#list subjects in Factor-Excluded
all_subjects_excluded = associative_data_by_subject %>%
  filter(factor == "Factor-Excluded") %>%
  pull(subject) %>%
  unique()

included_and_high_false_alarms = intersect(all_subjects_included, subjects_more_false_alarms)

included_and_low_false_alarms = setdiff(all_subjects_included, subjects_more_false_alarms)

excluded_and_high_false_alarms = intersect(all_subjects_excluded, subjects_more_false_alarms)

excluded_and_low_false_alarms = setdiff(all_subjects_excluded, subjects_more_false_alarms)


associative_data <- data %>%
  filter(task_name == "associative memory") %>%
  filter(any(!is.na(introspect_rating))) %>%
  filter(introspect_rating != "") %>%
  mutate(effect_group = case_when(
    subject %in% included_and_high_false_alarms ~ "Included and More False Alarms",
    subject %in% included_and_low_false_alarms ~ "Included and Not More False Alarms",
    subject %in% excluded_and_high_false_alarms ~ "Excluded and More False Alarms",
    subject %in% excluded_and_low_false_alarms ~ "Excluded and Not More False Alarms",
  )) %>%
  mutate(effect_group = factor(effect_group, levels = c("Included and More False Alarms", "Included and Not More False Alarms", "Excluded and More False Alarms", "Excluded and Not More False Alarms")))



summary_associative_data <- associative_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

##View(associative_data)
##View(summary_associative_data)

ggplot(summary_associative_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Associative Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()  +
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = associative_data))

    ## New version ----

associative_introspection <- data %>%
  filter(task_name == "associative memory") %>%
  filter(introspect_rating != "")

associative_data_by_subject <- associative_data_only_new %>%
  filter(task_name == "associative memory") %>%
  group_by(subject) %>%
  summarize(
    total_false_alarms = sum(false_alarm),
    factor = first(factor)
  )


#median false alarms among excluded
median_false_alarm_among_excluded <- median(associative_data_by_subject$total_false_alarms[associative_data_by_subject$factor == "Factor-Excluded"])


View(associative)

associative <- associative_data_by_subject %>%
  group_by(subject) %>%
  left_join(associative_introspection %>% select(subject, introspect_rating), by = "subject") %>%
  summarise(
    total_false_alarms = total_false_alarms,
    affected = if_else(total_false_alarms > median_false_alarm_among_excluded, 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
    factor = recode(factor(factor, levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating  ) %>%
  filter(!is.na(introspect_rating))

#View(anchoring_subjects)

summary_associative <- associative %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_anchoring)


ggplot(summary_associative, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Associative Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE)+
  scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = reference_data))




# 3 availability effect✅  -----------------------------------------------------------------------

    ##3.1 do we see the effect ----

#Did subjects for whom the first list contained famous men say it contained 
#more men more often than subjects for whom the first list contained less famous men? 


availability_data = data %>%
  filter(task_name == "availability") %>%
  mutate(choice_binary = as.numeric(choice == "List 1"))


summary_availability_data <- availability_data %>%
  group_by(factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary)
  )

#View(summary_availability_data)


ggplot(summary_availability_data, aes(x = factor, y = mean_choice, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Availability Effect", x = "Factor", y = "Percent chance of being engineer") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

#analysis

list_one_ex = availability_data %>%
  filter(factor == "Factor-Excluded") %>%
  pull(choice_binary)

list_one_in = availability_data %>%
  filter(factor == "Factor-Included") %>%
  pull(choice_binary)

prop_ex <- sum(list_one_ex) / length(list_one_ex)
prop_in <- sum(list_one_in) / length(list_one_in)

successes <- c(sum(list_one_ex), sum(list_one_in))
trials <- c(length(list_one_ex), length(list_one_in))

test_result <- prop.test(successes, trials, alternative = "less")

print(test_result)

#p-value = 0.004

    ## 3.2 introspection  -----------------------------------------------------------------------

#did factor included give lower introspection numbers than factor excluded?

summary_availability_data <- availability_data %>%
  group_by(factor) %>%
  mutate(introspect_rating = as.numeric(introspect_rating)) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_availability_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Availability Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = availability_data)

#"intro_rating(included and said first list contained more famous men)
#<
#  intro_rating(included and not said first list contained more famous men)
#"

availability_data <- availability_data %>%
  mutate(effect_group = case_when(
    choice == "List 1" & factor == "Factor-Included" ~ "Included and List 1",
    choice == "List 2" & factor == "Factor-Included" ~ "Included and List 2",
    choice == "List 1" & factor == "Factor-Excluded" ~ "Excluded and List 1",
    choice == "List 2" & factor == "Factor-Excluded" ~ "Excluded and List 2"
  ))%>%
  mutate(effect_group = factor(effect_group, levels = c("Included and List 1", "Included and List 2", "Excluded and List 1", "Excluded and List 2")))


summary_availability_data <- availability_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_availability_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Availability Introspection Ratings", x = "Condition", y = "introspection rating") +
  theme_custom() +
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = availability_data))

    ## New version ----


availability_subjects <- availability_data %>%
  group_by(subject) %>%
  summarise(
    choice = choice,
    affected = if_else(choice == "List 1", 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
    factor = recode(factor(factor, levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded")) 
  ) %>%
  filter(!is.na(introspect_rating))

View(availability_subjects)

summary_availability <- availability_subjects %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_anchoring)




ggplot(summary_availability, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Availability Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = availability_data))




# 4. Belief effect✅  ----

    ## 4.1 do we see the effect? -----------------------------------------------------------------------

#Were subjects who saw the plausible version more likely to say the argument was sound?


belief_data <- data %>%
  filter(task_name == "belief") %>%
  filter(!str_detect(stimulus, "Practice")) %>%
  filter(stimulus != "") %>%
  mutate(choice_binary = as.numeric(choice == "Yes"))

summary_belief_data <- belief_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Unbelievable", "Believable"))) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary)
  )
ggplot(summary_belief_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Belief Effect", x = "Condition", y = "Percent saying acceptible") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


belief_choices_ex <- belief_data %>%
  filter(factor == "Factor-Excluded") %>%
  pull(choice_binary)

belief_choices_in <- belief_data %>%
  filter(factor == "Factor-Included") %>%
  pull(choice_binary)

#analysis -- is there a better way to do this?
prop_ex <- sum(belief_choices_ex) / length(belief_choices_ex)
prop_in <- sum(belief_choices_in) / length(belief_choices_in)

successes <- c(sum(belief_choices_ex), sum(belief_choices_in))
trials <- c(length(belief_choices_ex), length(belief_choices_in))

test_result <- prop.test(successes, trials, alternative = "less")
print(test_result)


    ## 4.2 introspection ----

#Did factor included give higher introspection numbers than factor excluded?

summary_belief_data <- data %>%
  filter(task_name == "belief") %>%
  filter(introspect_rating != "") %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  
  group_by(factor) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_belief_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Belief Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


#"intro_rating(included and more "acceptable"s than average)
#>
#intro_rating(included and not more "acceptable"s than average)"

belief_data <- data %>%
  filter(task_name == "belief") %>%
  filter(stimulus != "") %>%
  mutate(choice = as.numeric(choice))

##View(belief_data_by_subject)

belief_data_by_subject <- data %>%
  filter(task_name == "belief") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    number_total_yes = sum(choice == "Yes"),
    factor = first(factor),
    introspect_rating = first(introspect_rating[!is.na(introspect_rating)])
  )

#median total_yes among excluded
median_total_yes_among_excluded <- median(belief_data_by_subject$number_total_yes[belief_data_by_subject$factor == "Factor-Excluded"])

subjects_more_acceptable = belief_data_by_subject %>%
  group_by(subject) %>%
  summarize(higher = number_total_yes > median_total_yes_among_excluded) %>%
  filter(higher) %>%
  pull(subject)

# List of all subjects in "Factor-Included"
all_subjects_included = belief_data_by_subject %>%
  filter(factor == "Factor-Included") %>%
  pull(subject) %>%
  unique()

# List of all subjects in "Factor-Excluded"
all_subjects_excluded = belief_data_by_subject %>%
  filter(factor == "Factor-Excluded") %>%
  pull(subject) %>%
  unique()

subjects_included_and_more_acceptable = intersect(all_subjects_included, subjects_more_acceptable)

subjects_included_and_less_acceptable = setdiff(all_subjects_included, subjects_more_acceptable)

subjects_excluded_and_more_acceptable = intersect(all_subjects_excluded, subjects_more_acceptable)

subjects_excluded_and_less_acceptable = setdiff(all_subjects_excluded, subjects_more_acceptable)

belief_data <- data %>%
  filter(task_name == "belief") %>%
  filter(any(!is.na(introspect_rating))) %>%
  filter(stimulus == "") %>%
  mutate(effect_group = case_when(
    subject %in% subjects_included_and_more_acceptable ~ "Included and More Acceptable",
    subject %in% subjects_included_and_less_acceptable ~ "Included and Less Acceptable",
    subject %in% subjects_excluded_and_more_acceptable ~ "Excluded and More Acceptable",
    subject %in% subjects_excluded_and_less_acceptable ~ "Excluded and Less Acceptable",
      )) %>%
  mutate(effect_group = factor(effect_group, levels = c("Included and More Acceptable", "Included and Less Acceptable", "Excluded and More Acceptable", "Excluded and Less Acceptable")))


#View(belief_data)

summary_belief_data <- belief_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_belief_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Belief Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = belief))

    ## New version ----

belief_data_to_calculate_median <- data %>%
  filter(task_name == "belief") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    number_total_yes = sum(choice == "Yes"),
    factor = first(factor),
    introspect_rating = first(introspect_rating[!is.na(introspect_rating)])
  )

#median total_yes among excluded
median_total_yes_among_excluded <- median(belief_data_to_calculate_median$number_total_yes[belief_data_by_subject$factor == "Factor-Excluded"])


belief_data_to_calculate_median <- data %>%
  filter(task_name == "belief") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    number_total_yes = sum(choice == "Yes"),
    factor = first(factor),
    introspect_rating = first(introspect_rating[!is.na(introspect_rating)])
  )

median_total_yes_among_excluded <- median(belief_data_to_calculate_median$number_total_yes[belief_data_to_calculate_median$factor == "Factor-Excluded"])


belief_data_by_subject <- data %>%
  filter(task_name == "belief") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    number_total_yes = sum(choice == "Yes"),
    factor = recode(factor(first(factor), levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = first(introspect_rating[!is.na(introspect_rating)]),
    affected = if_else(number_total_yes > median_total_yes_among_excluded, 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
  )%>%
  filter(!is.na(introspect_rating))

View(belief_data_by_subject)

summary_belief <- belief_data_by_subject %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_anchoring)

ggplot(summary_belief, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Belief Bias Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE)+
  scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = belief_data))

# 5. causal inference✅  ----
    ## 5.1 do we see the effect? ----

#Did subjects who saw only one green ball 
#think it was more casual than subjects who saw many green balls?

causal_data <- data %>%
  filter(task_name == "causal inference") %>%
  mutate(choice = as.numeric(choice))

summary_causal_data <- causal_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("One", "Nine"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice)
  )

ggplot(summary_causal_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Causal Inference", x = "Condition", y = "Causality") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


t.test(choice ~ factor, data = causal_data)

    ## 5.2 introspection----

#Did factor included give higher introspection numbers than factor excluded?

summary_causal_data <- causal_data %>%
  group_by(factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_causal_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Causal Inference Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = causal_data)
#p-value = 0.01214

#"intro_rating(included and higher than median causality rating)
#>
#  intro_rating(included and not higher than median causality rating)"

#median causality rating

median_causality_rating <- median(causal_data$choice[causal_data$factor == "Factor-Excluded"])

causal_data <- causal_data %>%
  mutate(effect_group = case_when(
    choice > median_causality_rating & factor == "Factor-Included" ~ "Included and Higher Causality Than Median",
    choice <= median_causality_rating & factor == "Factor-Included" ~ "Included and Not Higher Causality Than Median",
    choice > median_causality_rating & factor == "Factor-Excluded" ~ "Excluded and Higher Causality Than Median",
    choice <= median_causality_rating & factor == "Factor-Excluded" ~ "Excluded and Not Higher Causality Than Median"
  ))%>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Higher Causality Than Median", "Included and Not Higher Causality Than Median", "Excluded and Higher Causality Than Median", "Excluded and Not Higher Causality Than Median")))

summary_causal_data <- causal_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_causal_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Causal Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()  +
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = causal_data))

    ## New version ----

median_causality_rating <- median(causal_data$choice[causal_data$factor == "Factor-Excluded"])

causal_data_by_subject <- data %>%
  filter(task_name == "causal inference") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    choice = as.numeric(choice),
    factor = recode(factor(first(factor), levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    affected = if_else(choice > median_causality_rating, 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
  )%>%
  filter(!is.na(introspect_rating))

View(causal_data_by_subject)

summary_causal <- causal_data_by_subject %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_causal)



ggplot(summary_causal, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Causal Inference Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE)+
  scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = causal_data))

## brm ----

causal_data_by_subject <- data %>%
  filter(task_name == "causal inference") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  rename(estimate_of_effect_size = introspect_rating) %>%
  mutate(choice = as.numeric(choice))
  
# Fit the Bayesian mixed-effects model
model <- brm(
  formula = estimate_of_effect_size ~ choice + (1 + factor | subject),
  data = causal_data_by_subject,
  family = gaussian(),  # Assuming estimate_of_effect_size is continuous
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 5), class = "b"),  # Prior on fixed effects
    prior(exponential(1), class = "sd")  # Prior on random effects SDs
  ),
  iter = 4000,
  chains = 4,
  cores = 4,
  seed = 123
)

# Summarize the model
summary(model)


# Extract posterior samples for the fixed effect of `distance_from_anchor`
posterior_samples <- posterior_samples(model)

colnames(posterior_samples)

ggplot(posterior_samples, aes(x = b_choice)) +
  geom_density(fill = NA, alpha = 0.5) +  # Set fill to NA for no fill
  labs(
    title = str_wrap("Posterior Distribution: Introspection on Causal Effect", width = 30),
    x = str_wrap("Effect of Causality Report on Estimate of Effect Size", width = 30),
    y = "Density"
  ) +
  theme_custom() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

#6. contact principle✅  ----
    ## 6.1 do we see the effect?----

#Did subjects for whom Frank needed to make physical contact 
#judge his action as less acceptable?

contact_data = data %>% 
  filter(task_name == 'contact principle') %>%
  mutate(choice_binary = as.numeric(choice == "Permissible"))

summary_contact_data <- contact_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Contact", "No Contact"))) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary)
  )

ggplot(summary_contact_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Contact Effect", x = "Condition", y = "Permissibility") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


contact_choices_ex <- contact_data %>%
  filter(factor == "Factor-Excluded") %>%
  pull(choice_binary)

contact_choices_in <- contact_data %>%
  filter(factor == "Factor-Included") %>%
  pull(choice_binary)

#analysis -- is there a better way to do this?
prop_ex <- sum(contact_choices_ex) / length(contact_choices_ex)
prop_in <- sum(contact_choices_in) / length(contact_choices_in)

successes <- c(sum(contact_choices_ex), sum(contact_choices_in))
trials <- c(length(contact_choices_ex), length(contact_choices_in))

test_result <- prop.test(successes, trials, alternative = "less")
print(test_result)

    ## 6.2 introspection ----

#Did factor included give lower introspection numbers 
#than factor excluded?

summary_contact_data <- contact_data %>%
  group_by(factor) %>%
  mutate(introspect_rating = as.numeric(introspect_rating)) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_contact_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Contact Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = contact_data)

#intro_rating(included and acceptable)
#<
#intro_rating(included and unacceptable)

contact_data <- contact_data %>%
  mutate(effect_group = case_when(
    choice == "Permissible" & factor == "Factor-Included" ~ "Included and Permissible",
    choice == "Impermissible" & factor == "Factor-Included" ~ "Included and Impermissible",
    choice == "Permissible" & factor == "Factor-Excluded" ~ "Excluded and Permissible",
    choice == "Impermissible" & factor == "Factor-Excluded" ~ "Excluded and Impermissible"
      ))%>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Impermissible", "Included and Permissible", "Excluded and Impermissible", "Excluded and Permissible")))


summary_contact_data <- contact_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_contact_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Contact Introspection Ratings", x = "Condition", y = "introspection rating") +
  theme_custom()  +
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = contact_data))

    ## New version ----


contact_data_by_subject <- data %>%
  filter(task_name == "contact principle") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    choice = choice,
    factor = recode(factor(first(factor), levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    affected = if_else(choice == "Impermissible", 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
  )%>%
  filter(!is.na(introspect_rating))

View(contact_data_by_subject)

summary_contact <- contact_data_by_subject %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_causal)

ggplot(summary_contact, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Contact Principle Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE)+
  scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = contact_data))

#7. decoy effect ✅  ----
    ## 7.1 do we see the effect? TO DO----

#Were subjects who saw Brand W more likely to choose brand N?
decoy_data <- data %>%
  filter(task_name == "decoy effect") %>%
  mutate(choice_binary = as.numeric(choice == "Brand N (Target)"))%>%
  mutate(condition = factor(condition, levels = c("Decoy Present", "Decoy Absent")))
  

summary_decoy_data <- decoy_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary)
  )

ggplot(summary_decoy_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Decoy Effect", x = "Condition", y = "Choosing Brand N") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


decoy_choices_ex <- decoy_data %>%
  filter(factor == "Factor-Excluded") %>%
  pull(choice_binary)

decoy_choices_in <- decoy_data %>%
  filter(factor == "Factor-Included") %>%
  pull(choice_binary)

#analysis -- is there a better way to do this?
prop_ex <- sum(decoy_choices_ex) / length(decoy_choices_ex)
prop_in <- sum(decoy_choices_in) / length(decoy_choices_in)

successes <- c(sum(decoy_choices_ex), sum(decoy_choices_in))
trials <- c(length(decoy_choices_ex), length(decoy_choices_in))

test_result <- prop.test(successes, trials, alternative = "less")
print(test_result)

    ## 7.2 introspection ----

#Did factor included give higher introspection numbers 
#factor excluded?

summary_decoy_data <- decoy_data %>%
  group_by(factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = 100-mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_decoy_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Decoy Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


#"intro_rating(included and chose brand N)
#>
#intro_rating(included and did not choose brand N)"

decoy_data <- decoy_data %>%
  mutate(effect_group = case_when(
    choice == "Brand N (Target)" & factor == "Factor-Included" ~ "Included and Chose Brand N",
    choice != "Brand N (Target)" & factor == "Factor-Included" ~ "Included and Did Not Choose Brand N",
    choice == "Brand N (Target)" & factor == "Factor-Excluded" ~ "Excluded and Chose Brand N",
    choice != "Brand N (Target)" & factor == "Factor-Excluded" ~ "Excluded and Did Not Choose Brand N"
      ))%>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Chose Brand N", "Included and Did Not Choose Brand N", "Excluded and Chose Brand N", "Excluded and Did Not Choose Brand N")))


summary_decoy_data <- decoy_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_decoy_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Decoy Introspection Ratings", x = "Condition", y = "introspection rating") +
  theme_custom() +
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = decoy_data))

    ## New version ----


decoy_data_by_subject <- data %>%
  filter(task_name == "decoy effect") %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    choice = choice,
    factor = recode(factor(first(factor), levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    affected = if_else(choice == "Brand N (Target)", 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
  )%>%
  filter(!is.na(introspect_rating))

View(decoy_data_by_subject)

summary_decoy <- decoy_data_by_subject %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_causal)

ggplot(summary_decoy, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Decoy Effect Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE)+
  scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = decoy_data))


#8. Double effect ✅  ----
    ##8.1 do we see the effect? ----

#Were subjects who saw Peter's killing as means to an end more likely 
#to judge it impermissible than those who saw it as a side-effect?

#double_effect_data <- data %>%
#  filter(task_name == "double effect") %>%
#  mutate(choice = as.numeric(choice))

#temporary until we fix why choice is coming up Null 
double_effect_data <- read_csv("double.csv") %>%
  filter(familiarity != "Yes") %>% #only when people are not familiar with a task
  mutate(
    task_name = factor(task_name)  # Convert task_name to a factor
  ) %>%
  mutate(choice_binary = as.numeric(choice == "Permissible"))

summary_double_effect_data <- double_effect_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Means", "Side Effect"))) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary)
  )

ggplot(summary_double_effect_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Double Effect", x = "Condition", y = "Permissibility") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


double_choices_ex <- double_effect_data %>%
  filter(factor == "Factor-Excluded") %>%
  pull(choice_binary)

double_choices_in <- double_effect_data %>%
  filter(factor == "Factor-Included") %>%
  pull(choice_binary)

#analysis -- is there a better way to do this?
prop_ex <- sum(double_choices_ex) / length(double_choices_ex)
prop_in <- sum(double_choices_in) / length(double_choices_in)

successes <- c(sum(double_choices_ex), sum(double_choices_in))
trials <- c(length(double_choices_ex), length(double_choices_in))

test_result <- prop.test(successes, trials, alternative = "less")
print(test_result)

    ##8.2 introspection----

#Did factor included give higher introspection numbers 
#than factor excluded?

summary_double_effect_data <- double_effect_data %>%
  group_by(factor) %>%
  mutate(introspect_rating = as.numeric(introspect_rating)) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

str(summary_double_effect_data)


ggplot(summary_double_effect_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Double Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = double_effect_data)

#"intro_rating(included and permissible)
#>
#intro_rating(included and not permissible)"

double_effect_data <- double_effect_data %>%
  mutate(effect_group = case_when(
    choice == "Permissible" & factor == "Factor-Included" ~ "Included and Permissible",
    choice == "Impermissible" & factor == "Factor-Included" ~ "Included and Impermissible",
    choice == "Permissible" & factor == "Factor-Excluded" ~ "Excluded and Permissible",
    choice == "Impermissible" & factor == "Factor-Excluded" ~ "Excluded and Impermissible"
      ))%>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Impermissible", "Included and Permissible", "Excluded and Impermissible", "Excluded and Permissible")))

summary_double_effect_data <- double_effect_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_double_effect_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Double Introspection Ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = double_effect_data))

    ## New version ----


double_data_by_subject <- double_effect_data %>%
  group_by(subject) %>%
  filter(any(!is.na(introspect_rating))) %>%
  summarize(
    choice = choice,
    factor = recode(factor(first(factor), levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    affected = if_else(choice == "Impermissible", 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
  )%>%
  filter(!is.na(introspect_rating))

View(double_data_by_subject)

summary_double <- double_data_by_subject %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_causal)

ggplot(summary_double, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Double Effect Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE)+
  scale_y_continuous(limits = c(0, 100))


summary(lm(introspect_rating ~ effect_group, data = double_data))


#9. Halo Effect  ✅ ----
    ##9.1 do we see the effect? ----

#Did subjects who were shown some attractive and 
#some attractive faces think the attractive were more persuasive, with the subjects who only saw neutral faces calling in the middle?

halo_bar_data <- data %>%
  filter(task_name == "halo") %>%
  mutate(choice = as.numeric(choice))%>%
  filter(stimulus != "") %>%
  mutate(
    condition = case_when(
      grepl("img/U", stimulus) ~ "unattractive",
      grepl("img/A", stimulus) ~ "attractive",
      grepl("img/M", stimulus) ~ "average",
      TRUE ~ condition
    )
  )



summary_halo_data <- halo_bar_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice)
  )

ggplot(summary_halo_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Average Persuasiveness by Attractiveness", x = "Condition", y = "Average Choice") +
  theme_custom()+
  scale_fill_manual(values = in_neutral_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))



summary(lm(choice ~ condition, data = halo_bar_data))


    ##9.2 introspection ----

#Did factor included give higher introspection 
#numbers than factor excluded?

summary_halo_data <- data %>%
  filter(task_name == "halo", stimulus == "") %>% 
  group_by(factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_halo_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Halo Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = halo_data)



#next question:"intro_rating(included and gave median attractive faces persuasive score higher than median unattractive face)
#>
#  intro_rating(included and gave median attractive faces persuasive score not higher than median unattractive face)"

halo_data <- data %>%
  filter(task_name == "halo") %>%
  filter(familiarity == "") %>%
  mutate(choice = as.numeric(choice)) %>%
  mutate(
    condition = case_when(
      grepl("img/U", stimulus) ~ "unattractive",
      grepl("img/A", stimulus) ~ "attractive",
      grepl("img/M", stimulus) ~ "average",
      TRUE ~ condition
    )
  )


# Find subjects who gave higher median scores to attractive than unattractive
subject_list_included_and_affected <- halo_data %>%
  # Step 1: Calculate median choice for each subject under "attractive" condition
  filter(condition == "attractive") %>%
  group_by(subject) %>%
  summarize(median_choice_attractive = median(choice, na.rm = TRUE)) %>%
  
  # Step 2: Join with median choice for "unattractive" condition
  inner_join(
    halo_data %>%
      filter(condition == "unattractive") %>%
      group_by(subject) %>%
      summarize(median_choice_unattractive = median(choice, na.rm = TRUE)),
    by = "subject"
  ) %>%
  
  # Step 3: Filter subjects where median choice for "attractive" is higher than "unattractive"
  filter(median_choice_attractive > median_choice_unattractive) %>%
  
  # Pull the subject list
  pull(subject)

View(subject_list_included_and_affected)

# List of all subjects in "Factor-Included"
all_subjects_included = halo_data %>%
  filter(factor == "Factor-Included") %>%
  pull(subject)

#View(all_subjects_included)

subject_list_included_and_not_higher_than_median = setdiff(all_subjects_included, subject_list_included_and_affected)

#View(subject_list_included_and_not_higher_than_median)

halo_data <- data %>%
  filter(task_name == "halo") %>%
  filter(familiarity == "") %>%
  
  mutate(effect_group = case_when(
    factor == "Factor-Excluded" ~ "Excluded",
    subject %in% subject_list_included_and_higher_than_median ~ "Included and Higher Than Median",
    TRUE ~ "Included and Not Higher Than Median",
    
    )) %>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Higher Than Median", "Included and Not Higher Than Median", "Excluded")))


#View(halo_data)

halo_data <- data %>%
  filter(task_name == "halo") %>%
  filter(familiarity != "") %>%
  mutate(effect_group = case_when(
    factor == "Factor-Excluded" ~ "Excluded",
    subject %in% subject_list_included_and_higher_than_median ~ "Included and Shows Effect",
    TRUE ~ "Included and Doesn't Show Effect",
    
  )) %>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Shows Effect", "Included and Doesn't Show Effect", "Excluded")))


summary_halo_data <- halo_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

#View(summary_halo_data)


ggplot(summary_halo_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Halo Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()  +
  scale_fill_manual(values = three_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = halo_data))


    ## New version ----

halo_introspection <- data %>%
  filter(task_name == "halo") %>%
  filter(introspect_rating != "") %>%
  mutate(
    condition = case_when(
      grepl("img/U", stimulus) ~ "unattractive",
      grepl("img/A", stimulus) ~ "attractive",
      grepl("img/M", stimulus) ~ "average",
      TRUE ~ condition
    )
  )


halo_data <- data %>%
  filter(task_name == "halo") %>%
  filter(familiarity == "") %>%
  mutate(choice = as.numeric(choice)) %>%
  mutate(
    condition = case_when(
      grepl("img/U", stimulus) ~ "unattractive",
      grepl("img/A", stimulus) ~ "attractive",
      grepl("img/M", stimulus) ~ "average",
      TRUE ~ condition
    )
  )


halo_data_included <- halo_data %>%
  # Step 1: Calculate median choice for each subject under "attractive" condition
  filter(condition == "attractive") %>%
  group_by(subject) %>%
  summarize(median_choice_attractive = median(choice, na.rm = TRUE)) %>%
  
  # Step 2: Join with median choice for "unattractive" condition
  inner_join(
    halo_data %>%
      filter(condition == "unattractive") %>%
      group_by(subject) %>%
      summarize(median_choice_unattractive = median(choice, na.rm = TRUE)),
    by = "subject"
  )%>%
  left_join(halo_introspection %>% select(subject, introspect_rating), by = "subject") %>%
  
  summarize(factor = "Factor Included",
            affected = if_else(median_choice_attractive > median_choice_unattractive, 
                               "Affected by Bias", 
                               "Not Affected by Bias"),
            introspect_rating=introspect_rating,
            
            )%>%
              filter(!is.na(introspect_rating))

View(halo_data_included)

halo_data_excluded = halo_data %>%
  filter(factor == "Factor-Excluded") %>%
  group_by(subject) %>%
  summarize(median_choice = median(choice, na.rm = TRUE))  %>%
  left_join(halo_introspection %>% select(subject, introspect_rating), by = "subject") %>%
  summarize(factor = "Factor Excluded",
            affected = "Not Affected by Bias",
            introspect_rating=introspect_rating,
            
            )%>%
              filter(!is.na(introspect_rating))

View(halo_data_excluded)


halo_combined <- rbind(halo_data_included, halo_data_excluded) %>%
  mutate(factor = factor(factor, levels = c("Factor Included", "Factor Excluded"))) 
  
View(halo_combined)


summary_halo <- halo_combined %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

#View(summary_anchoring)

ggplot(summary_halo, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Halo Effect Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 100))




summary(lm(introspect_rating ~ effect_group, data = halo_data))

#10 hindsight bias ----
    ##10.1 do we see the effect? TO DO----

#Did subjects who saw the correct answers misremember their previous answers as closer to the correct answer than the subjects who did not see the correct answers?

# Function to clean and convert values
convert_to_numeric <- function(values) {
  # Remove all commas
  values <- gsub(",", "", values)
  
  # Handle ' million', ' Million', 'M'
  millions <- grepl(" million| milliion| millon| mil| Million|M", values)
  values[millions] <- as.numeric(gsub(" million| milliion| millon| mil| Million|M", "", values[millions])) * 1e6
  
  # Handle 'k'
  thousands <- grepl("k", values, ignore.case = TRUE)
  values[thousands] <- as.numeric(gsub("k|K", "", values[thousands])) * 1e3
  
  # Attempt to convert the rest directly to numeric
  values[!millions & !thousands] <- suppressWarnings(as.numeric(values[!millions & !thousands]))
  
  return(values)
}




true_values <- c(
  Pakistan = 203177034,
  Nigeria = 199045324,
  Mexico = 131738729,
  Vietnam = 97074662,
  The_Democratic_Republic_of_the_Congo = 85705256,
  Thailand = 69256846,
  Tanzania = 60229204,
  South_Korea = 51273440,
  Colombia = 49705306,
  Uganda = 45169147,
  Ukraine = 43877093,
  Malaysia = 32294009,
  North_Korea = 25683863,
  Niger = 22850032,
  Burkina_Faso = 20106983,
  Romania = 19519762,
  Zimbabwe = 17154637,
  The_Netherlands = 17114912,
  Somalia = 14600000,
  Guinea = 13270289,
  Benin = 11683042,
  Haiti = 11193952,
  Greece = 11133944,
  The_Czech_Republic = 10629078,
  Azerbaijan = 9980369
)

View(hindsight_data)

hindsight_data <- data %>%
  filter(task_name == "hindsight bias") %>%
  filter(!(subject %in% c("66749b876f32a4ad246db5da","5f2d95153c1140074cc81b4c","667469ea0d42f70a9a75567b","667444e5662b7a4ebf82d5e1","665c6f67d9ea69740afbcab8","6273238a4a8b39041ff1bd2c", "664d3c950d24d83ca7b4dd68", "6020606d7b0258677b881f63", "6159fe7811a7e1b94401c33f"))) %>%
  mutate(parsed_choice = convert_to_numeric(choice)) %>%
  filter(parsed_choice != "NA") %>%
  mutate(which_estimate = case_when(
    str_detect(auxiliary_info1, "_first_response") ~ "first",
    str_detect(auxiliary_info1, "_recall_original_response") ~ "recall",
    TRUE ~ NA_character_
  )) %>%



  # Extract the country name
  mutate(country = case_when(
    str_detect(auxiliary_info1, "_estimate") ~ str_extract(auxiliary_info1, "^[^_]+(?:_[^_]+)*(?=_estimate)"),
    str_detect(auxiliary_info1, "_recall") ~ str_extract(auxiliary_info1, "^[^_]+(?:_[^_]+)*(?=_recall)"),
    TRUE ~ NA_character_
  ))%>%

  mutate(true_value = true_values[country])%>%
  mutate(difference_from_true = as.numeric(true_value) - as.numeric(parsed_choice))

View(hindsight_data)

medians_hindsight <- hindsight_data %>%
  group_by(subject) %>%
  summarise(
    median_first_estimate = abs(median(difference_from_true[which_estimate == "first"], na.rm = TRUE)),
    median_second_estimate = abs(median(difference_from_true[which_estimate == "recall"], na.rm = TRUE)),
    factor = first(factor),
    introspect_rating = first(introspect_rating)
  ) %>%
  filter(!is.na(median_first_estimate) & !is.na(median_second_estimate))

View(medians_hindsight)

custom_labels <- c("median_first_estimate" = "Median First Estimate",
                   "median_second_estimate" = "Median Second Estimate")


medians_summary <- medians_hindsight %>%
  pivot_longer(cols = c(median_first_estimate, median_second_estimate),
               names_to = "estimate_type",
               values_to = "estimate_value") %>%
  group_by(estimate_type, factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  
  summarise(
    mean_choice = mean(estimate_value),
    se_choice = se(estimate_value),
    n_choice = n()
  )

# Create the ggplot with n labels for each bar
ggplot(medians_summary, aes(x = estimate_type, y = mean_choice, fill = factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), 
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n=", n_choice)), 
            position = position_dodge(0.9), vjust = -0.5) +
  facet_wrap(~factor) +
  labs(title = "Hindsight Effect",
       x = "Estimate Type",
       y = "Mean Estimate Distance From True Value") +
  theme_custom() + 
  scale_fill_manual(values = in_and_ex) +
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "_", " "), width = 10))+
  guides(fill = FALSE)



    ##10.2 introspection----

#Did factor included give lower introspection numbers than factor excluded?

hindsight_data <- data %>%
  filter(task_name == "hindsight") %>%
  filter(stimulus == "") 

View(hindsight_data)
View(summary_hindsight_data)

summary_hindsight_data <- hindsight_data %>%
  group_by(factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_hindsight_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Hindsight Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = mere_exposure_data)

##"intro_rating(included and average distance between the answer and the true number is lower after than before)
#<
#intro_rating(included and average distance between the answer and the true number is not lower after than before)"

hindsight_introspection <- data %>%
  filter(task_name == "hindsight")
#this was because of a bug where all the introspect ratings were saved under hindsight instead of hindsight bias

medians_hindsight <- hindsight_data %>%
  group_by(subject) %>%
  summarise(
    median_first_estimate = abs(median(difference_from_true[which_estimate == "first"], na.rm = TRUE)),
    median_second_estimate = abs(median(difference_from_true[which_estimate == "recall"], na.rm = TRUE)),
    factor = first(factor),
    introspect_rating = first(introspect_rating)
  ) %>%
  filter(!is.na(median_first_estimate) & !is.na(median_second_estimate))


View(medians_hindsight)

summary_hindsight <- medians_hindsight %>%
  mutate(affected = if_else(median_first_estimate > median_second_estimate, 
                            "Affected by Bias", 
                            "Not Affected by Bias"))%>%
  left_join(hindsight_introspection %>% select(subject, introspect_rating), by = "subject") %>%
  select(factor, affected, introspect_rating.y) %>%
  group_by(factor, affected) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  mutate(factor = recode(factor, 
                         `Factor-Included` = "Factor Included", 
                         `Factor-Excluded` = "Factor Excluded")) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating.y),
    se_introspect_rating = se(introspect_rating.y),
    n = n()
  )

ggplot(summary_hindsight, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Hindsight Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 100))




#11 mere exposure ✅ ----
    ##11.1 do we see the effect? ----

#Were the most seen words the most liked?

mere_exposure_data <- data %>%
  filter(task_name == "mere exposure") %>%
  filter(stimulus != "") %>%
  mutate(choice = as.numeric(choice))%>%
  mutate(condition = factor(condition, levels = c("25", "13","1")))
    

summary_mere_exposure_data <- mere_exposure_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice)
  )

ggplot(summary_mere_exposure_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Mere Exposure effect", x = "Times seen the word", y = "Average rating of word") +
  theme_custom()+
  scale_fill_manual(values = in_neutral_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


summary(lm(choice ~ condition, data = mere_exposure_data))


    ##11.2 introspection----

#Did factor included give higher introspection numbers 
#than factor excluded?

mere_exposure_data <- data %>%
  filter(task_name == "mere exposure") %>%
  filter(stimulus == "") %>%
  mutate(choice = as.numeric(choice))

summary_mere_exposure_data <- mere_exposure_data %>%
  group_by(factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_mere_exposure_data, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Mere Exposure Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = mere_exposure_data)

#next question: "intro_rating(included and their median rating of how much they liked the words was greater than the median rating for excluded)
#>
#intro_rating(included and not their median rating of how much they liked the words was greater than the median rating for excluded)"


mere_exposure_data <- data %>%
  filter(task_name == "mere exposure") %>%
  filter(stimulus != "") %>%
  mutate(choice = as.numeric(choice))

#median rating among excluded
median_rating_excluded = median(mere_exposure_data$choice[mere_exposure_data$factor == "Factor-Excluded"])

#I do not know why but these two subjects never gave introspection ratings
filtered_data <- data %>%
  filter(!(subject %in% c("58635484a73baa00010db537", "65e29e4514fa80bea57284b7")))

#make new table with subject number, average rating, and introspection rating
subject_data <- filtered_data %>%
  group_by(subject) %>%
  filter(task_name == "mere exposure") %>%
  mutate(choice = as.numeric(choice)) %>%
  summarise(
    average_choice_rating_per_subject = median(choice, na.rm = TRUE),
    introspect_rating = first(introspect_rating[!is.na(introspect_rating)]),
    factor = first(factor)
  )


#make new column for effect group
subject_data <- subject_data %>%
  mutate(effect_group = case_when(
    average_choice_rating_per_subject > median_rating_excluded & factor == "Factor-Included" ~ "Included and Rating Higher Than Excluded Median",
    average_choice_rating_per_subject <= median_rating_excluded & factor == "Factor-Included" ~ "Included and Rating Not Higher Than Excluded Median",
    average_choice_rating_per_subject > median_rating_excluded & factor == "Factor-Excluded" ~ "Excluded and Rating Higher Than Excluded Median",
    average_choice_rating_per_subject <= median_rating_excluded & factor == "Factor-Excluded" ~ "Excluded and Rating Not Higher Than Excluded Median",
  )) %>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Rating Higher Than Excluded Median", "Included and Rating Not Higher Than Excluded Median", "Excluded and Rating Higher Than Excluded Median", "Excluded and Rating Not Higher Than Excluded Median", "Excluded")))

summary_mere_exposure_data <- subject_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_mere_exposure_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Mere Exposure Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom() +
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

    ## New version ----


mere_exposure_data <- data %>%
  filter(task_name == "mere exposure") %>%
  filter(stimulus != "") %>%
  mutate(choice = as.numeric(choice))

#median rating among excluded
median_rating_excluded = median(mere_exposure_data$choice[mere_exposure_data$factor == "Factor-Excluded"])

#I do not know why but these two subjects never gave introspection ratings
filtered_data <- data %>%
  filter(!(subject %in% c("58635484a73baa00010db537", "65e29e4514fa80bea57284b7")))

#make new table with subject number, average rating, and introspection rating
mere_exposure_subject_data <- filtered_data %>%
  group_by(subject) %>%
  filter(task_name == "mere exposure") %>%
  mutate(choice = as.numeric(choice)) %>%
  summarise(
    average_choice_rating_per_subject = median(choice, na.rm = TRUE),
    introspect_rating = first(introspect_rating[!is.na(introspect_rating)]),
    affected = if_else(average_choice_rating_per_subject > median_rating_excluded,
                       "Affected by Bias",
                       "Not Affected by Bias"),
    factor = recode(factor(first(factor), levels = c("Factor-Included", "Factor-Excluded")),
                    `Factor-Included` = "Factor Included",
                    `Factor-Excluded` = "Factor Excluded"),
  )


summary_mere_exposure <- mere_exposure_subject_data %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )



ggplot(summary_mere_exposure, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Mere Exposure Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = representativeness_data))




#12 reference price ✅ ----
    ##12.1 do we see the effect? ----

#When subjects were told the hotel was fancy, were 
#they more likely to give a higher price they'd be willing to pay?

reference_price_data <- data %>%
  filter(task_name == "reference price") %>%
  mutate(choice_parsed = parse_number(choice))

##View(reference_price_data)

summary_reference_price_data <- reference_price_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("hotel", "motel"))) %>%
  summarize(
    mean_choice = mean(choice_parsed),
    se_choice = se(choice_parsed)
  )

ggplot(summary_reference_price_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Amount Willing to Pay for Beer", x = "Condition", y = "Average Amount Willing to Pay (Dollars)") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


t.test(choice_parsed ~ factor, data = reference_price_data)
# p-value = 0.4913


    ##12.2 introspection----

#Did factor included give lower introspection numbers than 
#factor excluded?

summary_reference_price_data <- reference_price_data %>%
  group_by(condition) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_reference_price_data, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Reference Price Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = reference_price_data)
#p-value = 0.8909

#"intro_rating(included and higher than median price)
#<
#  intro_rating(included and not higher than median price)
#"

#median price among excluded
median_price_among_excluded <- median(reference_price_data$choice_parsed[reference_price_data$factor == "Factor-Excluded"])

reference_price_data <- reference_price_data %>%
  mutate(effect_group = case_when(
    choice_parsed > median_price_among_excluded & factor == "Factor-Included" ~ "Included and Higher Than Median",
    choice_parsed <= median_price_among_excluded & factor == "Factor-Included" ~ "Included and Not Higher Than Median",
    choice_parsed > median_price_among_excluded & factor == "Factor-Excluded" ~ "Excluded and Higher Than Median",
    choice_parsed <= median_price_among_excluded & factor == "Factor-Excluded" ~ "Excluded and Not Higher Than Median",
    factor == "Factor-Excluded" ~ "Excluded"
  ))%>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Higher Than Median", "Included and Not Higher Than Median",  "Excluded and Higher Than Median", "Excluded and Not Higher Than Median")))




summary_reference_price_data <- reference_price_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_reference_price_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Reference Price Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom() +
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = reference_price_data))

    ## New version ----

median_price_among_excluded <- median(reference_price_data$choice_parsed[reference_price_data$factor == "Factor-Excluded"])

View(reference_data)

reference_data <- data %>%
  group_by(subject) %>%
  filter(task_name == "reference price") %>%
  summarise(
    choice_parsed = parse_number(choice),
    affected = if_else(choice_parsed > median_price_among_excluded, 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
    factor = recode(factor(factor, levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded")) 
  ) 


summary_reference <- reference_data %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )


ggplot(summary_reference, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Reference Price Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = reference_data))


#13 representativeness  ✅ ----
    ##13.1 do we see the effect ----

#When subjects were given the description about Jack, 
#did more of them say he was an engineer?

representativeness_data <- data %>%
  filter(task_name == "rep") %>%
  mutate(choice = as.numeric(choice))

summary_representativeness_data <- representativeness_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice)
  )

ggplot(summary_representativeness_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Is Jack an Engineer?", x = "Condition", y = "average likelihood of engineer") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


t.test(choice ~ factor, data = representativeness_data)
# p-value = 7.142e-06

    ##13.2 introspection----

#Did factor included give higher introspection 
#numbers than factor excluded?

summary_representativeness_data <- representativeness_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_representativeness_data, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Representativeness Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = representativeness_data)
#p-value = 0.2962

#next question: "intro_rating(included and said Jack was more likely to be engineer than median of excluded)
#>
#  intro_rating(included and not said Jack was more likely to be engineer than median of excluded"

#median likelihood of engineer among excluded
median_likelihood_of_engineer <- median(representativeness_data$choice[representativeness_data$factor == "Factor-Excluded"])
print(median_likelihood_of_engineer)

representativeness_data <- representativeness_data %>%
  mutate(effect_group = case_when(
    choice > median_likelihood_of_engineer & factor == "Factor-Included" ~ "Included and Said Engineer",
    choice <= median_likelihood_of_engineer & factor == "Factor-Included" ~ "Included and Not Said Engineer",
    choice > median_likelihood_of_engineer & factor == "Factor-Excluded" ~ "Excluded and Said Engineer",
    choice <= median_likelihood_of_engineer & factor == "Factor-Excluded" ~ "Excluded and Not Said Engineer"
  )) %>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Said Engineer", "Included and Not Said Engineer",  "Excluded and Said Engineer", "Excluded and Not Said Engineer")))



summary_representativeness_data <- representativeness_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary_representativeness_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Representativeness Introspection Ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = four_colors) +
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))

    ## New version ----

median_likelihood_of_engineer <- median(representativeness_data$choice[representativeness_data$factor == "Factor-Excluded"])


View(representatative_data)

representatative_data <- data %>%
  group_by(subject) %>%
  filter(task_name == "rep") %>%
  summarise(
    affected = if_else(choice > median_likelihood_of_engineer, 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
    factor = recode(factor(factor, levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded")) 
  ) 


summary_representatative <- representatative_data %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )



ggplot(summary_representatative, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Representativeness Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 100))



summary(lm(introspect_rating ~ effect_group, data = representativeness_data))



#14 status quo ✅ ----

    ##14.1 do we see the effect ----

#When subjects were told the status quo, 
#were they more likely to recommend the 70/30 allocation?

status_quo_data <- data %>%
  filter(task_name == "status_quo") %>%
  mutate(choice_binary = as.numeric(choice == "70/30"))%>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) 
  


summary_status_quo_data <- status_quo_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary)
  )

ggplot(summary_status_quo_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Choices to continue the status quo", x = "Condition", y = "Percent subjects who recommended the status quo") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))



status_quo_choices_ex <- status_quo_data %>%
  filter(factor == "Factor-Excluded") %>%
  pull(choice_binary)

status_quo_choices_in <- status_quo_data %>%
  filter(factor == "Factor-Included") %>%
  pull(choice_binary)

#analysis -- is there a better way to do this?
prop_ex <- sum(status_quo_choices_ex) / length(status_quo_choices_ex)
prop_in <- sum(status_quo_choices_in) / length(status_quo_choices_in)

successes <- c(sum(status_quo_choices_ex), sum(status_quo_choices_in))
trials <- c(length(status_quo_choices_ex), length(status_quo_choices_in))

test_result <- prop.test(successes, trials, alternative = "less")
print(test_result)

#p-value = 0.3625
  
    ##14.2 introspection----

#Did factor included give lower introspection 
#numbers than factor excluded?

summary_status_quo_data <- status_quo_data %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  group_by(condition) %>%
  summarize(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

View(summary_status_quo_data)

ggplot(summary_status_quo_data, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Status Quo Introspection Ratings", x = "Condition", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))


t.test(introspect_rating ~ factor, data = status_quo_data)
#p-value = 0.3647

#next question: "intro_rating(included and said 70/30)
#<
#  intro_rating(included and said 50/50)"

status_quo_data <- data %>%
  group_by(subject) %>%
  filter(task_name == "status_quo") %>%
  summarise(
    affected = if_else(choice == "70/30", 
                       "Affected by Bias", 
                       "Not Affected by Bias"),
    factor = recode(factor(factor, levels = c("Factor-Included", "Factor-Excluded")), 
                    `Factor-Included` = "Factor Included", 
                    `Factor-Excluded` = "Factor Excluded"),
    introspect_rating = introspect_rating,
    condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded")) 
  ) 

View(summary_status_quo)

summary_status_quo <- status_quo_data %>%
  group_by(factor, affected) %>%
  summarise(
    mean_introspect_rating = 100-mean(introspect_rating),
    se_introspect_rating = se(introspect_rating),
    n = n()
  )

ggplot(summary_status_quo, aes(x = factor, y = mean_introspect_rating, fill = affected, group = affected)) + 
  theme_custom() +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste("n =", n), x = stage(factor, after_stat = x - .9 / 2 / 2), y = mean_introspect_rating), 
            hjust = 0, vjust=-0.35, family = "optima", size = 4, position = position_dodge(0.9)) +
  geom_text(aes(label = str_wrap(affected, width = 10), y = 15, color = affected), 
            position = position_dodge(0.9), vjust = 0, family = "optima", size = 5, lineheight = 0.8) +
  labs(title = "Status Quo Bias Introspection", x = "", y = "Introspection Rating") +
  scale_fill_manual(values = effect_no) +
  scale_color_manual(values = c("Affected by Bias" = "white", "Not Affected by Bias" = "black")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 100))

# all tasks

# Calculate introspection ratings for all tasks
summary_all_tasks <- data %>%
  filter(introspect_rating != "") %>%
  group_by(task_name, factor) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_introspect_rating = 100 - mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  ) %>%
  ungroup()

# Now summarize across tasks
summary_overall <- summary_all_tasks %>%
  group_by(factor) %>%
  summarize(
    overall_mean_introspect_rating = mean(mean_introspect_rating),
    overall_se_introspect_rating = se(se_introspect_rating))

# Plotting the overall averages
ggplot(summary_overall, aes(x = factor, y = overall_mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = overall_mean_introspect_rating - overall_se_introspect_rating, 
        ymax = overall_mean_introspect_rating + overall_se_introspect_rating), 
    width = 0.2
  ) +
  labs(
    title = "Overall Introspection Ratings Across Tasks",
    x = "Condition",
    y = "Introspection Rating"
  ) +
  theme_custom() +
  scale_fill_manual(values = in_and_ex) +
  guides(fill = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  scale_y_continuous(limits = c(0, 100))

# Knittr ----

if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
library(knitr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

knitr::opts_chunk$set(dev = 'svg')

# Use the stitch function
knitr::spin("analysis2024.R")
