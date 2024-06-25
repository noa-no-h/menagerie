# Setup -------------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(sjPlot)
require(magrittr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv('data.csv') %>%
  arrange(subject, task_name)
head(data)

data = data %>%
  filter(familiarity != "Yes") %>% #only when people are not familiar with a task
  mutate(
    task_name = factor(task_name)  # Convert task_name to a factor
  )

View(data)



# 1 Did we see the effects? -------------------------------------------------



## 1.1 anchoring effect? -----------------------------------------------------------------------


### Antarctica -----------------------------------------------------------------------


anchor_antarctica_data <- data %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus != "")%>%
  filter(stimulus == "Antarctic Temperature")
  
anchor_antarctica_data$choice <- as.numeric(anchor_antarctica_data$choice)


#violin
ggplot(anchor_antarctica_data, aes(x = condition, y = choice)) +
  geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.5) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  labs(title = "Distribution of Estimates by Anchor Presence", x = "Anchor Presence", y = "Estimate") +
  theme_minimal()

#bar
ggplot(summary_anchor_antarctica_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean Estimates by Anchor Presence", x = "Anchor Presence", y = "Mean Estimate") +
  theme_minimal()

#analysis

low_anchor <- anchor_antarctica_data %>%
  filter(condition == "Low Anchor") %>%
  pull(choice)

no_anchor <- anchor_antarctica_data %>%
  filter(condition == "No Anchor") %>%
  pull(choice)

t_test_result <- t.test(low_anchor, no_anchor, var.equal = TRUE)

print(t_test_result)
#p-value = 0.001


### Whale -----------------------------------------------------------------------
anchor_whale_data <- data %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus != "") %>%
  filter(stimulus == "Whale Length")
  
anchor_whale_data$choice <- as.numeric(anchor_whale_data$choice)

summary_anchor_whale_data <- anchor_whale_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice, na.rm = TRUE),
    se_choice = sd(choice, na.rm = TRUE) / sqrt(n())
  )


ggplot(anchor_whale_data, aes(x = condition, y = choice)) +
  geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.5) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  labs(title = "Distribution of Estimates by Anchor Presence", x = "Anchor Presence", y = "Estimate") +
  theme_minimal()

#analysis

low_anchor <- anchor_whale_data %>%
  filter(condition == "Low Anchor") %>%
  pull(choice)

no_anchor <- anchor_whale_data %>%
  filter(condition == "No Anchor") %>%
  pull(choice)

t_test_result <- t.test(low_anchor, no_anchor, var.equal = TRUE)

print(t_test_result)
#p-value = 0.8

## 1.2 associative memory effect? -----------------------------------------------------------------------

#plot
associative_data <- data %>%
  filter(task_name == "associative memory") %>%
  filter(stimulus != "") %>%
  mutate(false_alarm = ifelse(choice == "Original" & auxiliary_info1 == "New", 1, 0))

factor_ex_associative = associative_data %>%
  filter(factor == "Factor-Excluded")

factor_in_associative = associative_data %>%
  filter(factor == "Factor-Included") 

false_alarm_summary <- associative_data %>%
  group_by(factor) %>%
  summarize(count_false_alarm = sum(false_alarm))

ggplot(false_alarm_summary, aes(x = factor, y = count_false_alarm)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Count of False Alarms by Condition",
       x = "Condition",
       y = "Count of False Alarms") +
  theme_minimal()

#analysis

false_alarm_ex <- associative_data %>%
  filter(factor == "Factor-Excluded") %>%
  pull(false_alarm)

false_alarm_in <- associative_data %>%
  filter(factor == "Factor-Included") %>%
  pull(false_alarm)

prop_ex <- sum(false_alarm_ex) / length(false_alarm_ex)
prop_in <- sum(false_alarm_in) / length(false_alarm_in)

successes <- c(sum(false_alarm_ex), sum(false_alarm_in))
trials <- c(length(false_alarm_ex), length(false_alarm_in))

test_result <- prop.test(successes, trials, alternative = "less")

print(test_result)
#p-value = 2e-05

## 1.3 availability effect? -----------------------------------------------------------------------

availability_data = data %>%
  filter(task_name == "availability") %>%
  mutate(choice_binary = as.numeric(choice == "List 1"))

ggplot(availability_data, aes(x = factor, fill = choice)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Choice Between Excluded and Included Factors",
       x = "Factor",
       y = "Count",
       fill = "choice") +
  theme_minimal()

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




## 1.4 belief effect? -----------------------------------------------------------------------
## 1.5 causal inference? -----------------------------------------------------------------------
## 1.6 contact principle? -----------------------------------------------------------------------
## 1.7 decoy effect? -----------------------------------------------------------------------
## 1.8 double effect? -----------------------------------------------------------------------
## 1.9 halo effect? -----------------------------------------------------------------------

halo_bar_data <- data %>%
  filter(task_name == "halo") %>%
  filter(stimulus != "") %>%
  mutate(
    condition = case_when(
      grepl("img/U", stimulus) ~ "unattractive",
      grepl("img/A", stimulus) ~ "attractive",
      grepl("img/M", stimulus) ~ "average",
      TRUE ~ condition
    )
  )

halo_bar_data$choice <- as.numeric(halo_bar_data$choice)


summary_halo_data <- halo_bar_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice, na.rm = TRUE),
    se_choice = sd(choice, na.rm = TRUE) / sqrt(n())
  )

ggplot(summary_halo_data, aes(x = condition, y = mean_choice)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Average Persuasiveness by Attractiveness", x = "Condition", y = "Average Choice") +
  theme_minimal()

anova_model <- aov(choice ~ condition, data = halo_bar_data)
summary(anova_model)
tukey_hsd <- TukeyHSD(anova_model)
print(tukey_hsd)


## 1.10 hindsight bias? -----------------------------------------------------------------------
## 1.11 mere exposure effect? -----------------------------------------------------------------------
## 1.12 reference price? -----------------------------------------------------------------------
## 1.13 representativeness heuristic? -----------------------------------------------------------------------
## 1.14 status quo bias? -----------------------------------------------------------------------

# 2 Introspection-----------------------------------------------------------------------

## 2.1 anchoring effect? -----------------------------------------------------------------------

data_anchoring <- data %>%
  filter(task_name == "anchoring") %>%
  filter(stimulus != "")

data_anchoring$choice <- as.numeric(data_anchoring$choice)


#View(data_anchoring)

median_antarctic <- data_anchoring %>%
  filter(condition == 'No Anchor') %>%
  filter(stimulus == 'Antarctic Temperature') %>%
  summarize(median_choice = median(choice, na.rm = TRUE)) %>%
  pull(median_choice)

median_whale <- data_anchoring %>%
  filter(condition == 'No Anchor') %>%
  filter(stimulus == 'Whale Length') %>%
  summarize(median_choice = median(choice, na.rm = TRUE)) %>%
  pull(median_choice)

print(median_antarctic)
print(median_whale)

antarctic_subject_showing_effect <- data_anchoring %>%
  filter(stimulus == 'Antarctic Temperature') %>%
  filter(condition == 'Low Anchor') %>%
  filter(choice < median_antarctic) %>%
  pull(subject)

whale_subject_showing_effect <- data_anchoring %>%
  filter(stimulus == 'Whale Length') %>%
  filter(condition == 'Low Anchor') %>%
  filter(choice < median_whale) %>%
  pull(subject)

View(whale_subject_showing_effect)
View(antarctic_subject_showing_effect)



common_subjects <- intersect(antarctic_subject_showing_effect, whale_subject_showing_effect)
View(common_subjects)

introspection_showing_effect <- data %>%
  filter(subject %in% common_subjects) %>%
  filter(choice == '') %>%
  filter(task_name == 'anchoring') %>%
  pull(introspect_rating)

print(introspection_showing_effect)








## 2.2 associative memory effect? -----------------------------------------------------------------------


associative_data <- data %>%
  filter(task_name == "associative memory") %>%
  filter(introspect_rating != "") %>%
  filter(familiarity == "No")

summary_data <- associative_data %>%
  group_by(factor) %>%
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = sd(as.numeric(introspect_rating), na.rm = TRUE) / sqrt(n())
  )

View(associative_data)

ggplot(summary_data, aes(x = factor, y = mean_introspect_rating)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Difference in Introspect Rating between Factors",
       x = "Group",
       y = "Mean Introspect Rating") +
  theme_minimal()


#analysis

factor_ex_introspect <- associative_data %>%
  filter(factor == "Factor-Excluded") %>%
  mutate(introspect_rating = as.numeric(introspect_rating)) %>%
  pull(introspect_rating)

factor_in_introspect <- associative_data %>%
  filter(factor == "Factor-Included") %>%
  mutate(introspect_rating = as.numeric(introspect_rating)) %>%
  pull(introspect_rating)

t_test_result <- t.test(factor_ex_introspect, factor_in_introspect, var.equal = TRUE)
print(t_test_result)

#p=0.1

## 2.3 availability effect? -----------------------------------------------------------------------




## 2.4 belief effect? -----------------------------------------------------------------------
## 2.5 causal inference? -----------------------------------------------------------------------
## 2.6 contact principle? -----------------------------------------------------------------------
## 2.7 decoy effect? -----------------------------------------------------------------------
## 2.8 double effect? -----------------------------------------------------------------------
## 2.9 halo effect? -----------------------------------------------------------------------
## 2.10 hindsight bias? -----------------------------------------------------------------------
## 2.11 mere exposure effect? -----------------------------------------------------------------------
## 2.12 reference price? -----------------------------------------------------------------------
## 2.13 representativeness heuristic? -----------------------------------------------------------------------
## 2.14 status quo bias? -----------------------------------------------------------------------



