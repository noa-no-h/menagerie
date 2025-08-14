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






setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
dodge <- position_dodge(width=0.9)

data <- read.csv('data.csv') %>%
  arrange(subject, task_name) %>%
  mutate(total_time = ifelse(grepl("^total_time", introspect_rating), introspect_rating, NA)) %>%
  mutate(introspect_rating = ifelse(grepl("^total_time", introspect_rating), NA, introspect_rating)) %>%
  mutate(introspect_rating = as.numeric(introspect_rating))
head(data)

data = data %>%
  filter(familiarity != "Yes") %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded")))


# color palettes: hot for included, cool for excluded

#Included vs. Excluded
in_and_ex <- c("#F37121", "#4793AF")

in_neutral_ex <- c("#F37121", "#D3D3D3", "#4793AF")



#In&Effect, In&NoEffect, Ex&Effect, Ex&NoEffect
four_colors <- c("#f1c40f", "#e74c3c","#9b59b6", "#1abc9c")

#In&Effect, In&NoEffect, Ex
three_colors <- c("#f1c40f", "#e74c3c","#4793AF")

#In&Effect, Ex
two_colors <- c("#f1c40f", "#e74c3c")

theme_custom <- function() {
  theme_minimal(base_family = "Forum") +
    theme(
    )
}

font_import(pattern = "Forum", prompt = FALSE)
loadfonts(device = "pdf")

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
  guides(fill = FALSE)



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

ggplot(summary_status_quo_data, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Status Quo Introspection Ratings", x = "Condition", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)


t.test(introspect_rating ~ factor, data = status_quo_data)
#p-value = 0.3647

#next question: "intro_rating(included and said 70/30)
#<
#  intro_rating(included and said 50/50)"


status_quo_data <- status_quo_data %>%
  mutate(effect_group = case_when(
    choice == "70/30" & factor == "Factor-Included" ~ "Included and Status Quo",
    choice == "50/50" & factor == "Factor-Included" ~ "Included and not Status Quo",
    choice == "70/30" & factor == "Factor-Excluded" ~ "Excluded and Status Quo",
    choice == "50/50" & factor == "Factor-Excluded" ~ "Excluded and not Status Quo"
  ))%>%
  mutate(effect_group = factor(effect_group, levels = c("Included and Status Quo", "Included and not Status Quo",  "Excluded and Status Quo", "Excluded and not Status Quo")))


summary_status_quo_data <- status_quo_data %>%
  group_by(effect_group) %>%
  summarize(
    mean_introspect_rating = 100 - mean(introspect_rating),  # Flip the scale
    se_introspect_rating = se(introspect_rating)
  )


ggplot(summary_status_quo_data, aes(x = effect_group, y = mean_introspect_rating, fill = effect_group, fill = effect_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Introspection ratings", x = "Condition", y = "Introspection rating") +
  scale_fill_manual(values = four_colors) +
  theme_custom() +
  theme(text = element_text(family = "Forum")) +
  guides(fill = FALSE)


summary(lm(introspect_rating ~ effect_group, data = status_quo_data))
#effect_groupIncluded and Status Quo       0.0062 ** 

if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
library(knitr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

knitr::opts_chunk$set(dev = 'svg')

# Use the stitch function
knitr::spin("sample.R")
