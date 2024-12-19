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
library(forcats)  


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



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
dodge <- position_dodge(width=0.9)

#data <- read.csv('November_2024_pilot.csv') %>%
data <- read.csv('full_pilot.csv') %>%
  filter(subject != "") %>%
  arrange(subject, task_name) %>%
  filter(familiarity != "Yes") %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  mutate(introspect_rating = as.numeric(introspect_rating)) %>%
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

december_pilot = read_csv('v5pilot2.csv') %>%
  arrange(subject, task_name) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) 
  

subjects_all = data %>%
  pull(subject) %>%
  unique()

View(subjects_all)


#find subjects who need to be excluded

attention_exclude <- data %>%
  filter((`task_name` == "attention check 2" & `auxiliary_info1` == "Failure") |
           (`task_name` == "attention check 3" & `auxiliary_info1` == "Incorrect")) %>%
  pull(subject)

#events <- read.csv('NovemberBrowserEvents.csv') %>%
events <- read.csv('full_pilot_browser_events.csv') %>%
  arrange(subject)

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

demographics <- read.csv('full_pilot_demo.csv') %>%
  arrange(subject)

time_exclude <- demographics %>%
  mutate(total_time = total_time/60000)

View(time_exclude)

ggplot(time_exclude, aes(x = total_time)) +
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

p.vals = c()

#font_import(pattern = "Optima", prompt = FALSE)
loadfonts(device = "pdf")

# seen before analysis ----

#data <- read.csv('November_2024_pilot.csv') %>%
data_with_seen_before <- read.csv('full_pilot.csv') %>%
  filter(subject != "") %>%
  arrange(subject, task_name) %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  mutate(introspect_rating = as.numeric(introspect_rating))
View(data_with_seen_before)

familiarity_percentage <- data_with_seen_before %>%
  group_by(task_name) %>%
  summarise(total_responses = n(),
            yes_responses = sum(familiarity == "Yes")) %>%
  mutate(percentage_yes = (yes_responses / total_responses) * 100)


ggplot(familiarity_percentage, aes(x = fct_reorder(task_name, percentage_yes, .desc = TRUE), y = percentage_yes)) +
  geom_bar(stat = "identity", fill = "sky blue") +
  geom_text(aes(label = paste0(round(percentage_yes, 1), "%", " ", task_name), y = 0), 
            vjust = 0, 
            angle = 90, 
            color = "black",
            family = "Optima",
            size = 5, 
            hjust = 0) + 
  labs(title = "Percent Familiar With Each Task",
       x = "Task",
       y = "Percent Familiar") +
  theme_custom()+
  theme(axis.text.x = element_blank())

## 1.1 Affect ----


affect_data = december_pilot %>%
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
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

t.test(choice ~ factor, data = affect_data)

#7. Halo Effect  ✅ ----
##7.1 do we see the effect? ----

#Did subjects who were shown some attractive and 
#some attractive faces think the attractive were more persuasive, with the subjects who only saw neutral faces calling in the middle?

halo_bar_data <- data %>%
  filter(task_name == "halo") %>%
  mutate(choice = as.numeric(choice))%>%
  mutate(auxiliary_info1 = as.numeric(auxiliary_info1))%>%
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
    se_choice = se(choice),
    count = n() 
  )

ggplot(summary_halo_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  labs(title = "Average Persuasiveness by Attractiveness", x = "Condition", y = "Average Choice") +
  theme_custom()+
  scale_fill_manual(values = in_neutral_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


anova_result <- aov(choice ~ condition, data = halo_bar_data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)



ggplot(halo_bar_data, aes(x = choice, y = auxiliary_info1)) +
  geom_jitter(color = "black", width = 0.1, height = 0.1) +
  labs(title = "Persuasiveness vs. Attractiveness Ratings", x = "Persuasiveness Rating", y = "Attractiveness Rating") +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) +
  scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 1)) +
  theme_custom()



correlation_test <- cor.test(halo_bar_data$choice, halo_bar_data$auxiliary_info1)
print(correlation_test)


##7.2 introspection ----

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


#t.test(introspect_rating ~ factor, data = summary_halo_data)



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

#8 hindsight bias ----
##8.1 do we see the effect? ----

#Did subjects who saw the correct answers misremember their previous answers as closer to the correct answer than the subjects who did not see the correct answers?

hindsight_late <- data %>%
  filter(task_name == "hindsight") %>%
  filter(as.numeric(choice)<2000000000) %>%
  filter(as.Date(timestamp) > as.Date("2024-11-07")) %>%
  filter(!(subject %in% c("6109ca40ef8f38498af102ff", 
                          "670b086620f71c5b6cc49abc", 
                          "5a93bb216475f900019fa294"))) %>%
  filter(choice != "") %>%
  mutate(auxiliary_info1 = as.numeric(auxiliary_info1))



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

hindsight_early <- data %>%
  filter(task_name == "hindsight bias") %>%
  filter(!(subject %in% c("66749b876f32a4ad246db5da","5f2d95153c1140074cc81b4c","667469ea0d42f70a9a75567b","667444e5662b7a4ebf82d5e1","665c6f67d9ea69740afbcab8","6273238a4a8b39041ff1bd2c", "664d3c950d24d83ca7b4dd68", "6020606d7b0258677b881f63", "6159fe7811a7e1b94401c33f"))) %>%
  mutate(parsed_choice = convert_to_numeric(choice)) %>%
  filter(parsed_choice != "NA") %>%
  mutate(which_estimate = case_when(
    str_detect(auxiliary_info1, "_first_response") ~ "first",
    str_detect(auxiliary_info1, "_recall_original_response") ~ "recall",
    TRUE ~ NA_character_
  )) %>%
  mutate(stimulus = case_when(
    str_detect(auxiliary_info1, "_first_response") ~ "first estimate",
    str_detect(auxiliary_info1, "_recall_original_response") ~ "memory",
    TRUE ~ NA_character_
  )) %>%
  
  # Extract the country name
  mutate(country = case_when(
    str_detect(auxiliary_info1, "_estimate") ~ str_extract(auxiliary_info1, "^[^_]+(?:_[^_]+)*(?=_estimate)"),
    str_detect(auxiliary_info1, "_recall") ~ str_extract(auxiliary_info1, "^[^_]+(?:_[^_]+)*(?=_recall)"),
    TRUE ~ NA_character_
  ))%>%
  
  mutate(true_value = true_values[country])%>%
  mutate(auxiliary_info1 = as.numeric(true_value) - as.numeric(parsed_choice))%>%
  mutate(condition = factor)

View(hindsight_early)

# medians_hindsight <- hindsight_data %>%
#   group_by(subject) %>%
#   summarise(
#     median_first_estimate = abs(median(difference_from_true[which_estimate == "first estimate"], na.rm = TRUE)),
#     median_second_estimate = abs(median(difference_from_true[which_estimate == "memory"], na.rm = TRUE)),
#     factor = first(factor),
#     introspect_rating = first(introspect_rating)
#   ) %>%
#   filter(!is.na(median_first_estimate) & !is.na(median_second_estimate))
# 
# View(medians_hindsight)
# 
# custom_labels <- c("median_first_estimate" = "Median First Estimate",
#                    "median_second_estimate" = "Median Second Estimate")
# 
# 
# medians_summary <- medians_hindsight %>%
#   pivot_longer(cols = c(median_first_estimate, median_second_estimate),
#                names_to = "estimate_type",
#                values_to = "estimate_value") %>%
#   group_by(estimate_type, factor) %>%
#   mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
#   
#   summarise(
#     mean_choice = mean(estimate_value),
#     se_choice = se(estimate_value),
#     n_choice = n()
#   )

hindsight_data <- bind_rows(hindsight_early, hindsight_late)


# Create the subject-by-subject table with four categories
comparison_table <- hindsight_data %>%
  # Create a new column to identify if it's "first estimate" or "memory"
  mutate(stimulus_type = ifelse(grepl("first estimate$", stimulus), "first_estimate", "memory")) %>%
  # Group by subject, condition,  and stimulus_type
  group_by(subject, condition, stimulus_type) %>%
  # Calculate the mean of auxiliary_info1 for each group
  summarize(avg_auxiliary_info1 = mean(auxiliary_info1, na.rm = TRUE)) %>%
  ungroup() %>%
  # Pivot the data to have separate columns for each combination of status and stimulus_type
  pivot_wider(names_from = stimulus_type, values_from = avg_auxiliary_info1)

# Calculate summary statistics (mean, standard error, and count)
hindsight_summary_data <- comparison_table %>%
  rename(first_estimate = `first_estimate`, memory = `memory`) %>%
  pivot_longer(cols = c(first_estimate, memory), names_to = "estimate_type", values_to = "avg_auxiliary_info1") %>%
  group_by(condition, estimate_type) %>%
  summarize(
    mean_auxiliary_info1 = mean(avg_auxiliary_info1, na.rm = TRUE),
    se_auxiliary_info1 = sd(avg_auxiliary_info1, na.rm = TRUE) / sqrt(n()),
    n_auxiliary_info1 = n()
  ) %>%
  ungroup() %>%
  mutate(combined_condition = factor(
    paste(condition, estimate_type),
    levels = c("Factor-Included first_estimate", "Factor-Included memory", 
               "Factor-Excluded first_estimate", "Factor-Excluded memory")
  ))



# Plot with error bars and n labels
ggplot(hindsight_summary_data, aes(x = combined_condition, y = mean_auxiliary_info1, fill = combined_condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_auxiliary_info1 - se_auxiliary_info1, ymax = mean_auxiliary_info1 + se_auxiliary_info1),
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n=", n_auxiliary_info1)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  labs(title = "Hindsight Effect",
       x = "Condition and Estimate Type", y = "Average Distance from True Value") +
  theme_custom() +
  scale_fill_manual(values = c("#F37121", "#F37121","#4793AF", "#4793AF")) +
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "_", " "), width = 10))+
  guides(fill = FALSE)

t_test_result <- t.test(
  memory ~ condition,
  data = comparison_table)
print(t_test_result)


## 8.1.1 new hindsight ----

new_hindsight_data = december_pilot %>%
  filter(task_name == "hindsight effect")%>%
  filter(is.na(stimulus)) %>%
  mutate(choice = as.numeric(choice)) 


summary_new_hindsight_data <- new_hindsight_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("knowledge of outcome", "no knowledge of outcome"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(summary_new_hindsight_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Hindsight", x = "Condition", y = "Percent Likelihood of British Victory") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

t.test(choice ~ factor, data = new_hindsight_data)


##8.2 introspection----

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
    median_first_estimate = abs(median(difference_from_true[which_estimate == "first estimate"], na.rm = TRUE)),
    median_second_estimate = abs(median(difference_from_true[which_estimate == "memory"], na.rm = TRUE)),
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




#9 illusory truth ----
##9.1 do we see the effect? ----

illusory_truth_data <- data %>%
  filter(task_name == "illusion of truth pt2") %>%
  filter(stimulus != "")

false_positive_counts <- illusory_truth_data %>%
  filter(auxiliary_info1 == "false positive") %>%
  group_by(subject, factor) %>%
  summarize(false_positive_count = n()) %>%
  ungroup()

# Step 2: Calculate the average and standard error of false positives per subject for each factor
average_false_positives <- false_positive_counts %>%
  group_by(factor) %>%
  summarize(
    avg_false_positive = mean(false_positive_count),
    se_false_positive = se(false_positive_count),
    count = n()
  )


ggplot(average_false_positives, aes(x = factor, y = avg_false_positive, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_false_positive - se_false_positive, ymax = avg_false_positive + se_false_positive), width = 0.2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  labs(title = "Illusory Truth", x = "Condition", y = "Average False Positive Count") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

t_test_result <- t.test(
  false_positive_count ~ factor,  # Compare false positives between factors
  data = false_positive_counts,   # Use the data frame with counts per subject
)
print(t_test_result)

#9.11 new illusory ----

illusory_truth_data <- december_pilot %>%
  filter(task_name == "illusion of truth pt2") %>%
  filter(stimulus != "")%>%
  mutate(auxiliary_info1 = ifelse(choice > 50 & (condition == "false_new" | condition == "false_old"), 
                                  "false positive", 
                                  "not false positive"))

false_positive_counts <- illusory_truth_data %>%
  filter(auxiliary_info1 == "false positive") %>%
  group_by(subject, factor) %>%
  summarize(false_positive_count = n()) %>%
  ungroup()

# Step 2: Calculate the average and standard error of false positives per subject for each factor
average_false_positives <- false_positive_counts %>%
  group_by(factor) %>%
  summarize(
    avg_false_positive = mean(false_positive_count),
    se_false_positive = se(false_positive_count),
    count = n()
  )


ggplot(average_false_positives, aes(x = factor, y = avg_false_positive, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_false_positive - se_false_positive, ymax = avg_false_positive + se_false_positive), width = 0.2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  labs(title = "Illusory Truth", x = "Condition", y = "Average False Positive Count") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

t_test_result <- t.test(
  false_positive_count ~ factor,  # Compare false positives between factors
  data = false_positive_counts,   # Use the data frame with counts per subject
)
print(t_test_result)


#10 imaginability ----
#10.1 do we see the effect? ----

imaginability_data11 <- data %>%
  filter(task_name == "imaginability") %>%
  #mutate(choice = 100+as.numeric(choice)) %>%
  filter(as.Date(timestamp) == "2024-11-11") %>%
  mutate(choice = as.numeric(choice))
  
imaginability_data8 <- data %>%
  filter(task_name == "imaginability") %>%
  filter(as.Date(timestamp) == "2024-11-08") %>%
  mutate(choice = 0.09 * ((as.numeric(choice)) + 100) + 1) 

imaginability_data <- rbind(imaginability_data11, imaginability_data8)

summary_imaginability_data <- imaginability_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Difficult to Imagine", "Easy to Imagine"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(summary_imaginability_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Imaginability Effect", x = "Condition", y = "Perceived Likelihood of Catching Disease") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


t.test(choice ~ factor, data = imaginability_data)


#12 omission ----
##12.1 do we see the effect? ----


omission_data <- data %>%
  filter(task_name == "omission principle") %>%
  mutate(choice = as.numeric(choice))

summary_omission_data <- omission_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("commission", "omission"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(summary_omission_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Omission Principle", x = "Condition", y = "Forbidden to Obligatory") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


t.test(choice ~ factor, data = omission_data)



#13 recognition ----
##13.1 do we see the effect? ----

recognition_data <- data %>%
  filter(task_name == "recognition: city") %>%
  filter(factor == "Factor-Included")

count_recognition_data <- recognition_data %>%
  count(auxiliary_info1)

ggplot(count_recognition_data, aes(x = auxiliary_info1, y = n, fill = auxiliary_info1)) +
  geom_bar(stat = "identity") +
  labs(title = "Recognition Effect for City Population", x = "Within Factor-Included", y = "Count") +
  geom_text(aes(label = paste0("n=", n)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

count_data <- table(recognition_data$auxiliary_info1)
chisq_test <- chisq.test(count_data)
print(chisq_test)

#14 reference price ✅ ----
##14.1 do we see the effect? ----

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
    se_choice = se(choice_parsed),
    count = n()
  )

ggplot(summary_reference_price_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Amount Willing to Pay for Beer", x = "Condition", y = "Average Amount Willing to Pay (Dollars)") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))


t.test(choice_parsed ~ factor, data = reference_price_data)


##14.2 introspection----

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


#15 representativeness ----
##15.1 do we see the effect? ----

representativeness_data <- data %>%
  filter(task_name == "rep") %>%
  mutate(choice = as.numeric(choice))

summary_representativeness_data <- representativeness_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(summary_representativeness_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Is Jack an Engineer?", x = "Condition", y = "average likelihood of engineer") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

t.test(choice ~ factor, data = representativeness_data)


#16 status quo ✅ ----

##16.1 do we see the effect ----

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

summary(glm(choice_binary ~ condition, data = status_quo_data, family = 'binomial'))

## 16.1.1 new status quo

status_quo_data <- december_pilot %>%
  filter(task_name == "status_quo") %>%
  filter(is.na(stimulus)) %>%
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

summary(glm(choice_binary ~ condition, data = status_quo_data, family = 'binomial'))



##16.2 introspection----

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

#17 sunk cost ----
##17.1 do we see the effect? ----

sunk_cost_data <- data %>%
  filter(task_name == "sunk_cost effect") %>% 
  mutate(switched = choice == 'Solar-powered Pump',
         switched.num = as.numeric(switched))


percentage_sunk_cost_data <- sunk_cost_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Sunk Cost", "No Sunk Cost"))) %>%
  summarize(
    total_in_condition = n(),  # Total number of subjects in each condition
    solar_powered_count = sum(choice == "Solar-powered Pump")  # Count who chose "Solar-powered pump"
  ) %>%
  mutate(percentage_solar_powered = (solar_powered_count / total_in_condition) * 100)

summary(glm(switched ~ condition, data = sunk_cost_data, family = 'binomial'))

ggplot(percentage_sunk_cost_data, aes(x = condition, y = percentage_solar_powered, fill = condition)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Switching Projects by Condition", x = "Condition", y = "Percentage of Choices to Switch") +
  geom_text(aes(label = paste0("n=", total_in_condition)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

solar_powered_counts <- percentage_sunk_cost_data$solar_powered_count
total_counts <- percentage_sunk_cost_data$total_in_condition
prop_test_result <- prop.test(solar_powered_counts, total_counts)
print(prop_test_result)

summary(glm(switched ~ condition, data = sunk_cost_data, family = 'binomial'))

test = brm(switched.num ~ condition, data = sunk_cost_data, family = 'binomial')

##17.1.1 new sunk cost

sunk_cost_data <- december_pilot %>%
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

summary(glm(switched ~ condition, data = sunk_cost_data, family = 'binomial'))

ggplot(percentage_sunk_cost_data, aes(x = condition, y = percentage_switched, fill = condition)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage Who Stopped Investing", x = "Condition", y = "Percentage of Choices to Stop Investing") +
  geom_text(aes(label = paste0("n=", total_in_condition)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

solar_powered_counts <- percentage_sunk_cost_data$solar_powered_count
total_counts <- percentage_sunk_cost_data$total_in_condition
prop_test_result <- prop.test(solar_powered_counts, total_counts)
print(prop_test_result)

summary(glm(switched ~ condition, data = sunk_cost_data, family = 'binomial'))

test = brm(switched.num ~ condition, data = sunk_cost_data, family = 'binomial')



## 18.1 Simulation ----


simulation_data = december_pilot %>%
  filter(task_name == "simulation")%>%
  filter(is.na(stimulus)) %>%
  mutate(choice = as.numeric(choice)) 


summary_simulation_data <- simulation_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("barely missed", "missed"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(summary_simulation_data, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Simulation", x = "Condition", y = "How upset would he feel") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+   scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

t.test(choice ~ factor, data = simulation_data)


# Q1: Aggregating across tasks, are participants aware that they are being influenced by the heuristics or biases? ----


q1data <- data %>%
  filter(introspect_rating != "" & factor == "Factor-Included") 
  
#restrict to factor included condition. Find HDI for intercept

  model <- brm(
    formula = introspect_rating ~ 1 + (1 | subject) + (1 | task_name),
    data = q1data,
    family = gaussian(),
    prior = c(
      prior(normal(0, 10), class = "Intercept"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4,           # Number of MCMC chains
    iter = 2000,          # Number of iterations per chain
    warmup = 500,         # Number of warmup iterations per chain
    cores = 4             # Number of cores to use for computation
  )
  
  summary(model)
  #          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
  #Intercept    57.25      2.58    51.80    61.97 1.00     1510     2654
  
# Q1.5: How about factor excluded?   
  
  
  q15data <- data %>%
    filter(introspect_rating != "" & factor == "Factor-Excluded") 
  
  #restrict to factor included condition. Find HDI for intercept
  
  model <- brm(
    formula = introspect_rating ~ 1 + (1 | subject) + (1 | task_name),
    data = q15data,
    family = gaussian(),
    prior = c(
      prior(normal(0, 10), class = "Intercept"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4,           # Number of MCMC chains
    iter = 2000,          # Number of iterations per chain
    warmup = 500,         # Number of warmup iterations per chain
    cores = 4             # Number of cores to use for computation
  )
  
  summary(model)
  
# Q2: Does that awareness come from actual experience of the heuristic or bias, or is it attributable to lay theories? ----

  
  q2data <- data %>%
    filter(introspect_rating != "") 
  

  model <- brm(
    formula = introspect_rating ~ factor + (1 | subject) + (1 + factor | task_name),
    data = q2data,
    family = gaussian(),
    prior = c(
      prior(normal(0, 10), class = "Intercept"),
      prior(exponential(1), class = "sd")
    ),
    chains = 4,           # Number of MCMC chains
    iter = 2000,          # Number of iterations per chain
    warmup = 500,         # Number of warmup iterations per chain
    cores = 4             # Number of cores to use for computation
  )
  
  summary(model)
  
  #                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
  #Intercept                57.94      2.48    52.60    62.49 1.00     1862
  #factorFactorMExcluded    -1.69      1.99    -5.70     2.24 1.00     4873
  #Tail_ESS
  #Intercept                 2084
  #factorFactorMExcluded     4111
  
#Q3 Are participants who are more impacted by a bias consciously aware of the extent to which they are affected by heuristics and biases? ----

  #Halo
  halo_affected_data = data %>%
    filter(task_name == "halo") %>%
    mutate(
      affected_level = if_else(
        stimulus != "",
        #attractiveness is likert 1-7
        100* abs((as.numeric(auxiliary_info1) - 1) / 6 - (as.numeric(choice) - 1) / 4), 
        NA_real_  
      )
    )
  
  
    # Hindsight
  hindsight_affected_data <- data %>%
    filter(task_name == "hindsight") %>%
    mutate(country = word(stimulus, 1)) %>%
        group_by(country) %>%
    mutate(
      first_estimate = if_else(grepl("first estimate", stimulus), as.numeric(choice), NA_real_),
      memory_estimate = if_else(grepl("memory", stimulus), as.numeric(choice), NA_real_)
    ) %>%
    fill(first_estimate, .direction = "downup") %>%  # Fill down and up to have the first estimate in each row
    fill(memory_estimate, .direction = "downup") %>% # Same for memory estimate
    ungroup() %>% mutate(
      affected_level = if_else(
        grepl("memory", stimulus),  # Only calculate for "memory" rows
        # Calculate the affected level as the normalized difference
        abs(first_estimate - memory_estimate) / abs(as.numeric(auxiliary_info1) - first_estimate) * 100,
        NA_real_  # Set to NA if condition is not met
      )
    )
    
  
  # illusion of truth
  illusion_of_truth_affected_data <- data %>%
    filter(task_name == "illusion of truth pt2") %>%
    mutate(
      affected_level = if_else(
        task_name == "illusion of truth pt2" & stimulus != "" & auxiliary_info1 == "false positive",
        100, 0))
  
  #imaginability
  imaginability_affected_data <- imaginability_data %>%
    mutate(
      affected_level = if_else(
        condition == "Easy to Imagine",
        (choice - 1) / 9 * 100,  # Scale 1 to 10 choice to 0 to 100 for Easy to Imagine
        (10 - choice) / 9 * 100  # Scale 1 to 10 choice to 100 to 0 for Difficult to Imagine
      )
    )
  
  
  #omission
  omission_affected_data <- data %>%
    filter(task_name == "omission principle") %>%
    mutate(
      affected_level = if_else(
        condition == "commission",
        (as.numeric(choice) - 1) / 6 * 100,         # When condition is "commission"
        100 - ((as.numeric(choice) - 1) / 6 * 100)  # When condition is not "commission" (assumed to be omission)
      )
    )
  
  #reference price TO DO
  reference_price_affected_data <- data %>%
    filter(task_name == "reference price")
  
  #representativeness TO DO
  
  #status quo
  status_quo_affected_data <- data %>%
    filter(task_name == "status_quo") %>%
    mutate(
      affected_level = if_else(
        choice == "70/30",
        100, 0))
  
  #sunk cost
  sunk_cost_affected_data <- data %>%
    filter(task_name == "sunk_cost effect") %>%
    mutate(
      affected_level = if_else(
        choice == "Rocket Engine",
        100, 0))
  
  list_affected_dfs <- list(
    halo_affected_data,
    #hindsight_affected_data,
    #illusion_of_truth_affected_data,
    #imaginability_affected_data,
    #omission_affected_data,
    status_quo_affected_data,
    sunk_cost_affected_data
    
  )
  
  # Convert `choice` to character in each data frame and bind them
  affected_level_data <- bind_rows(lapply(list_affected_dfs, function(df) {
    df %>% mutate(choice = as.character(choice))
  }))
  
  
View(affected_level_data)


q3data <- affected_level_data %>%
  filter(introspect_rating != "" & !is.na(affected_level))


model <- brm(
  formula = introspect_rating ~ affected_level * factor + (1 + affected_level | subject) + (1 + factor * affected_level | task_name)
,
  data = q3data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4,           # Number of MCMC chains
  iter = 2000,          # Number of iterations per chain
  warmup = 500,         # Number of warmup iterations per chain
  cores = 4             # Number of cores to use for computation
)

summary(model)
  
 ----


# Q4: Variation across tasks ----------------------------------------------
model <- brm(
  formula = introspect_rating ~ factor + (1 | subject) + (1 + factor | task_name),
  data = data %>% filter(introspect_rating != ''),
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4,           # Number of MCMC chains
  iter = 2000,          # Number of iterations per chain
  warmup = 500,         # Number of warmup iterations per chain
  cores = 4             # Number of cores to use for computation
)

summary(model)
hdi(model, effects = 'all')

model2 <- brm(
  formula = introspect_rating ~ task_name + factor * task_name + (1 | subject), #+ (1 + factor | task_name),
  data = data %>% filter(introspect_rating != ''),
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4,           # Number of MCMC chains
  iter = 2000,          # Number of iterations per chain
  warmup = 500,         # Number of warmup iterations per chain
  cores = 4             # Number of cores to use for computation
)
summary(model2)

  # Knittr ----

if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
library(knitr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

knitr::opts_chunk$set(dev = 'svg')

# Use the stitch function
knitr::spin("analysis2024.R")
