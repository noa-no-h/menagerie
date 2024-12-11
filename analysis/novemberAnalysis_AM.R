# Setup -------------------------------------------------------------------
if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'lme4', 'lmerTest', 'extrafont', 'RColorBrewer',
              'this.path', 'brms', 'extrafont', 'bayestestR')
p_load(char = pkg.names)

setwd(here())

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

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
dodge <- position_dodge(width=0.9)

# Load data ---------------------------------------------------------------

data <- read.csv('full_pilot.csv') %>%
  filter(subject != "") %>%
  arrange(subject, task_name) %>%
  #filter(familiarity != "Yes") %>%
  mutate(factor = factor(factor, levels = c("Factor-Included", "Factor-Excluded")),
         introspect_rating = as.numeric(introspect_rating),
         introspect_rating = if_else(
            introspect_rating != "" & task_name %in% c("associative memory", "availability", 
                                                       "decoy effect", "hindsight bias", 
                                                       "omission principle", "reference price",
                                                       "status_quo", "sunk_cost effect"),
            100 - introspect_rating,
            introspect_rating)
         )

subjects_all = data %>%
  pull(subject) %>%
  unique()

#find subjects who need to be excluded

attention_exclude <- data %>%
  filter((`task_name` == "attention check 2" & `auxiliary_info1` == "Failure") |
           (`task_name` == "attention check 3" & `auxiliary_info1` == "Incorrect")) %>%
  pull(subject)

events <- read.csv('full_pilot_browser_events.csv') %>%
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

demographics <- read.csv('full_pilot_demo.csv') %>%
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

p.vals = c()

#font_import(pattern = "Optima", prompt = FALSE)
loadfonts(device = "pdf")

#7. Halo effect ----
##7.1 do we see the effect? ----

#Did subjects who were shown some attractive and 
#some attractive faces think the attractive were more persuasive, with the subjects who only saw neutral faces calling in the middle?

halo_data <- data %>%
  filter(task_name == "halo") %>%
  mutate(choice = as.numeric(choice),
         auxiliary_info1 = as.numeric(auxiliary_info1))

halo_data_choices = halo_data %>% 
  filter(stimulus != "") %>%
  mutate(
    condition = case_when(
      grepl("img/U", stimulus) ~ "unattractive",
      grepl("img/A", stimulus) ~ "attractive",
      grepl("img/M", stimulus) ~ "average",
      TRUE ~ condition
    )
  )

halo_summary <- halo_data_choices %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    mean_attractiveness = mean(auxiliary_info1, na.rm = T),
    se_attractiveness = se(auxiliary_info1),
    count = n() 
  )

# manipulation check
ggplot(halo_summary, aes(x = condition, y = mean_attractiveness, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_attractiveness - se_attractiveness, ymax = mean_attractiveness + se_attractiveness), width = 0.2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  labs(title = "Average Attractiveness by Condition", x = "Condition", y = "Average Attractiveness") +
  theme_custom()+
  scale_fill_manual(values = in_neutral_ex)+
  guides(fill = FALSE)

# actual effect
ggplot(halo_summary, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  labs(title = "Average Persuasiveness by Attractiveness", x = "Condition", y = "Average Choice") +
  theme_custom()+
  scale_fill_manual(values = in_neutral_ex)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

# bayesian analysis
halo_analysis = brm(choice ~ condition + (1 | subject),
                           data = halo_data_choices)
summary(halo_analysis)
hdi(halo_analysis, effects = 'all')

# 
# ggplot(halo_bar_data, aes(x = auxiliary_info1, y = choice)) +
#   geom_jitter(color = "black", width = 0.1, height = 0.1) +
#   labs(title = "Persuasiveness vs. Attractiveness Ratings", x = "Attractiveness Rating", y = "Persuasiveness Rating") +
#   #scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) +
#   #scale_x_continuous(limits = c(1, 5), breaks = seq(1, 5, by = 1)) +
#   theme_custom()
# 
# 
# correlation_test <- cor.test(halo_bar_data$choice, halo_bar_data$auxiliary_info1)
# print(correlation_test)

##7.2 are people aware of the effect? ----

halo_data_introspection = halo_data %>% 
  filter(stimulus == "")

## in experience condition

# halo_effect_analysis_factorincluded = brm(choice ~ condition + (condition | subject),
#                            data = halo_bar_data %>% filter(factor == 'Factor-Included'))
# summary(halo_effect_analysis_factorincluded)
# hdi(halo_effect_analysis, effects = 'all')
# halo_effect_sizes = ranef(halo_effect_analysis_factorincluded)$subject[,1,2]
# hist(halo_effect_sizes)

halo_data_introspection_experience = halo_data_introspection %>% 
  filter(factor == 'Factor-Included')

halo_effectsizes <- halo_data_choices %>%
  filter(factor == 'Factor-Included') %>%
  group_by(subject, condition) %>%
  summarize(mean_choice = mean(choice), .groups = 'drop') %>%
  pivot_wider(names_from = condition, values_from = mean_choice) %>%
  mutate(effect_size = attractive - unattractive) %>%
  select(-attractive, -unattractive)

halo_data_introspection_experience = halo_data_introspection_experience %>% 
  left_join(halo_effectsizes, by = 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0, c(T,F), c('Effect', 'No effect')))

# dichotomous
halo_summary_introspection_experience = halo_data_introspection_experience %>% 
  #filter(factor == 'Factor-Included') %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  )

ggplot(halo_summary_introspection_experience,
       aes(x = showed_effect, y = mean_introspect_rating)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Halo Introspection ratings", x = "Showed effect?", y = "Introspection rating") +
  theme_custom()

halo_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                      halo_data_introspection_experience %>% filter(showed_effect))
summary(halo_analysis_introspection_experience_midpoint)
hdi(halo_analysis_introspection_experience_midpoint)

halo_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect, halo_data_introspection_experience)
summary(halo_analysis_introspection_experience_dichotomized)
hdi(halo_analysis_introspection_experience_dichotomized)

# continuous
ggplot(halo_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(title = "Halo Introspection ratings", x = "Effect size", y = "Introspection rating")

halo_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size, halo_data_introspection_experience)
summary(halo_analysis_introspection_experience_continuous)
hdi(halo_analysis_introspection_experience_continuous)

# across conditions
halo_summary_introspection_both <- halo_data_introspection %>% 
  group_by(factor) %>%
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(halo_summary_introspection_both, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Halo Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

halo_analysis_introspection_both = brm(introspect_rating ~ factor,
                                  data = halo_data_introspection)
summary(halo_analysis_introspection_both)
hdi(halo_analysis_introspection_both, effects = 'all')

#12 Omission effect ----
##12.1 do we see the effect? ----

omission_data <- data %>%
  filter(task_name == "omission principle") %>%
  mutate(choice = as.numeric(choice))

omission_summary <- omission_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("commission", "omission"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(omission_summary, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Omission Principle", x = "Condition", y = "Forbidden to Obligatory") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

omission_analysis = brm(choice ~ condition, omission_data)
summary(omission_analysis)
hdi(omission_analysis)

##12.2 are people aware of the effect? -----------------------------------------

omission_data_introspection = omission_data %>% 
  filter(stimulus == "")

## in experience condition
omission_data_introspection_experience <- omission_data_introspection %>% 
  filter(factor == 'Factor-Included') %>% 
  mutate(effect_size = choice,
         showed_effect = factor(choice < 4, c(T,F), c('Effect', 'No effect')))

# dichotomized
omission_summary_introspection_experience <- omission_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(omission_summary_introspection_experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Omission introspection ratings", x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

omission_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect, omission_data_introspection_experience)
summary(omission_analysis_introspection_experience_dichotomized)
hdi(omission_analysis_introspection_experience_dichotomized)

# continuous
ggplot(omission_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

omission_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size, omission_data_introspection_experience)
summary(omission_analysis_introspection_experience_continuous)
hdi(omission_analysis_introspection_experience_continuous)

# across conditions

omission_summary_introspection_both <- omission_data_introspection %>% 
  group_by(factor) %>%
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(omission_summary_introspection_both, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Omission introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

omission_analysis_introspection_both = brm(introspect_rating ~ factor,
                                           data = omission_data_introspection)
summary(omission_analysis_introspection_both)
hdi(omission_analysis_introspection_both, effects = 'all')

#13 Recognition heuristic ----
##13.1 do we see the effect? ----

recognition_data <- data %>%
  filter(task_name == "recognition: city") %>%
  filter(factor == "Factor-Included") %>% 
  mutate(chose_recognizable = auxiliary_info1 == 'chose recognizable',
         chose_recognizable_num = as.numeric(chose_recognizable))

recognition_count <- recognition_data %>%
  count(auxiliary_info1)

ggplot(recognition_count, aes(x = auxiliary_info1, y = n, fill = auxiliary_info1)) +
  geom_bar(stat = "identity") +
  labs(title = "Recognition Effect for City Population", x = "Within Factor-Included", y = "Count") +
  geom_text(aes(label = paste0("n=", n)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

recognition_analysis = brm(chose_recognizable_num ~ 1 + (1 | subject), 
                           recognition_data,
                           family = 'bernoulli')
summary(recognition_analysis)
hdi(recognition_analysis)

##13.2 are people aware of the effect? -----------------------------------------
recognition_data_introspection <- data %>%
  filter(task_name == "recognition")

## in experience condition
recognition_data_introspection_experience <- recognition_data_introspection %>%
  filter(factor == "Factor-Included")

recognition_effectsize = recognition_data %>% 
  group_by(subject) %>% 
  summarize(effect_size = mean(chose_recognizable)) %>% 
  select(subject, effect_size)

recognition_data_introspection_experience = recognition_data_introspection_experience %>% 
  left_join(recognition_data_effectsize, 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0.5, c(T,F), c('Effect', 'No effect')))

# dichotomous
recognition_summary_introspection_experience = recognition_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating),
            se_introspect_rating = se(introspect_rating))

ggplot(recognition_summary_introspection_experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Recognition introspection ratings: Experience condition", x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))


recognition_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                                recognition_data_introspection_experience %>% filter(showed_effect))
summary(recognition_analysis_introspection_experience_midpoint)
hdi(recognition_analysis_introspection_experience_midpoint)

recognition_analysis_introspection_experience_dichotomous = brm(introspect_rating ~ showed_effect,
                                                    recognition_data_introspection_experience)
summary(recognition_analysis_introspection_experience_dichotomous)
hdi(recognition_analysis_introspection_experience_dichotomous)

# continuous
ggplot(recognition_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

recognition_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size,
                                                                recognition_data_introspection_experience)
summary(recognition_analysis_introspection_experience_continuous)
hdi(recognition_analysis_introspection_experience_continuous)

# across conditions
recognition_summary_introspection_both <- recognition_data_introspection %>% 
  group_by(factor) %>%
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(recognition_summary_introspection_both, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "recognition introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

recognition_analysis_introspection_both = brm(introspect_rating ~ factor,
                                           data = recognition_data_introspection)
summary(recognition_analysis_introspection_both)
hdi(recognition_analysis_introspection_both, effects = 'all')

#14 Reference price ----
##14.1 do we see the effect? ----

#When subjects were told the hotel was fancy, were 
#they more likely to give a higher price they'd be willing to pay?

reference_data <- data %>%
  filter(task_name == "reference price") %>%
  mutate(choice_parsed = parse_number(choice)) %>% 
  filter(choice_parsed <= 40)

reference_summary <- reference_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("hotel", "motel"))) %>%
  summarize(
    mean_choice = mean(choice_parsed),
    se_choice = se(choice_parsed),
    count = n()
  )

ggplot(reference_summary, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Amount Willing to Pay for Beer", x = "Condition", y = "Average Amount Willing to Pay (Dollars)") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

reference_analysis = brm(choice_parsed ~ condition,
                         reference_data)
summary(reference_analysis)
hdi(reference_analysis)

##14.2 are people aware of the effect?----

# in experience condition
reference_median = mean(reference_data$choice_parsed[reference_data$factor == 'Factor-Excluded'])
reference_data_introspection_experience = reference_data %>% 
  filter(factor == 'Factor-Included') %>% 
  mutate(effect_size = choice_parsed,
         showed_effect = factor(choice_parsed > reference_median, c(T,F), c('Effect', 'No effect')))

# dichotomized
reference_summary_introspection_experience <- reference_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(reference_summary_introspection_experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "reference introspection ratings", x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

reference_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect, reference_data_introspection_experience)
summary(reference_analysis_introspection_experience_dichotomized)
hdi(reference_analysis_introspection_experience_dichotomized)

# continuous
ggplot(reference_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

reference_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size, reference_data_introspection_experience)
summary(reference_analysis_introspection_experience_continuous)
hdi(reference_analysis_introspection_experience_continuous)

# across conditions
reference_summary_introspection_both <- reference_data %>%
  group_by(condition) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(reference_summary_introspection_both, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Reference Price Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))

reference_analysis_introspection_both = brm(introspect_rating ~ condition,
                                            reference_data)
summary(reference_analysis_introspection_both)
hdi(reference_analysis_introspection_both)

#15 representativeness ----
##15.1 do we see the effect? ----

representativeness_data <- data %>%
  filter(task_name == "rep") %>%
  mutate(choice = as.numeric(choice))

representativeness_summary <- representativeness_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(representativeness_summary, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Is Jack an Engineer?", x = "Condition", y = "average likelihood of engineer") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

representativeness_analysis = brm(choice ~ condition, representativeness_data)
summary(representativeness_analysis)
hdi(representativeness_analysis)

##15.2 are people aware of the effect? -----------------------------------------

# in experience condition
representativeness_median = mean(representativeness_data$choice[representativeness_data$factor == 'Factor-Excluded'])
representativeness_data_introspection_experience = representativeness_data %>% 
  filter(factor == 'Factor-Included') %>% 
  mutate(effect_size = choice,
         showed_effect = factor(choice > representativeness_median, c(T,F), c('Effect', 'No effect')))

# dichotomized
representativeness_summary_introspection_experience <- representativeness_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(representativeness_summary_introspection_experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "representativeness introspection ratings", x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

representativeness_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1, representativeness_data_introspection_experience)
summary(representativeness_analysis_introspection_experience_midpoint)
hdi(representativeness_analysis_introspection_experience_midpoint)

representativeness_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect, representativeness_data_introspection_experience)
summary(representativeness_analysis_introspection_experience_dichotomized)
hdi(representativeness_analysis_introspection_experience_dichotomized)

# continuous
ggplot(representativeness_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

representativeness_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size, representativeness_data_introspection_experience)
summary(representativeness_analysis_introspection_experience_continuous)
hdi(representativeness_analysis_introspection_experience_continuous)

# across conditions
representativeness_summary_introspection_both <- representativeness_data %>%
  group_by(condition) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(representativeness_summary_introspection_both, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "representativeness Price Introspection ratings", x = "Condition", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+   scale_y_continuous(limits = c(0, 100))

representativeness_analysis_introspection_both = brm(introspect_rating ~ condition,
                                            representativeness_data)
summary(representativeness_analysis_introspection_both)
hdi(representativeness_analysis_introspection_both)

#16 status quo ----

##16.1 do we see the effect? ----

statusquo_data <- data %>%
  filter(task_name == "status_quo") %>%
  mutate(choice_binary = as.numeric(choice == "70/30"))%>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) 

statusquo_summary <- statusquo_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice_binary),
    se_choice = se.prop(choice_binary),
    count = n()
  )

ggplot(statusquo_summary, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice,
                    ymax = mean_choice + se_choice),
                width = 0.2)+
  labs(title = "Choices to continue the status quo", x = "Condition", y = "Percent subjects who recommended the status quo") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
   theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

statusquo_analysis = brm(choice_binary ~ condition, data = statusquo_data, family = 'bernoulli')
summary(statusquo_analysis)
hdi(statusquo_analysis)

##16.2 are people aware of the effect? ----

## in experience condition
statusquo_data_introspection_experience = statusquo_data %>% 
  filter(factor == 'Factor-Included') %>% 
  mutate(effect_size = choice_binary,
         showed_effect = factor(choice_binary, c(1,0), c('Effect', 'No effect')))

# dichotomized
statusquo_summary_introspection_experience <- statusquo_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(statusquo_summary_introspection_experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "statusquo introspection ratings", x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

statusquo_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1, statusquo_data_introspection_experience)
summary(statusquo_analysis_introspection_experience_midpoint)
hdi(statusquo_analysis_introspection_experience_midpoint)

statusquo_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect, statusquo_data_introspection_experience)
summary(statusquo_analysis_introspection_experience_dichotomized)
hdi(statusquo_analysis_introspection_experience_dichotomized)

## across conditions

statusquo_summary_introspection_both <- statusquo_data %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  group_by(condition) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating), # check this
    se_introspect_rating = se(introspect_rating)
  )

ggplot(statusquo_summary_introspection_both, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Status Quo Introspection Ratings", x = "Condition", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

statusquo_analysis_introspection_both = brm(introspect_rating ~ condition,
                                                     statusquo_data)
summary(statusquo_analysis_introspection_both)
hdi(statusquo_analysis_introspection_both)

#17 sunk cost ----
##17.1 do we see the effect? ----

sunkcost_data <- data %>%
  filter(task_name == "sunk_cost effect") %>% 
  mutate(switched = choice == 'Solar-powered Pump',
         switched.num = as.numeric(switched),
         condition = factor(condition, levels = c("Sunk Cost", "No Sunk Cost")))

sunkcost_summary <- sunkcost_data %>%
  group_by(condition) %>%
  summarize(mean_switched = mean(switched),
            se_switched = se.prop(switched),
            total = n())

ggplot(sunkcost_summary, aes(x = condition, y = mean_switched, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = mean_switched - se_switched, 
        ymax = mean_switched + se_switched), 
    width = 0.2
  ) +
  labs(title = "Percentage Switching Projects by Condition", x = "Condition", y = "Percentage of Choices to Switch") +
  geom_text(aes(label = paste0("n=", total)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

sunkcost_effect_analysis = brm(switched.num ~ condition,
                               data = sunk_cost_data,
                               family = 'bernoulli')
summary(sunkcost_effect_analysis)
hdi(sunkcost_effect_analysis, effects = 'all')


##17.2 are people aware of the effect? -----------------------------------------

## in experience condition
sunkcost_data_introspection_experience = sunkcost_data %>% 
  filter(factor == 'Factor-Included') %>% 
  mutate(effect_size = !switched,
         showed_effect = factor(!switched, c(T,F), c('Effect', 'No effect')))

# dichotomized
sunkcost_summary_introspection_experience <- sunkcost_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(sunkcost_summary_introspection_experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "sunkcost introspection ratings", x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

sunkcost_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1, sunkcost_data_introspection_experience)
summary(sunkcost_analysis_introspection_experience_midpoint)
hdi(sunkcost_analysis_introspection_experience_midpoint)

sunkcost_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect, sunkcost_data_introspection_experience)
summary(sunkcost_analysis_introspection_experience_dichotomized)
hdi(sunkcost_analysis_introspection_experience_dichotomized)

## across conditions

sunkcost_summary_introspection_both <- sunkcost_data %>%
  mutate(condition = factor(factor, levels = c("Factor-Included", "Factor-Excluded"))) %>%
  group_by(condition) %>%
  summarize(
    mean_introspect_rating = mean(introspect_rating), # check this
    se_introspect_rating = se(introspect_rating)
  )

ggplot(sunkcost_summary_introspection_both, aes(x = condition, y = mean_introspect_rating, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Status Quo Introspection Ratings", x = "Condition", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

sunkcost_analysis_introspection_both = brm(introspect_rating ~ condition,
                                            sunkcost_data)
summary(sunkcost_analysis_introspection_both)
hdi(sunkcost_analysis_introspection_both)


# all tasks ---------------------------------------------------------------

all_list_introspection_experience = list(halo_data_introspection_experience,
                                         omission_data_introspection_experience,
                                         recognition_data_introspection_experience,
                                         reference_data_introspection_experience,
                                         representativeness_data_introspection_experience,
                                         sunkcost_data_introspection_experience)

all_data_introspection_experience = all_list_introspection_experience[[1]] %>% 
  select(subject, task_name, introspect_rating, effect_size, showed_effect)
for (i in 2:length(all_data_introspection_experience)) {
  all_data_introspection_experience = all_data_introspection_experience %>% 
    rbind(all_list_introspection_experience[[i]] %>% 
            select(subject, task_name, introspect_rating, effect_size, showed_effect))
}

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


# Save image --------------------------------------------------------------

save.image('novemberAnalysis_AM_output.rdata')
