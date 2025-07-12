# Setup -------------------------------------------------------------------
if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'RColorBrewer', 'extrafont',
              'this.path', 'brms', 'bayestestR', 'rstan', 'posterior')
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

data <- read.csv('pilot2_data.csv') %>%
  filter(subject != "") %>%
  arrange(subject, task_name) %>%
  mutate(factor = factor(factor, c("Factor-Included", "Factor-Excluded"), c("experience", "control")),
         introspect_rating = as.numeric(introspect_rating), # make numeric
         introspect_rating = if_else(
            introspect_rating != "" & task_name %in% c("associative memory", "availability", 
                                                       "decoy effect", "hindsight bias", 
                                                       "omission principle", "reference price",
                                                       "status_quo", "sunk_cost effect"),
            100 - introspect_rating, # reverse-coded ones
            introspect_rating),
         introspect_rating = introspect_rating - 50 # center around midpoint of zero
         )

subjects_all = data %>%
  pull(subject) %>%
  unique()


#find subjects who need to be excluded

attention_exclude <- data %>%
  filter((`task_name` == "attention check 2" & `auxiliary_info1` == "Failure") |
           (`task_name` == "attention check 3" & `auxiliary_info1` == "Incorrect")) %>%
  pull(subject)

events <- read.csv('pilot2_browser_events.csv') %>%
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

demographics <- read.csv('pilot2_demographics.csv') %>%
  arrange(subject) %>% 
  mutate(total_time = total_time/60000)

ggplot(demographics, aes(x = total_time)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Time Histogram", x = "Minutes", y = "Count") +
  theme_custom()

print(median(demographics$total_time))

nofactor = df %>% 
  filter(subject %in% demographics$subject,
         is.na(factor)) %>% 
  pull(subject)
length(nofactor) #0

all_potential_exclusions <- c(attention_exclude, tab_away_exclude)
to_exclude <- unique(all_potential_exclusions)

number_subjects <- n_distinct(data$subject)
number_to_exclude <- length(to_exclude)
print(number_subjects) #324
print(number_to_exclude) #115
#324-115 = 209

subjects_after_main_exclusion <- data %>%
  filter(!subject %in% to_exclude) %>%
  pull(subject) %>%
  unique()

final_subjects <- data %>%
  filter(!subject %in% to_exclude,
         !is.na(factor),
         !(subject == "62d06d1b651d6922f62fab9b" & factor == "control"),
         !(subject == "672cbd3e4db513bd8523d57f" & factor == "control")) %>%
  pull(subject) %>%
  unique()

subjects_removed_by_subsequent_filters <- setdiff(subjects_after_main_exclusion, final_subjects)
# 1 subject removed by subsequent filters

data <- data %>%
  filter(!subject %in% to_exclude,
         !is.na(factor),
         !(subject == "62d06d1b651d6922f62fab9b" & factor == "control"),
         !(subject == "672cbd3e4db513bd8523d57f" & factor == "control"))

length(unique(data$subject)) #206 Participants(
length(unique(data$subject[data$factor == 'experience'])) # 100 experience)
length(unique(data$subject[data$factor == 'control'])) # 106 control


#font_import(pattern = "Optima", prompt = FALSE)
loadfonts(device = "pdf")

# Halo effect ----
## do we see the effect? ----

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
    ),
    condition = factor(condition, c('attractive', 'average', 'unattractive'))
  )

halo_summary <- halo_data_choices %>%
  group_by(condition) %>%
  filter(condition != "average") %>%
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
  labs(title = "Average Persuasiveness by Condition", x = "Condition", y = "Average Persuasiveness") +
  theme_custom()+
  scale_fill_manual(values = exp_control)+
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
  scale_fill_manual(values = exp_control)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

# bayesian analysis
halo_analysis = brm(choice ~ condition + (1 | subject),
                    data = halo_data_choices %>% mutate(choice = scale(choice)),
                    prior = default_priors,
                    save_pars = save_pars(group = F),
                    cores = 4,
                    control = list(adapt_delta = 0.95))
summarise_draws(halo_analysis)
check_divergences(halo_analysis$fit)
summary(halo_analysis)
hdi(halo_analysis, effects = 'all')

## are people aware of the effect? ----

halo_data_introspection = halo_data %>% 
  filter(stimulus == "")

halo_data_introspection_experience = halo_data_introspection %>% 
  filter(factor == 'experience')

halo_effectsizes <- halo_data_choices %>%
  filter(factor == 'experience') %>%
  group_by(subject, condition) %>%
  summarize(mean_choice = mean(choice), .groups = 'drop') %>%
  pivot_wider(names_from = condition, values_from = mean_choice) %>%
  mutate(effect_size = attractive - unattractive) %>%
  select(-attractive, -unattractive)

halo_data_introspection_experience = halo_data_introspection_experience %>% 
  left_join(halo_effectsizes, by = 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0, c(T,F), c('Effect', 'No effect')),
         effect_size_range = range01(effect_size))

# dichotomous
halo_summary_introspection_experience = halo_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  )

ggplot(halo_summary_introspection_experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "Halo Introspection ratings", x = "Showed effect?", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = effect_no) +
  guides(fill = F)

halo_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                      halo_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating, center = F)),
                                                      save_pars = save_pars(group = F))
summary(halo_analysis_introspection_experience_midpoint)
hdi(halo_analysis_introspection_experience_midpoint)
summarise_draws(halo_analysis_introspection_experience_midpoint)
check_divergences(halo_analysis_introspection_experience_midpoint$fit)

halo_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect,
                                                          halo_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                          prior = default_priors,
                                                          save_pars = save_pars(group = F))
summary(halo_analysis_introspection_experience_dichotomized)
hdi(halo_analysis_introspection_experience_dichotomized)
summarise_draws(halo_analysis_introspection_experience_dichotomized)
check_divergences(halo_analysis_introspection_experience_dichotomized$fit)

# continuous
ggplot(halo_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(title = "Halo Introspection ratings", x = "Effect size", y = "Introspection rating")

halo_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size,
                                                        halo_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                      effect_size = scale(effect_size)),
                                                        prior = default_priors,
                                                        save_pars = save_pars(group = F))
summary(halo_analysis_introspection_experience_continuous)
hdi(halo_analysis_introspection_experience_continuous)
summarise_draws(halo_analysis_introspection_experience_continuous)
check_divergences(halo_analysis_introspection_experience_continuous$fit)

# Illusory truth effect ---------------------------------------------------
## do we see the effect? ----

illusory_data <- data %>%
  filter(task_name == "illusion of truth pt2") %>% 
  mutate(choice = as.numeric(choice),
         seen_before = factor(condition %in% c('true_old', 'false_old'), c(T,F), c('Seen', 'Unseen')),
         response_over_midpoint = choice > 50)

illusory_data_choices = illusory_data %>% 
  filter(stimulus != "")

illusory_summary = illusory_data_choices %>% 
  filter(factor == 'experience') %>% 
  group_by(seen_before) %>% 
  summarize(count = n(),
            mean_choice = mean(choice),
            se_choice = se(choice),
            mean_response_over_midpoint = mean(response_over_midpoint),
            se_response_over_midpoint = se.prop(response_over_midpoint))

ggplot(illusory_summary, aes(x = seen_before, y = mean_choice, fill = seen_before)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_choice - se_choice,
                    ymax = mean_choice + se_choice),
                width = 0.2) +
  theme_custom() +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +

  labs(title = "Illusory Truth Effect", x = "Seen Before?", y = "Truth Rating") +
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)

illusory_analysis = brm(choice ~ seen_before + (1 | subject),
                        prior = default_priors,
                        illusory_data_choices %>% mutate(choice = scale(choice)),
                        save_pars = save_pars(group = F),
                        cores = 4,
                        control = list(adapt_delta = 0.95))
summarise_draws(illusory_analysis)
check_divergences(illusory_analysis$fit)
summary(illusory_analysis)
hdi(illusory_analysis)

## are people aware of the effect? ----

illusory_data_introspection = illusory_data %>% 
  filter(stimulus == "")

illusory_data_introspection_experience = illusory_data_introspection %>% 
  filter(factor == 'experience')

illusory_effectsizes <- illusory_data_choices %>%
  filter(factor == 'experience') %>%
  group_by(subject, seen_before) %>%
  summarize(mean_choice = mean(choice), .groups = 'drop') %>%
  pivot_wider(names_from = seen_before, values_from = mean_choice) %>%
  mutate(effect_size = Seen - Unseen) %>%
  select(-Seen, -Unseen)

illusory_data_introspection_experience = illusory_data_introspection_experience %>% 
  left_join(illusory_effectsizes, by = 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0, c(T,F), c('Effect', 'No effect')),
         effect_size_range = range01(effect_size))

# dichotomous
illusory_summary_introspection_experience = illusory_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  )

ggplot(illusory_summary_introspection_experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "illusory Introspection ratings", x = "Showed effect?", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = effect_no) +
  guides(fill = F)

illusory_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                      illusory_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating, center = F)),
                                                      save_pars = save_pars(group = F))
summary(illusory_analysis_introspection_experience_midpoint)
hdi(illusory_analysis_introspection_experience_midpoint)
summarise_draws(illusory_analysis_introspection_experience_midpoint)
check_divergences(illusory_analysis_introspection_experience_midpoint$fit)

illusory_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect,
                                                              illusory_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                              prior = default_priors,
                                                              save_pars = save_pars(group = F))
summary(illusory_analysis_introspection_experience_dichotomized)
hdi(illusory_analysis_introspection_experience_dichotomized)
summarise_draws(illusory_analysis_introspection_experience_dichotomized)
check_divergences(illusory_analysis_introspection_experience_dichotomized$fit)

# continuous
ggplot(illusory_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(title = "illusory Introspection ratings", x = "Effect size", y = "Introspection rating")

illusory_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size,
                                                            illusory_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                              effect_size = scale(effect_size)),
                                                            prior = default_priors,
                                                            save_pars = save_pars(group = F))
summary(illusory_analysis_introspection_experience_continuous)
hdi(illusory_analysis_introspection_experience_continuous)
summarise_draws(illusory_analysis_introspection_experience_continuous)
check_divergences(illusory_analysis_introspection_experience_continuous$fit)


# Omission effect ----
## do we see the effect? ----

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
  scale_fill_manual(values = exp_control)+
  guides(fill = "none")+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

omission_analysis = brm(choice ~ condition,
                        omission_data %>% mutate(choice = scale(choice)),
                        prior = default_priors,
                        save_pars = save_pars(group = F))
summary(omission_analysis)
hdi(omission_analysis)
summarise_draws(omission_analysis)
check_divergences(omission_analysis$fit)

## are people aware of the effect? -----------------------------------------

omission_data_introspection = omission_data %>% 
  filter(stimulus == "")

omission_mean = mean(omission_data$choice[omission_data$factor == 'control'])
omission_data_introspection_experience <- omission_data_introspection %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = -choice,
         effect_size_range = range01(effect_size),
         showed_effect = factor(choice < omission_mean, c(T,F), c('Effect', 'No effect')))

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
  scale_fill_manual(values = effect_no)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(-50, 50))

omission_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                          omission_data_introspection_experience%>% mutate(introspect_rating = scale(introspect_rating, center = F)),
                                                          save_pars = save_pars(group = F))
summary(omission_analysis_introspection_experience_midpoint)
hdi(omission_analysis_introspection_experience_midpoint)
summarise_draws(omission_analysis_introspection_experience_midpoint)
check_divergences(omission_analysis_introspection_experience_midpoint$fit)

omission_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect,
                                                              omission_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                              prior = default_priors,
                                                              save_pars = save_pars(group = F))
summary(omission_analysis_introspection_experience_dichotomized)
hdi(omission_analysis_introspection_experience_dichotomized)
summarise_draws(omission_analysis_introspection_experience_dichotomized)
check_divergences(omission_analysis_introspection_experience_dichotomized$fit)

# continuous
ggplot(omission_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

omission_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size,
                                                            omission_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                              effect_size = scale(effect_size)),
                                                            prior = default_priors,
                                                            save_pars = save_pars(group = F))
summary(omission_analysis_introspection_experience_continuous)
hdi(omission_analysis_introspection_experience_continuous)
summarise_draws(omission_analysis_introspection_experience_continuous)
check_divergences(omission_analysis_introspection_experience_continuous$fit)

# Recognition heuristic ----
## do we see the effect? ----
recognition_data <- data %>%
  filter(task_name == "recognition: city") %>%
  filter(factor == "experience") %>%
  mutate(chose_recognizable = auxiliary_info1 == 'chose recognizable',
         chose_recognizable_num = as.numeric(chose_recognizable))

recognition_summary <- recognition_data %>%
  group_by(auxiliary_info1) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(
    total_n = sum(n),
    proportion = n / total_n,
    percent = proportion * 100,
    se_proportion = sqrt(proportion * (1 - proportion) / n),
    se_percent = se_proportion * 100
  )

ggplot(recognition_summary, aes(x = auxiliary_info1, y = percent, fill = auxiliary_info1)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = percent - se_percent, ymax = percent + se_percent),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  geom_text(aes(label = paste0("n=", n)),
            position = position_dodge(0.9),
            vjust = -0.5, # Adjust this if text overlaps with error bars too much
            family = "Optima",
            size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, max(recognition_summary$percent + recognition_summary$se_percent, na.rm = TRUE) * 1.1), expand = c(0,0)) +
  labs(
    title = "Recognition Effect for City Population",
    x = "Chose the Recognizable City",
    y = "Percentage"
  ) +
  theme_custom() +
  scale_fill_manual(values = exp_control) +
  guides(fill = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), # Added for completeness if theme_custom doesn't set it
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

recognition_analysis = brm(chose_recognizable_num ~ 1 + (1 | subject), 
                           recognition_data,
                           family = 'bernoulli',
                           save_pars = save_pars(group = F))
summary(recognition_analysis)
hdi(recognition_analysis)
summarise_draws(recognition_analysis)
check_divergences(recognition_analysis$fit)

## are people aware of the effect? -----------------------------------------
recognition_data_introspection <- data %>%
  filter(task_name == "recognition")

recognition_data_introspection_experience <- recognition_data_introspection %>%
  filter(factor == "experience")

recognition_effectsize = recognition_data %>% 
  group_by(subject) %>% 
  summarize(effect_size = mean(chose_recognizable)) %>% 
  select(subject, effect_size)

recognition_data_introspection_experience = recognition_data_introspection_experience %>% 
  left_join(recognition_effectsize, 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0.5, c(T,F), c('Effect', 'No effect')),
         effect_size_range = range01(effect_size))

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
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

recognition_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                             recognition_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating, center = F)),
                                                             save_pars = save_pars(group = F))
summary(recognition_analysis_introspection_experience_midpoint)
hdi(recognition_analysis_introspection_experience_midpoint)
summarise_draws(recognition_analysis_introspection_experience_midpoint)
check_divergences(recognition_analysis_introspection_experience_midpoint$fit)

recognition_analysis_introspection_experience_dichotomous = brm(introspect_rating ~ showed_effect,
                                                                recognition_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                                prior = default_priors,
                                                                save_pars = save_pars(group = F))
summary(recognition_analysis_introspection_experience_dichotomous)
hdi(recognition_analysis_introspection_experience_dichotomous)
summarise_draws(recognition_analysis_introspection_experience_dichotomous)
check_divergences(recognition_analysis_introspection_experience_dichotomous$fit)

# continuous
ggplot(recognition_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

recognition_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size,
                                                               recognition_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                                    effect_size = scale(effect_size)),
                                                               prior = default_priors,
                                                               save_pars = save_pars(group = F))
summary(recognition_analysis_introspection_experience_continuous)
hdi(recognition_analysis_introspection_experience_continuous)
summarise_draws(recognition_analysis_introspection_experience_continuous)
check_divergences(recognition_analysis_introspection_experience_continuous$fit)

# Reference price ----
## do we see the effect? ----

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
  scale_fill_manual(values = exp_control)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

reference_analysis = brm(choice_parsed ~ condition,
                         reference_data %>% mutate(choice_parsed = scale(choice_parsed)),
                         prior = default_priors,
                         save_pars = save_pars(group = F))
summary(reference_analysis)
hdi(reference_analysis)
summarise_draws(reference_analysis)
check_divergences(reference_analysis$fit)

## are people aware of the effect?----
reference_data_introspection = reference_data

reference_mean = mean(reference_data$choice_parsed[reference_data$factor == 'control'])
reference_data_introspection_experience = reference_data_introspection %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = choice_parsed,
         effect_size_range = range01(effect_size),
         showed_effect = factor(choice_parsed > reference_mean, c(T,F), c('Effect', 'No effect')))

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
  scale_fill_manual(values = effect_no)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

reference_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                           reference_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating, center = F)),
                                                           save_pars = save_pars(group = F))
summary(reference_analysis_introspection_experience_midpoint)
hdi(reference_analysis_introspection_experience_midpoint)
summarise_draws(reference_analysis_introspection_experience_midpoint)
check_divergences(reference_analysis_introspection_experience_midpoint$fit)

reference_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect,
                                                               reference_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                               prior = default_priors,
                                                               save_pars = save_pars(group = F))
summary(reference_analysis_introspection_experience_dichotomized)
hdi(reference_analysis_introspection_experience_dichotomized)
summarise_draws(reference_analysis_introspection_experience_dichotomized)
check_divergences(reference_analysis_introspection_experience_dichotomized$fit)

# continuous
ggplot(reference_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

reference_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size,
                                                             reference_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                                effect_size = scale(effect_size)),
                                                             prior = default_priors,
                                                             save_pars = save_pars(group = F))
summary(reference_analysis_introspection_experience_continuous)
hdi(reference_analysis_introspection_experience_continuous)
summarise_draws(reference_analysis_introspection_experience_continuous)
check_divergences(reference_analysis_introspection_experience_continuous$fit)

# Representativeness ----
## do we see the effect? ----

representativeness_data <- data %>%
  filter(task_name == "rep") %>%
  mutate(choice = as.numeric(choice))

representativeness_summary <- representativeness_data %>%
  group_by(condition) %>%
  mutate(condition = factor(condition, levels = c("Factor-Included", "Factor-Excluded"), labels = c("description", "no description"))) %>%
  summarize(
    mean_choice = mean(choice),
    se_choice = se(choice),
    count = n()
  )

ggplot(representativeness_summary, aes(x = condition, y = mean_choice, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_choice - se_choice, ymax = mean_choice + se_choice), width = 0.2) +
  labs(title = "Representativeness Heuristic", x = "Condition", y = "average likelihood of engineer") +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom() +
  scale_fill_manual(values = exp_control)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

representativeness_analysis = brm(choice ~ condition,
                                  representativeness_data %>% mutate(choice = scale(choice)),
                                  prior = default_priors,
                                  save_pars = save_pars(group = F))
summary(representativeness_analysis)
hdi(representativeness_analysis)
summarise_draws(representativeness_analysis)
check_divergences(representativeness_analysis$fit)

## are people aware of the effect? -----------------------------------------
representativeness_data_introspection = representativeness_data

representativeness_mean = mean(representativeness_data$choice[representativeness_data$factor == 'control'])
representativeness_data_introspection_experience = representativeness_data_introspection %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = choice,
         effect_size_range = range01(effect_size),
         showed_effect = factor(choice > representativeness_mean, c(T,F), c('Effect', 'No effect')))

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
  scale_fill_manual(values = effect_no)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))

representativeness_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1,
                                                                    representativeness_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating, center = F)),
                                                                    save_pars = save_pars(group = F))
summary(representativeness_analysis_introspection_experience_midpoint)
hdi(representativeness_analysis_introspection_experience_midpoint)
summarise_draws(representativeness_analysis_introspection_experience_midpoint)
check_divergences(representativeness_analysis_introspection_experience_midpoint$fit)

representativeness_analysis_introspection_experience_dichotomized = brm(introspect_rating ~ showed_effect,
                                                                        representativeness_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                                        prior = default_priors,
                                                                        save_pars = save_pars(group = F))
summary(representativeness_analysis_introspection_experience_dichotomized)
hdi(representativeness_analysis_introspection_experience_dichotomized)
summarise_draws(representativeness_analysis_introspection_experience_dichotomized)
check_divergences(representativeness_analysis_introspection_experience_dichotomized$fit)

# continuous
ggplot(representativeness_data_introspection_experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_custom() +
  labs(x = 'Choice', y = 'Influence rating')

representativeness_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size,
                                                                      representativeness_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                                                  effect_size = scale(effect_size)),
                                                                      prior = default_priors,
                                                                      save_pars = save_pars(group = F))
summary(representativeness_analysis_introspection_experience_continuous)
hdi(representativeness_analysis_introspection_experience_continuous)
summarise_draws(representativeness_analysis_introspection_experience_continuous)
check_divergences(representativeness_analysis_introspection_experience_continuous$fit)

# Aggregating across all tasks ---------------------------------------------------------------

all_list_introspection_experience = list(halo_data_introspection_experience,
                                         illusory_data_introspection_experience,
                                         omission_data_introspection_experience,
                                         recognition_data_introspection_experience,
                                         reference_data_introspection_experience,
                                         representativeness_data_introspection_experience)

all_data_introspection_experience = all_list_introspection_experience[[1]] %>% 
  select(subject, task_name, introspect_rating, effect_size, effect_size_range, showed_effect)
for (i in 2:length(all_list_introspection_experience)) {
  all_data_introspection_experience = all_data_introspection_experience %>% 
    rbind(all_list_introspection_experience[[i]] %>% 
            select(subject, task_name, introspect_rating, effect_size, effect_size_range, showed_effect))
}

test = all_data_introspection_experience %>% 
  group_by(subject) %>% 
  summarize(introspect_rating = mean(introspect_rating)) %>% 
  group_by() %>% 
  summarize(introspect_rating.m = mean(introspect_rating),
            introspect_rating.se = se(introspect_rating))
ggplot(test, aes(x=0, y = introspect_rating.m)) +
  geom_point(color = 'red', size = 3) +
  geom_errorbar(aes(ymin = introspect_rating.m - introspect_rating.se,
                    ymax = introspect_rating.m + introspect_rating.se),
                width = 0.2, color = 'red') +
  geom_jitter(color = 'gray', alpha = 0.1,
              mapping = aes(y = introspect_rating),
              data = all_data_introspection_experience,
              width = .1, height = 0) +
  theme_black() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(-50, 50)) +
  labs(x = '', y = 'Self-reported bias') +
  geom_hline(yintercept = 0, color = 'white', linetype = 'dashed')

## dichotomous
all_summary_introspection_experience = all_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating),
            se_introspect_rating = se(introspect_rating))

ggplot(all_summary_introspection_experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "", x = "Influenced by heuristic?", y = "Influence rating") +
  theme_custom() +
  scale_fill_manual(values = effect_no) +
  scale_x_discrete(labels = c('Yes', 'No')) +
  guides(fill = "none") +
  scale_y_continuous(limits = c(-50, 50))

  ggplot(all_summary_introspection_experience,
         aes(x = showed_effect, y = mean_introspect_rating, color = showed_effect)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "", x = "", y = "Self-reported bias") +
  theme_black() +
  scale_fill_manual(values = effect_no) +
  scale_x_discrete(labels = c('Yes', 'No')) +
  guides(color = "none") +
  scale_y_continuous(limits = c(-50, 50)) +
  geom_jitter(color = 'gray', alpha = 0.1,
              mapping = aes(y = introspect_rating),
              data = all_data_introspection_experience %>% 
                filter(!is.na(showed_effect)),
              width = .1, height = 0) +
  geom_hline(yintercept = 0, color = 'white', linetype = 'dashed')

all_analysis_introspection_experience_midpoint = brm(introspect_rating ~ 1 + (1 | subject) + (1 | task_name),
                                                     all_data_introspection_experience,
                                                     save_pars = save_pars(group = F))
summarise_draws(all_analysis_introspection_experience_midpoint)
check_divergences(all_analysis_introspection_experience_midpoint$fit)
summary(all_analysis_introspection_experience_midpoint)
hdi(all_analysis_introspection_experience_midpoint)

all_analysis_introspection_experience_dichotomous = brm(introspect_rating ~ showed_effect + (showed_effect || subject) + (showed_effect || task_name),
                                                        all_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                     showed_effect = relevel(showed_effect, ref = 'No effect')),
                                                        prior = default_priors,
                                                        save_pars = save_pars(group = F),
                                                        cores = 4,
                                                        control = list(adapt_delta = 0.95))
summarise_draws(all_analysis_introspection_experience_dichotomous)
check_divergences(all_analysis_introspection_experience_dichotomous$fit)
summary(all_analysis_introspection_experience_dichotomous)
hdi(all_analysis_introspection_experience_dichotomous)

## continuous
ggplot(all_data_introspection_experience,
       aes(x = effect_size_range, y = introspect_rating)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm') +
  theme_custom() +
  labs(x = 'Influence magnitude', 
       y = 'Influence rating')

ggplot(all_data_introspection_experience,
       aes(x = effect_size_range, y = introspect_rating)) +
  geom_point(alpha=0.5, color = 'white') +
  geom_smooth(method='lm') +
  theme_black() +
  labs(x = '\nObserved\nbias magnitude', 
       y = 'Self-reported bias')

all_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size_range + (effect_size_range | subject) + (effect_size_range | task_name),
                                                       all_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                    effect_size_range = scale(effect_size_range)),
                                                       prior = default_priors,
                                                       save_pars = save_pars(group = F),
                                                       cores = 4,
                                                       control = list(adapt_delta = 0.95))
summarise_draws(all_analysis_introspection_experience_continuous)
check_divergences(all_analysis_introspection_experience_continuous$fit)
summary(all_analysis_introspection_experience_continuous)
hdi(all_analysis_introspection_experience_continuous)

## continuous, standardized within
all_data_introspection_experience = all_data_introspection_experience %>% 
  group_by(subject) %>% 
  mutate(effect_size_range_within = scale(effect_size_range),
         introspect_rating_within = scale(introspect_rating)) %>% 
  ungroup()

ggplot(all_data_introspection_experience,
       aes(x = effect_size_range_within, y = introspect_rating_within)) +
  geom_point(alpha=0.8) +
  geom_smooth(method='lm') +
  theme_custom()

all_analysis_introspection_experience_continuous_within = brm(introspect_rating_within ~ effect_size_range_within + (1 | subject) + (effect_size_range_within | task_name),
                                                              all_data_introspection_experience %>% mutate(introspect_rating_within = scale(introspect_rating_within),
                                                                                                           effect_size_range_within = scale(effect_size_range_within)),
                                                               prior = default_priors,
                                                               save_pars = save_pars(group = F),
                                                               cores = 4,
                                                               control = list(adapt_delta = 0.95))
summarise_draws(all_analysis_introspection_experience_continuous_within)
check_divergences(all_analysis_introspection_experience_continuous_within$fit)
summary(all_analysis_introspection_experience_continuous_within)
hdi(all_analysis_introspection_experience_continuous_within)

## By subject
all_bysubject_introspection_experience = all_data_introspection_experience %>%
  group_by(subject) %>% 
  summarize(subject_cor = cor(effect_size_range, introspect_rating))
ggplot(all_bysubject_introspection_experience, aes(x = subject_cor)) +
  geom_histogram(color = 'black') +
  theme_custom() +
  labs(x = 'Participant-level correlation between\ninfluence ratings and influence magnitudes',
       y = 'Number of subjects') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T), color = 'red') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) - se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) + se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  scale_y_continuous(labels = c(), expand = expansion(mult = c(0, 0.05)))

theme_update(strip.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             plot.background = element_blank(),
             axis.text=element_text(size=18, colour = "black"),
             axis.title=element_text(size=24, face = "bold"),
             axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"),
             legend.text = element_text(size = 18),
             plot.title = element_text(size = 26, face = "bold", vjust = 1),
             panel.margin = unit(1.0, "lines"), 
             plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
             axis.line = element_line(colour = "black", size = 2),
             axis.ticks = element_line(color = 'black', size = 3),
             axis.ticks.length = unit(.25, 'cm')
)
theme_black = function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = 12, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = 12, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = 18, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = 18, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, color = "white"),  
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

ggplot(all_bysubject_introspection_experience, aes(x = subject_cor)) +
  geom_histogram(color = 'white') +
  theme_black() +
  labs(x = 'Participant-level correlation between\nobserved and self-reported bias magnitudes',
       y = 'Number of subjects') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T), color = 'red') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) - se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) + se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  scale_y_continuous(labels = c(), expand = expansion(mult = c(0, 0.05)))
get.ci = function(x) {return(c(mean(x,na.rm = T) - 1.96*se(x), mean(x, na.rm = T), mean(x, na.rm = T) + 1.96*se(x)))}
get.ci(all_bysubject_introspection_experience$subject_cor)

# Save image --------------------------------------------------------------
# for use in combined analysis
all_data_introspection_experience_pilot2 = all_data_introspection_experience
save(all_data_introspection_experience_pilot2, file = 'pilot2_alltasks.rdata')

# save all analyses
save.image('pilot2_output.rdata')
