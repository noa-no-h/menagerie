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

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
dodge <- position_dodge(width=0.9)

default_priors <- set_prior("normal(0,1)", class = 'b')

# color palettes

exp_control <- c("#F37121", "#4793AF")
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

# Load data ---------------------------------------------------------------

df <- read.csv('pilot1_data.csv') %>%
  arrange(subject, task_name) %>% 
  mutate(factor = factor(factor, c("Factor-Included", "Factor-Excluded"), c("experience", "control")),
         introspect_rating = introspect_rating - 10)

demo <- read.csv('pilot1_demographics.csv') %>%
  arrange(subject) %>% 
  mutate(total_min = total_time / 60000)

df <- df %>% mutate_if(is.character, ~na_if(., ""))
demo <- demo %>% mutate_if(is.character, ~na_if(., ""))

length(unique(demo$subject)) # 606 Participants

## Exclusion 

# subjects that glitched
glitched <- c("A3EQAA13A1LMNY", 'ABICECSTT3MWF', 'A256QIR5XUIP8K', 'A1LJT9OW9UR4GF')

# Attention Check 1
failed.attn1 <- df %>%
  filter(task_name == 'attention check 2', auxiliary_info1 == 'Failure') %>% 
  pull(subject)

# Attention Check 2
failed.attn2 <- df %>%
  filter(task_name == 'attention check 3', auxiliary_info1 == 'Incorrect') %>% 
  pull(subject)

naExcluded <- df %>%
  group_by(subject) %>%
  summarize(all_factors_na = all(is.na(factor)))%>%
  filter(all_factors_na)

# exclude subjects who restarted & took it more than once
wrong.trial.num = df %>% group_by(subject) %>%
  summarize(numTrials = n()) %>% 
  filter(numTrials > 76) %>% 
  pull(subject)

length(unique(demo$subject)) # 606

nofactor = df %>% 
  filter(subject %in% demo$subject,
        is.na(factor)) %>% 
  pull(subject)


df <- df %>%
  filter(subject %in% demo$subject,
         !is.na(factor),
         !(subject %in% glitched),
         !(subject %in% failed.attn1),
         !(subject %in% failed.attn2),
         !(subject %in% wrong.trial.num)) 

initial_subjects <- unique(demo$subject)
excluded_subjects_list <- unique(c(glitched, failed.attn1, failed.attn2, wrong.trial.num, naExcluded))
subjects_removed_from_initial_606 <- intersect(initial_subjects, excluded_subjects_list)
num_subjects_removed_from_initial_606 <- length(subjects_removed_from_initial_606)
print(num_subjects_removed_from_initial_606) #88 excluded

length(unique(df$subject)) #518 Participants(
length(unique(df$subject[df$factor == 'experience'])) # 277 experience)
length(unique(df$subject[df$factor == 'control'])) # 241 control

# Ensure df is in the state you expect
# str(df) # To check structure
# head(df) # To look at a few rows

total_unique_subjects_val <- unique(df$subject)
L_total <- length(total_unique_subjects_val)
print(paste("Total unique subjects (L_total):", L_total)) # Should be 518

exp_subjects_val <- unique(df$subject[df$factor == 'experience'])
L_exp <- length(exp_subjects_val)
print(paste("Experience unique subjects (L_exp):", L_exp)) # Should be 278

ctrl_subjects_val <- unique(df$subject[df$factor == 'control'])
L_ctrl <- length(ctrl_subjects_val)
print(paste("Control unique subjects (L_ctrl):", L_ctrl)) # Should be 242

intersect_subjects_val <- intersect(exp_subjects_val, ctrl_subjects_val)
L_intersect <- length(intersect_subjects_val)
V_intersect <- intersect_subjects_val
print(paste("Intersect length (L_intersect):", L_intersect)) # Should be 1
print("Intersect value (V_intersect):")
print(V_intersect) # Should be NA

print(paste("Number of rows with NA in df$factor:", sum(is.na(df$factor))))

# See which subjects (from your 518) have an NA factor for at least one row
# These are the subjects contributing to the "factor-induced NA" in the group lists
subjects_with_na_factor <- unique(df$subject[is.na(df$factor)])
print("Subjects (from your 518) that have NA in df$factor for at least one of their rows:")
print(subjects_with_na_factor)
print(paste("Count of such subjects:", length(subjects_with_na_factor)))


df$effect.size = NA
df$effect.size.fac = NA

# Anchoring ----------------------------------------------------

## do people show the effect?----
df.anchor = df %>%
  filter(task_name == 'anchoring',
         condition != 'High Anchor') %>% # we only ended up using the low anchor version of the antarctic question for the final study
  mutate(condition = factor(condition),
         choice = as.numeric(choice),
         distance.from.anchor = abs(-45 - choice))

df.anchor.choices = df.anchor %>% 
  filter(!is.na(choice),
         stimulus == 'Antarctic Temperature')

ggplot(df.anchor.choices, aes(x = choice)) +
  geom_histogram() +
  facet_wrap(~condition)

summary.anchor = df.anchor.choices %>%
  group_by(condition) %>%
  summarize(choice.m = mean(choice), choice.se = se(choice),
            distance.from.anchor.m = mean(distance.from.anchor),
            count = n(),
            distance.from.anchor.se = se(distance.from.anchor))

ggplot(summary.anchor, aes(x = condition, y = choice.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Condition") +
  theme_custom()

ggplot(summary.anchor, aes(x = condition, y = distance.from.anchor.m, fill = condition)) +
  geom_col() + 
  geom_errorbar(aes(ymin = distance.from.anchor.m - distance.from.anchor.se,
                    ymax = distance.from.anchor.m + distance.from.anchor.se),
                width = .2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(title = "Anchor Effect",x = "Condition", y = "distance from anchor") +
   theme_custom()+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.anchor = brm(distance.from.anchor ~ condition,
                       data = df.anchor.choices %>%
                         mutate(distance.from.anchor = scale(distance.from.anchor),
                               condition = relevel(condition, ref = "No Anchor")),
                       prior = default_priors,
                       save_pars = save_pars(group = F))
summarise_draws(analysis.anchor)
check_divergences(analysis.anchor$fit)
summary(analysis.anchor)
hdi(analysis.anchor)

## are people aware of the effect? ----
df.anchor.intro <- df.anchor %>% filter(!is.na(introspect_rating))

df.anchor.intro.experience = df.anchor.intro %>% 
  filter(factor == 'experience')

anchor.mean.control.response = mean(df.anchor.choices$distance.from.anchor[df.anchor.choices$factor == 'control'])

df.anchor.effectsizes = df.anchor.choices %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = -distance.from.anchor,
         effect_size_range = range01(effect_size),
         showed_effect = factor(distance.from.anchor < anchor.mean.control.response, c(T,F), c('Effect', 'No effect'))) %>% 
  select(subject, effect_size, effect_size_range, showed_effect)

df.anchor.intro.experience = df.anchor.intro.experience %>% 
  left_join(df.anchor.effectsizes, by = 'subject')

# dichotomous
summary.anchor.intro.experience = df.anchor.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  ) %>% 
  filter(!is.na(showed_effect))

ggplot(summary.anchor.intro.experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect?", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = effect_no)+
  guides(fill=F)

analysis.anchor.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                    df.anchor.intro.experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                    prior = default_priors,
                                                    save_pars = save_pars(group = F))
summarise_draws(analysis.anchor.intro.experience.dichotomized)
check_divergences(analysis.anchor.intro.experience.dichotomized$fit)
summary(analysis.anchor.intro.experience.dichotomized)
hdi(analysis.anchor.intro.experience.dichotomized)

# continuous
ggplot(df.anchor.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.anchor.intro.experience.continuous = brm(introspect_rating ~ effect_size,
                                                  df.anchor.intro.experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                        effect_size = scale(effect_size)),
                                                  prior = default_priors,
                                                  save_pars = save_pars(group = F))
summarise_draws(analysis.anchor.intro.experience.continuous)
check_divergences(analysis.anchor.intro.experience.continuous$fit)
summary(analysis.anchor.intro.experience.continuous)
hdi(analysis.anchor.intro.experience.continuous)

# Availability -------------------------------------------------
## do people show the effect? ----
df.avail = df %>%
  filter(task_name == 'availability'#,
         #familiarity == 'No'
         ) %>% 
  mutate(choice.binary = choice == 'List 1') %>%
  group_by(condition)

df.avail.summarized = df.avail %>%
  summarize(
    n = n(),
    p = mean(choice.binary),
    se = sqrt(p * (1 - p) / n),
    lower = p - se,
    upper = p + se,
    percentage = p * 100,
    percentageLower = lower * 100,
    percentageUpper = upper * 100
  )

ggplot(df.avail.summarized, aes(x = condition, y = percentage, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = percentageLower, ymax = percentageUpper),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  scale_fill_manual(
    values = c("#F37121", "#4793AF"), # Assuming two conditions, adjust if needed
    guide = "none"
  ) +
  geom_text(aes(label = paste0("n=", n)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(
    title = "Availability Heuristic",
    y = "Percentage who Chose 'List 1'",
    x = "Condition"
  ) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) +
  theme_custom() 

analysis.avail = brm(choice.binary ~ condition,
                     data = df.avail,
                     family = 'bernoulli',
                     prior = default_priors,
                     save_pars = save_pars(group = F))
summarise_draws(analysis.avail)
check_divergences(analysis.avail$fit)
summary(analysis.avail)
hdi(analysis.avail)

## are people aware of the effect? ----

df.avail.intro.experience = df.avail %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = choice.binary,
         effect_size_std = scale(effect_size), effect_size_range = range01(effect_size),
         showed_effect = factor(choice.binary, c(T,F), c('Effect', 'No effect')))

# dichotomized
summary.avail.intro.experience <- df.avail.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary.avail.intro.experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = effect_no)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.avail.intro.experience = brm(introspect_rating ~ showed_effect,
                                     df.avail.intro.experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                     prior = default_priors,
                                     save_pars = save_pars(group = F))
summarise_draws(analysis.avail.intro.experience)
check_divergences(analysis.avail.intro.experience$fit)
summary(analysis.avail.intro.experience)
hdi(analysis.avail.intro.experience)



# Abnormal selection in causal inference -------------------------------------------------------------
## do people show the effect? ----
df.cause = df %>% filter(task_name == 'causal inference') %>%
  mutate(choice = as.numeric(choice))

summary.cause = df.cause %>%
  group_by(condition) %>%
  summarize(choice.m = mean(choice),
            choice.se = se(choice),
            count = n())

ggplot(summary.cause, aes(x = condition, y = choice.m, fill = condition)) +
  geom_col() + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  labs(title = "Abnormal Selection in Causal Inference", x = "Condition", y = "average causality rating") +
  theme_custom() +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)

analysis.cause = brm(choice ~ condition,
                     df.cause,
                     prior = default_priors,
                     save_pars = save_pars(group = F))
summarise_draws(analysis.cause)
check_divergences(analysis.cause$fit)
summary(analysis.cause)
hdi(analysis.cause)

## are people aware of the effect? ----
df.cause.intro <- df.cause %>% filter(!is.na(introspect_rating))

cause.mean.control.response = mean(df.cause$choice[df.cause$factor == 'control'])

df.cause.intro.experience = df.cause %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = choice,
         effect_size_std = scale(effect_size), effect_size_range = range01(effect_size),
         showed_effect = factor(choice > cause.mean.control.response, c(T,F), c('Effect', 'No effect')))

# dichotomized
summary.cause.intro.experience <- df.cause.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary.cause.intro.experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = effect_no)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.cause.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                      df.cause.intro.experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                      prior = default_priors,
                                      save_pars = save_pars(group = F))
summarise_draws(analysis.cause.intro.experience.dichotomized)
check_divergences(analysis.cause.intro.experience.dichotomized$fit)
summary(analysis.cause.intro.experience.dichotomized)
hdi(analysis.cause.intro.experience.dichotomized)

# continuous
ggplot(df.cause.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.cause.intro.experience.continuous = brm(introspect_rating ~ effect_size, 
                                                 df.cause.intro.experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                      effect_size = scale(effect_size)),
                                                 prior = default_priors,
                                                 save_pars = save_pars(group = F))
summary(analysis.cause.intro.experience.continuous)
hdi(analysis.cause.intro.experience.continuous)

# Decoy Effect -------------------------------------------------------------
## do people show the effect? ----
df.decoy = df %>%
  filter(task_name == 'decoy effect') %>% 
  mutate(choice.target = choice == 'Brand N (Target)')

summary.decoy = df.decoy %>% 
  group_by(factor) %>% 
  summarize(choice.target.m = mean(choice.target),
            choice.target.se = se.prop(choice.target),
            count = n())

ggplot(summary.decoy, aes(x = factor, y = choice.target.m, fill=factor)) +
  geom_col() +
  geom_errorbar(aes(ymin = choice.target.m - choice.target.se,
                    ymax = choice.target.m + choice.target.se),
                width = 0.2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  
  theme_custom()+ 
  labs(title = "Decoy Effect", x = "Condition", y = "Proportion Chose Brand N (Target)")+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)

analysis.decoy = brm(choice.target ~ condition,
                     df.decoy,
                     prior = default_priors,
                     family = 'bernoulli')
summarise_draws(analysis.decoy)
check_divergences(analysis.decoy$fit)
summary(analysis.decoy)
hdi(analysis.decoy)

## are people aware of the effect? ----
df.decoy.intro <- df.decoy %>%
  filter(!is.na(introspect_rating))

df.decoy.intro.experience = df.decoy %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = choice.target,
         effect_size_std = scale(effect_size), effect_size_range = range01(effect_size),
         showed_effect = factor(choice.target, c(T,F), c('Effect', 'No effect')))

# dichotomized
summary.decoy.intro.experience <- df.decoy.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(
    mean_introspect_rating = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_introspect_rating = se(introspect_rating)
  )

ggplot(summary.decoy.intro.experience, aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect", y = "introspection rating") +
  theme_custom()+
  scale_fill_manual(values = effect_no)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.decoy.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                   df.decoy.intro.experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                   prior = default_priors,
                                                   save_pars = save_pars(group = F))
summarise_draws(analysis.decoy.intro.experience.dichotomized)
check_divergences(analysis.decoy.intro.experience.dichotomized$fit)
summary(analysis.decoy.intro.experience.dichotomized)
hdi(analysis.decoy.intro.experience.dichotomized)

# Belief ------------------------------------------------------------------
## do people show the effect? ----
df.belief <- df %>%
  filter(task_name == 'belief') %>% 
  mutate(choice.yes = choice == 'Yes')
df.belief.choices = df.belief %>%
  filter(factor == "experience", !is.na(choice), !is.na(condition))
  
summary.belief <- df.belief.choices %>%
  group_by(condition) %>% 
  summarize(choice.yes.m = mean(choice.yes),
            choice.yes.se = se.prop(choice.yes),
            count = n())

ggplot(summary.belief, aes(x = condition, y = choice.yes.m, fill = condition)) +
  geom_col() +
  geom_errorbar(aes(ymin = choice.yes.m - choice.yes.se,
                    ymax = choice.yes.m + choice.yes.se),
                width = 0.2) +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  theme_custom() +
  labs(title = "Belief Effect",x = "Condition", y = "Proportion Chose Yes, Valid")+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)

analysis.belief = brm(choice.yes ~ condition,
                      df.belief.choices,
                      prior = default_priors,
                      family = 'bernoulli')
summarise_draws(analysis.belief)
check_divergences(analysis.belief$fit)
summary(analysis.belief)
hdi(analysis.belief)

## are people aware of the effect? ----

df.belief.intro = df.belief %>%
  filter(!is.na(introspect_rating))

df.belief.intro.experience = df.belief.intro %>% 
  filter(factor == 'experience')

belief.effectsizes <- df.belief.choices %>%
  filter(factor == 'experience') %>%
  group_by(subject, condition) %>%
  summarize(mean_choice = mean(choice.yes), .groups = 'drop') %>%
  pivot_wider(names_from = condition, values_from = mean_choice) %>%
  mutate(effect_size = Believable - Unbelievable) %>%
  select(-Believable, -Unbelievable)

df.belief.intro.experience = df.belief.intro.experience %>% 
  left_join(belief.effectsizes, by = 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0, c(T,F), c('Effect', 'No effect')),
         effect_size_std = scale(effect_size), effect_size_range = range01(effect_size))

# dichotomized
summary.belief.intro.experience = df.belief.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  )

ggplot(summary.belief.intro.experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect?", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = effect_no) +
  guides(fill = F)

analysis.belief.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                 df.belief.intro.experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                 prior = default_priors,
                                                 save_pars = save_pars(group = F))
summarise_draws(analysis.belief.intro.experience.dichotomized)
check_divergences(analysis.belief.intro.experience.dichotomized$fit)
summary(analysis.belief.intro.experience.dichotomized)
hdi(analysis.belief.intro.experience.dichotomized)

# continuous
ggplot(df.belief.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.belief.intro.experience.continuous = brm(introspect_rating ~ effect_size,
                                                  df.belief.intro.experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                        effect_size = scale(effect_size)),
                                                  prior = default_priors,
                                                  save_pars = save_pars(group = F))
summarise_draws(analysis.belief.intro.experience.continuous)
check_divergences(analysis.belief.intro.experience.continuous$fit)
summary(analysis.belief.intro.experience.continuous)
hdi(analysis.belief.intro.experience.continuous)

# Mere exposure ------------------------------------------------------
## do people show the effect? ----
df.mee = df %>%
  filter(task_name == 'mere exposure') %>%
  mutate(choice = as.numeric(choice))

df.mee.choices <- df.mee %>%
  filter(factor == "experience", !is.na(choice)) %>% 
  mutate(condition = factor(condition, c(1,25), c('low', 'high')))

summary.mee = df.mee.choices %>%
  group_by(condition) %>%
  summarize(choice.m = mean(choice),
            choice.se = se(choice),
            count = n())

ggplot(summary.mee, aes(x = condition, y = choice.m, fill = condition)) +
  geom_col() + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  theme_custom() +
  geom_text(aes(label = paste0("n=", count)), 
            position = position_dodge(0.9), vjust = -0.5, 
            family = "Optima") +
  labs(title = "Mere Exposure Effect",x = "Number of repeats", y = "Mean Liking Rating")+
  scale_fill_manual(values = exp_control)+
  guides(fill = FALSE)

analysis.mee = brm(choice ~ condition + (condition | subject),
                   df.mee.choices %>% mutate(choice = scale(choice)),
                   prior = default_priors,
                   save_pars = save_pars(group = F),
                   cores = 4,
                   control = list(adapt_delta = 0.95))
summarise_draws(analysis.mee)
check_divergences(analysis.mee$fit)
summary(analysis.mee)
hdi(analysis.mee)

## are people aware of the effect? ---------------------------------------------------

df.mee.intro <- df.mee %>% filter(!is.na(introspect_rating))

df.mee.intro.experience = df.mee.intro %>% 
  filter(factor == 'experience')

mee.effectsizes <- df.mee.choices %>%
  filter(factor == 'experience') %>%
  group_by(subject, condition) %>%
  summarize(mean_choice = mean(choice), .groups = 'drop') %>%
  pivot_wider(names_from = condition, values_from = mean_choice) %>%
  mutate(effect_size = high - low) %>%
  select(-high, -low)

df.mee.intro.experience = df.mee.intro.experience %>% 
  left_join(mee.effectsizes, by = 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0, c(T,F), c('Effect', 'No effect')),
         effect_size_std = scale(effect_size), effect_size_range = range01(effect_size))

# dichotomized
summary.mee.intro.experience = df.mee.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  )

ggplot(summary.mee.intro.experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect?", y = "Introspection rating") +
  theme_custom() +
  scale_fill_manual(values = effect_no) +
  guides(fill = F)

analysis.mee.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                 df.mee.intro.experience %>% mutate(introspect_rating = scale(introspect_rating)),
                                                 prior = default_priors,
                                                 save_pars = save_pars(group = F))
summarise_draws(analysis.mee.intro.experience.dichotomized)
check_divergences(analysis.mee.intro.experience.dichotomized$fit)
summary(analysis.mee.intro.experience.dichotomized)
hdi(analysis.mee.intro.experience.dichotomized)

# continuous
ggplot(df.mee.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.mee.intro.experience.continuous = brm(introspect_rating ~ effect_size, 
                                               df.mee.intro.experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                  effect_size = scale(effect_size)),
                                               prior = default_priors,
                                               save_pars = save_pars(group = F))
summarise_draws(analysis.mee.intro.experience.continuous)
check_divergences(analysis.mee.intro.experience.continuous$fit)
summary(analysis.mee.intro.experience.continuous)
hdi(analysis.mee.intro.experience.continuous)

# Aggregating across all tasks -----------------------------------------------------------

all_list_introspection_experience = list(df.anchor.intro.experience,
                                         df.avail.intro.experience,
                                         df.cause.intro.experience,
                                         df.decoy.intro.experience,
                                         df.belief.intro.experience,
                                         df.mee.intro.experience)

all_data_introspection_experience = all_list_introspection_experience[[1]] %>% 
  select(subject, task_name, introspect_rating, effect_size, effect_size_range, showed_effect)
for (i in 2:length(all_list_introspection_experience)) {
  all_data_introspection_experience = all_data_introspection_experience %>% 
    rbind(all_list_introspection_experience[[i]] %>% 
            select(subject, task_name, introspect_rating, effect_size, effect_size_range, showed_effect))
}

all_data_introspection_experience = all_data_introspection_experience %>%
  mutate(introspect_rating = (introspect_rating + 10) / 10)

mean(all_data_introspection_experience$introspect_rating)
test = all_data_introspection_experience %>% 
  group_by(subject) %>% 
  summarize(introspect_rating = mean(introspect_rating)) %>% 
  group_by() %>% 
  summarize(introspect_rating.m = mean(introspect_rating),
            introspect_rating.se = se(introspect_rating))
ggplot(test, aes(x=0, y = introspect_rating.m)) +
  geom_point(color = 'red') +
  geom_errorbar(aes(ymin = introspect_rating.m - introspect_rating.se,
                    ymax = introspect_rating.m + introspect_rating.se),
                width = 0.2, color = 'red') +
  geom_jitter(color = 'gray', alpha = 0.1,
             mapping = aes(y = introspect_rating),
             data = all_data_introspection_experience,
             width = .1, height = 0) +
  theme_black() +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(1,9), breaks = c(1,5,9)) +
  labs(x = '', y = 'Self-reported bias')

## dichotomous
all_summary_introspection_experience = all_data_introspection_experience %>% 
  filter(!is.na(showed_effect)) %>% 
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
  guides(fill = "none")

ggplot(all_summary_introspection_experience,
       aes(x = showed_effect, y = mean_introspect_rating, color = showed_effect)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "", x = "Influenced by heuristic?", y = "Influence rating") +
  scale_fill_manual(values = effect_no) +
  scale_x_discrete(labels = c('Yes', 'No')) +
  theme_black() +
  geom_jitter(color = 'gray', alpha = 0.1,
              mapping = aes(y = introspect_rating),
              data = all_data_introspection_experience %>% 
                filter(!is.na(showed_effect)),
              width = .1, height = 0) +
  scale_y_continuous(limits = c(1,9), breaks = c(1,5,9)) +
  labs(x = '', y = 'Self-reported bias') +
  guides(color = "none")

all_analysis_introspection_experience_dichotomous = brm(introspect_rating ~ showed_effect + (1 | subject) + (1 | task_name),
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
  labs(x = 'Influence magnitude', y = 'Influence rating')

ggplot(all_data_introspection_experience,
       aes(x = effect_size_range, y = introspect_rating)) +
  geom_point(alpha=0.5, color = 'white') +
  geom_smooth(method='lm') +
  theme_black() +
  labs(x = '\nObserved bias magnitude', y = 'Self-reported bias') +
  scale_y_continuous(limits = c(1,9), breaks = c(1,5,9))


all_analysis_introspection_experience_continuous = brm(introspect_rating ~ effect_size_range + (1 | subject) + (effect_size_range | task_name),
                                                             all_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
                                                                                                          effect_size_range = scale(effect_size_range)),
                                                             prior = default_priors,
                                                             save_pars = save_pars(group = F),
                                                             cores = 4,
                                                             control = list(adapt_delta = 0.95))
summarise_draws(all_analysis_introspection_experience_continuous)
check_divergences(all_analysis_introspection_experience_dichotomous$fit)
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
  geom_point(alpha=0.5) +
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
ggplot(all_bysubject_introspection_experience, aes(x = subject_cor)) +
  geom_histogram(color = 'white') +
  theme_black() +
  labs(x = 'Participant-level correlation between\nself-reported and observed bias magnitudes',
       y = 'Number of subjects') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T), color = 'red') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) - se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) + se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  scale_y_continuous(labels = c(), expand = expansion(mult = c(0, 0.05)))

# Save image --------------------------------------------------------------
# for use in pilot 4 analysis
all_data_introspection_experience_pilot1 = all_data_introspection_experience
save(all_data_introspection_experience_pilot1, file = 'pilot1_alltasks.rdata')

# save all analyses
save.image("pilot1_output1.rdata")



# save for observer ----
library(jsonlite)

df.anchor = df.anchor %>%
  filter(stimulus == "Antarctic Temperature")%>%
  filter(choice != "") %>%
  filter(auxiliary_info1 != "") 
  

task_data_list <- list(
  list(df.anchor, "anchor", list("subject", "choice", "auxiliary_info1")),
  list(df.avail, "availability", list("subject", "choice")),
  list(df.belief, "belief", list("subject", "choice", "stimulus")),
  list(df.cause, "cause", list("subject", "choice")),
  list(df.decoy, "decoy", list("subject", "choice")),
  list(df.mee, "mere_exposure", list("subject", "choice", "stimulus"))
)


for (task_data_info in task_data_list) {
  task_data = task_data_info[[1]]
  task_name = task_data_info[[2]]
  to_select = unlist(task_data_info[[3]])
  
  filtered_task_data = task_data %>%
    filter(factor == "experience") %>%
    select(all_of(to_select))
  
  db_json <- toJSON(filtered_task_data, pretty = TRUE)
  json_towrite = paste0(task_name, "_db = ", db_json, ";")
  write(json_towrite, paste0(task_name, "_db.js"))
  
}
