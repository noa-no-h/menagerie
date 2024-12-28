# Setup -------------------------------------------------------------------

if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'RColorBrewer', 'extrafont',
              'this.path', 'brms', 'bayestestR')
p_load(char = pkg.names)

setwd(here())

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
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

# Load data ---------------------------------------------------------------

df <- read.csv('pilot1_data.csv') %>%
  arrange(subject, task_name) %>% 
  mutate(factor = factor(factor, c("Factor-Included", "Factor-Excluded"), c("experience", "prediction")))

demo <- read.csv('pilot1_demographics.csv') %>%
  arrange(subject) %>% 
  mutate(total_min = total_time / 60000)

df <- df %>% mutate_if(is.character, ~na_if(., ""))
demo <- demo %>% mutate_if(is.character, ~na_if(., ""))

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

# exclude subjects who restarted & took it more than once
wrong.trial.num = df %>% group_by(subject) %>%
  summarize(numTrials = n()) %>% 
  filter(numTrials > 76) %>% 
  pull(subject)

df <- df %>%
  filter(subject %in% demo$subject,
         !(subject %in% glitched),
         !(subject %in% failed.attn1),
         !(subject %in% failed.attn2),
         !(subject %in% wrong.trial.num)) 

length(unique(df$subject)) #518 Participants

df$effect.size = NA
df$effect.size.fac = NA

#* 1 Anchoring (between-subjects) ----------------------------------------------------

#** data preparation ----
df.anchor = df %>%
  filter(task_name == 'anchoring',
         condition != 'High Anchor') %>% # we only ended up using the low anchor version of the antarctic question for the final study
  mutate(condition = factor(condition),
         choice = as.numeric(choice),
         distance.from.anchor = abs(-45 - choice)) #%>% 
  #group_by(subject) %>% filter(any(familiarity == 'No'))

df.anchor.choices = df.anchor %>% 
  filter(!is.na(choice),
         stimulus == 'Antarctic Temperature')

#** data visualization ----
ggplot(df.anchor.choices, aes(x = choice)) +
  geom_histogram() +
  facet_wrap(~condition)

summary.anchor = df.anchor.choices %>%
  group_by(condition) %>%
  summarize(choice.m = mean(choice), choice.se = se(choice),
            distance.from.anchor.m = mean(distance.from.anchor),
            distance.from.anchor.se = se(distance.from.anchor))

ggplot(summary.anchor, aes(x = condition, y = choice.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Condition") +
  theme_custom()

ggplot(summary.anchor, aes(x = condition, y = distance.from.anchor.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = distance.from.anchor.m - distance.from.anchor.se,
                    ymax = distance.from.anchor.m + distance.from.anchor.se),
                width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Condition") +
  theme_custom()

analysis.anchor = brm(choice ~ condition,
                      data = df.anchor.choices %>%
                       mutate(choice = scale(choice),
                              condition = relevel(condition, ref = "No Anchor")),
                      save_pars = save_pars(group = F))
summary(analysis.anchor)
hdi(analysis.anchor) # key comparison is low anchor vs. no anchor; we dropped the high anchor for the real study

analysis.anchor2 = brm(distance.from.anchor ~ condition,
                       data = df.anchor.choices %>%
                         mutate(distance.from.anchor = scale(distance.from.anchor),
                               condition = relevel(condition, ref = "No Anchor")),
                       save_pars = save_pars(group = F))
summary(analysis.anchor2)
hdi(analysis.anchor2)

#** introspection ratings ----
df.anchor.intro <- df.anchor %>% filter(!is.na(introspect_rating))

## in experience condition
df.anchor.intro.experience = df.anchor.intro %>% 
  filter(factor == 'experience')

anchor.mean.prediction.response = mean(df.anchor.choices$choice[df.anchor.choices$factor == 'prediction'])
df.anchor.effectsizes = df.anchor.choices %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = -choice,
         effect_size2 = -distance.from.anchor,
         effect_size_range = range01(effect_size),
         showed_effect = factor(choice < anchor.mean.prediction.response, c(T,F), c('Effect', 'No effect'))) %>% 
  select(subject, effect_size, effect_size2, effect_size_range, showed_effect)

df.anchor.intro.experience = df.anchor.intro.experience %>% 
  left_join(df.anchor.effectsizes, by = 'subject')

# dichotomous
summary.anchor.intro.experience = df.anchor.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  )

ggplot(summary.anchor.intro.experience,
       aes(x = showed_effect, y = mean_introspect_rating)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect?", y = "Introspection rating") +
  theme_custom()

analysis.anchor.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect, df.anchor.intro.experience,
                                                          save_pars = save_pars(group = F))
summary(analysis.anchor.intro.experience.dichotomized)
hdi(analysis.anchor.intro.experience.dichotomized)

# continuous
ggplot(df.anchor.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.anchor.intro.experience.continuous = brm(introspect_rating ~ effect_size, df.anchor.intro.experience,
                                                        save_pars = save_pars(group = F))
summary(analysis.anchor.intro.experience.continuous)
hdi(analysis.anchor.intro.experience.continuous)

## across conditions
summary.anchor.intro.both <- df.anchor.intro %>% group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.anchor.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Condition", y = "Influence rating")

analysis.anchor.intro.both = brm(introspect_rating ~ factor,
                                       data = df.anchor.intro,
                                       save_pars = save_pars(group = F))
summary(analysis.anchor.intro.both)
hdi(analysis.anchor.intro.both)

#* 2 Availability (between-subjects) -------------------------------------------------
#** data preparation ----
df.avail = df %>%
  filter(task_name == 'availability'#,
         #familiarity == 'No'
         ) %>% 
  mutate(choice.binary = choice == 'List 1')

#** data visualization ----
ggplot(df.avail, aes(x = condition, fill = choice.binary)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  theme_custom()

analysis.avail = brm(choice.binary ~ condition,
                     data = df.avail,
                     family = 'bernoulli',
                     save_pars = save_pars(group = F))
summary(analysis.avail)
hdi(analysis.avail)

#** introspection ratings ----
df.avail.intro = df.avail

## in experience condition
df.avail.intro.experience = df.avail.intro %>% 
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
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.avail.intro.experience = brm(introspect_rating ~ showed_effect,
                                     df.avail.intro.experience,
                                     save_pars = save_pars(group = F))
summary(analysis.avail.intro.experience)
hdi(analysis.avail.intro.experience)

## across conditions
summary.avail.intro.both = df.avail.intro %>%
  group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.avail.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

analysis.avail.intro.both = brm(introspect_rating ~ factor,
                                data = df.avail,
                                save_pars = save_pars(group = F))
summary(analysis.avail.intro.both)
hdi(analysis.avail.intro.both)

#* 3 Causal Inference (between-subjects) -------------------------------------------------------------
#** data preparation ----
df.cause = df %>% filter(task_name == 'causal inference') %>%
  mutate(choice = as.numeric(choice))

#** data visualization ----
summary.cause = df.cause %>%
  group_by(condition) %>%
  summarize(choice.m = mean(choice), choice.se = se(choice))

ggplot(summary.cause, aes(x = condition, y = choice.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  labs(x = "Condition") +
  theme_custom()

analysis.cause = brm(choice ~ condition,
                     df.cause,
                     save_pars = save_pars(group = F))
hdi(analysis.cause)

#** introspection ratings ----
df.cause.intro <- df.cause %>% filter(!is.na(introspect_rating))

## in experience condition
df.cause.intro.experience = df.cause %>% 
  filter(factor == 'experience') %>% 
  mutate(effect_size = choice,
         effect_size_std = scale(effect_size), effect_size_range = range01(effect_size),
         showed_effect = factor(choice > 50, c(T,F), c('Effect', 'No effect')))

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
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.cause.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                      df.cause.intro.experience,
                                      save_pars = save_pars(group = F))
summary(analysis.cause.intro.experience.dichotomized)
hdi(analysis.cause.intro.experience.dichotomized)

# continuous
ggplot(df.cause.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.cause.intro.experience.continuous = brm(introspect_rating ~ effect_size, 
                                                 df.cause.intro.experience,
                                                 save_pars = save_pars(group = F))
summary(analysis.cause.intro.experience.continuous)
hdi(analysis.cause.intro.experience.continuous)

## across conditions

summary.cause.intro.both <- df.cause.intro %>% group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.cause.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  labs(x = "Test Version") +
  theme_custom()

analysis.cause.intro.both = brm(introspect_rating ~ factor,
                                data = df.cause,
                                save_pars = save_pars(group = F))
summary(analysis.cause.intro.both)
hdi(analysis.cause.intro.both)

#* 6 Decoy Effect -------------------------------------------------------------
#** data preparation ----
df.decoy = df %>%
  filter(task_name == 'decoy effect') %>% 
  mutate(choice.target = choice == 'Brand N (Target)')

#** data visualization ----
summary.decoy = df.decoy %>% 
  group_by(factor) %>% 
  summarize(choice.target.m = mean(choice.target),
            choice.target.se = se.prop(choice.target))
ggplot(summary.decoy, aes(x = factor, y = choice.target.m)) +
  geom_col() +
  geom_errorbar(aes(ymin = choice.target.m - choice.target.se,
                    ymax = choice.target.m + choice.target.se),
                width = 0.2) +
  theme_custom()

#** inferential statistics ----
analysis.decoy = brm(choice.target ~ condition,
                     df.decoy,
                     family = 'bernoulli')
summary(analysis.decoy)
hdi(analysis.decoy)

#** introspection ratings ----
df.decoy.intro <- df.decoy %>%
  filter(!is.na(introspect_rating))

## in experience condition

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
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.decoy.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                   df.decoy.intro.experience,
                                                   save_pars = save_pars(group = F))
summary(analysis.decoy.intro.experience.dichotomized)
hdi(analysis.decoy.intro.experience.dichotomized)

## across conditions

summary.decoy.intro.both <- df.decoy.intro %>% group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.decoy.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  labs(x = "Test Version") +
  theme_custom()

analysis.decoy.intro.both = brm(introspect_rating ~ factor,
                                data = df.decoy,
                                save_pars = save_pars(group = F))
summary(analysis.decoy.intro.both)
hdi(analysis.decoy.intro.both)

#* 7 Associative memory (within-subjects) ------------------------------------------------------
#** data preparation ----
df.mem = df %>% filter(task_name == 'associative memory') %>%
  mutate(choice.fac = factor(choice),
         choice.orig = choice == 'Original')
df.mem.choices <- df.mem %>%
  filter(factor == 'experience',
         auxiliary_info1 == 'New')

#** data visualization ----
summary.mem = df.mem.choices %>% 
  group_by(condition) %>% 
  summarize(choice.orig.m = mean(choice.orig),
            choice.orig.se = se.prop(choice.orig))
ggplot(summary.mem, aes(x = condition, y = choice.orig.m)) +
  geom_col() +
  geom_errorbar(aes(ymin = choice.orig.m - choice.orig.se,
                    ymax = choice.orig.m + choice.orig.se),
                width = 0.2) +
  theme_custom()
  
#** inferential statistics ----
analysis.mem = brm(choice.orig ~ condition,
                   df.mem.choices,
                   family = 'bernoulli')
summary(analysis.mem)
hdi(analysis.mem)

#** introspection ratings ----
df.mem.intro = df.mem %>% 
  filter(!is.na(introspect_rating))

## in experience condition

df.mem.intro.experience = df.mem.intro %>% 
  filter(factor == 'experience')

mem.effectsizes <- df.mem.choices %>%
  filter(factor == 'experience') %>%
  group_by(subject, condition) %>%
  summarize(mean_choice = mean(choice.orig), .groups = 'drop') %>%
  pivot_wider(names_from = condition, values_from = mean_choice) %>%
  mutate(effect_size = Sleep - NonSleep) %>%
  select(-Sleep, -NonSleep)

df.mem.intro.experience = df.mem.intro.experience %>% 
  left_join(mem.effectsizes, by = 'subject') %>% 
  mutate(showed_effect = factor(effect_size > 0, c(T,F), c('Effect', 'No effect')),
         effect_size_std = scale(effect_size), effect_size_range = range01(effect_size))

# dichotomized
summary.mem.intro.experience = df.mem.intro.experience %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating, na.rm = T),
            se_introspect_rating = se(introspect_rating),
  )

ggplot(summary.mem.intro.experience,
       aes(x = showed_effect, y = mean_introspect_rating)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect?", y = "Introspection rating") +
  theme_custom()

analysis.mem.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                 df.mem.intro.experience,
                                                 save_pars = save_pars(group = F))
summary(analysis.mem.intro.experience.dichotomized)
hdi(analysis.mem.intro.experience.dichotomized)

# continuous
ggplot(df.mem.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.mem.intro.experience.continuous = brm(introspect_rating ~ effect_size, df.mem.intro.experience,
                                                  save_pars = save_pars(group = F))
summary(analysis.mem.intro.experience.continuous)
hdi(analysis.mem.intro.experience.continuous)

## across conditions

summary.mem.intro.both = df.mem.intro %>% 
  group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.mem.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme_custom()


analysis.mem.intro.both = brm(introspect_rating ~ factor,
                                data = df.mem,
                                save_pars = save_pars(group = F))
summary(analysis.mem.intro.both)
hdi(analysis.mem.intro.both)


#* 8 Belief (within-subjects) ------------------------------------------------------------------
#** data preparation ----
df.belief <- df %>%
  filter(task_name == 'belief') %>% 
  mutate(choice.yes = choice == 'Yes')
df.belief.choices = df.belief %>%
  filter(factor == "experience", !is.na(choice), !is.na(condition))
  
#** data visualization ----
summary.belief <- df.belief.choices %>%
  group_by(condition) %>% 
  summarize(choice.yes.m = mean(choice.yes),
            choice.yes.se = se.prop(choice.yes))

ggplot(summary.belief, aes(x = condition, y = choice.yes.m)) +
  geom_col() +
  geom_errorbar(aes(ymin = choice.yes.m - choice.yes.se,
                    ymax = choice.yes.m + choice.yes.se),
                width = 0.2) +
  theme_custom()

#** inferential statistics ----
analysis.belief = brm(choice.yes ~ condition,
                      df.belief.choices,
                      family = 'bernoulli')
hdi(analysis.belief)

#** introspection ratings ----

df.belief.intro = df.belief %>%
  filter(!is.na(introspect_rating))

## in experience condition
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
       aes(x = showed_effect, y = mean_introspect_rating)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect?", y = "Introspection rating") +
  theme_custom()

analysis.belief.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                 df.belief.intro.experience,
                                                 save_pars = save_pars(group = F))
summary(analysis.belief.intro.experience.dichotomized)
hdi(analysis.belief.intro.experience.dichotomized)

# continuous
ggplot(df.belief.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.belief.intro.experience.continuous = brm(introspect_rating ~ effect_size, df.belief.intro.experience,
                                               save_pars = save_pars(group = F))
summary(analysis.belief.intro.experience.continuous)
hdi(analysis.belief.intro.experience.continuous)

## across conditions
summary.belief.intro.both = df.belief.intro %>% 
  group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.belief.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme_custom()

analysis.belief.intro.both = brm(introspect_rating ~ factor,
                                 df.belief.intro)
hdi(analysis.belief.intro.both)

#* 9 Mere exposure (within-subjects) ------------------------------------------------------
#** data preparation ----
df.mee = df %>%
  filter(task_name == 'mere exposure') %>%
  mutate(choice = as.numeric(choice))

df.mee.choices <- df.mee %>%
  filter(factor == "experience", !is.na(choice)) %>% 
  mutate(condition = factor(condition, c(1,25), c('low', 'high')))

#** data visualization (general) ----

summary.mee = df.mee.choices %>%
  group_by(condition) %>%
  summarize(choice.m = mean(choice), choice.se = se(choice))

ggplot(summary.mee, aes(x = condition, y = choice.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  theme_custom() +
  labs(x = "Condition", y = "Mean Liking Rating")

#** inferential statistics ----

analysis.mee = brm(choice ~ condition,
                   df.mee.choices)
hdi(analysis.mee)

#** introspection ratings ---------------------------------------------------

df.mee.intro <- df.mee %>% filter(!is.na(introspect_rating))

## in experience condition

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
       aes(x = showed_effect, y = mean_introspect_rating)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(x = "Showed effect?", y = "Introspection rating") +
  theme_custom()

analysis.mee.intro.experience.dichotomized = brm(introspect_rating ~ showed_effect,
                                                    df.mee.intro.experience,
                                                    save_pars = save_pars(group = F))
summary(analysis.mee.intro.experience.dichotomized)
hdi(analysis.mee.intro.experience.dichotomized)

# continuous
ggplot(df.mee.intro.experience, aes(x = effect_size, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_custom()+
  labs(x = "Effect size", y = "Introspection rating")

analysis.mee.intro.experience.continuous = brm(introspect_rating ~ effect_size, df.mee.intro.experience,
                                                  save_pars = save_pars(group = F))
summary(analysis.mee.intro.experience.continuous)
hdi(analysis.mee.intro.experience.continuous)


## across conditions

summary.mee.intro.both <- df.mee.intro %>%
  group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.mee.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme_custom()

analysis.mee.intro.both = brm(introspect_rating ~ factor,
                              df.mee.intro)
hdi(analysis.mee.intro.both)

# All tasks -----------------------------------------------------------

## in experience condition
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

# dichotomous
all_summary_introspection_experience = all_data_introspection_experience %>% 
  filter(!is.na(showed_effect)) %>% 
  group_by(showed_effect) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating),
            se_introspect_rating = se(introspect_rating))

ggplot(all_summary_introspection_experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "", x = "Showed effect?", y = "Influence rating") +
  theme_custom() +
  scale_fill_manual(values = effect_no) +
  guides(fill = "none")

all_analysis_introspection_experience_dichotomous = brm(introspect_rating ~ showed_effect + (showed_effect | subject) + (showed_effect | task_name),
                                                        all_data_introspection_experience,
                                                        save_pars = save_pars(group = F))
summary(all_analysis_introspection_experience_dichotomous)
hdi(all_analysis_introspection_experience_dichotomous)

# continuous
ggplot(all_data_introspection_experience,
       aes(x = effect_size_range, y = introspect_rating)) +
  geom_point(alpha=0.8) +
  geom_smooth(method='lm') +
  theme_custom()

ggplot(all_data_introspection_experience,
       aes(x = effect_size_range, y = introspect_rating)) +
  geom_point(alpha=0.8) +
  geom_smooth(method='lm') +
  theme_custom()

all_analysis_introspection_experience_continuous_range = brm(introspect_rating ~ effect_size_range + (effect_size_range | subject) + (1 | task_name),
                                                             all_data_introspection_experience,
                                                             save_pars = save_pars(group = F))
summary(all_analysis_introspection_experience_continuous_range)
hdi(all_analysis_introspection_experience_continuous_range)

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

## across conditions
all_list_introspection_both = list(df.anchor.intro,
                                   df.avail.intro,
                                   df.cause.intro,
                                   df.decoy.intro,
                                   df.belief.intro,
                                   df.mee.intro)

all_data_introspection_both = all_list_introspection_both[[1]] %>% 
  select(subject, task_name, factor, introspect_rating)
for (i in 2:length(all_list_introspection_both)) {
  all_data_introspection_both = all_data_introspection_both %>% 
    rbind(all_list_introspection_both[[i]] %>% 
            select(subject, task_name, factor, introspect_rating))
}

all_summary_introspection_both = all_data_introspection_both %>% 
  group_by(factor) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating),
            se_introspect_rating = se(introspect_rating))

ggplot(all_summary_introspection_both, aes(x = factor, y = mean_introspect_rating, fill = factor)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "", x = "Condition", y = "Influence rating") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

all_analysis_introspection_both = brm(introspect_rating ~ factor + (1 | subject) + (factor | task_name),
                                      all_data_introspection_both,
                                      save_pars = save_pars(group = F))
summary(all_analysis_introspection_both)
hdi(all_analysis_introspection_both)

all_bytask_introspection_both = all_data_introspection_both %>% 
  group_by(task_name, factor) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating),
            se_introspect_rating = se(introspect_rating))

ggplot(all_bytask_introspection_both, aes(x = task_name, y = mean_introspect_rating, color = factor)) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "", x = "Condition", y = "Influence rating") +
  theme_custom() +
  scale_fill_manual(values = in_and_ex)+
  guides(fill = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))+ 
  scale_y_continuous(limits = c(0, 100))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7))


# Save image --------------------------------------------------------------

save.image("pilot1_output.rdata")