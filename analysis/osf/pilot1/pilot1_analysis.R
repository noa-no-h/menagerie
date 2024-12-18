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
                         mutate(distance.from.low.anchor = scale(distance.from.low.anchor),
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

## in experience condition
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
  scale_fill_manual(values = in_and_ex)+
  guides(fill = FALSE)+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14))

analysis.avail.intro.experience = brm(introspect_rating ~ showed_effect,
                                     df.avail.intro.experience,
                                     save_pars = save_pars(group = F))
summary(analysis.avail.intro.experience)
hdi(analysis.avail.intro.experience)

## across conditions
summary.avail.intro.both = df.avail %>%
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
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Condition")

analysis.cause = brm(choice ~ condition,
                     df.cause,
                     save_pars = save_pars(group = F))
hdi(analysis.cause)

#** introspection ratings ----
df.cause.intro <- df.cause %>% filter(!is.na(introspect_rating))

## in experience condition

## across conditions

summary.cause.intro.both <- df.cause.intro %>% group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(summary.cause.intro.both, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

#** AM: splitting by whether they made choices suggesting that they showed the effect or not ----

median.choice.cause = median(df.cause$choice)
df.cause = df.cause %>%
  mutate(choice.matches.condition = ifelse(condition == 'One',
                                           choice > median.choice.cause,
                                           choice < median.choice.cause))

df.cause.intro.graph2 <- df.cause %>% group_by(factor, choice.matches.condition) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.cause.intro.graph2, aes(x = factor, y = introspect.m, fill = choice.matches.condition)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

cause_intro2 = lm(introspect_rating ~ factor * choice.matches.condition, df.cause)
summary(cause_intro2)

# transfering to main df
for (i in 1:nrow(df)) {
  if (df$task_name[i] == 'causal inference' & !is.na(df$introspect_rating[i])) {
    which.row = df.cause$subject == df$subject[i]
    if (any(which.row)) {
      df$effect.size.fac[i] = df.cause$choice.matches.condition[which.row]
    }
  }
}

cause_intro2.t = df.cause %>% filter(factor == 'Factor-Included') %$%
  t.test(introspect_rating ~ choice.matches.condition)

#* 6 Decoy Effect -------------------------------------------------------------
#** data preparation ----
df.decoy = df %>% filter(task_name == 'decoy effect') %>% filter(familiarity == "No")
length(unique(df.decoy$subject))
#** data visualization ----
ggplot(df.decoy, aes(x = condition, fill = choice)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))

#** inferential statistics ----
decoy <- table(df.decoy$choice, df.decoy$condition)
decoy
decoyChi <- chisq.test(decoy)
decoyChi$expected >= 5
decoyChi

# p-value = 3e-04

#** introspection ratings ----
df.decoy.intro <- df.decoy %>% filter(!is.na(introspect_rating))

df.decoy.intro.graph <- df.decoy.intro %>% group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.decoy.intro.graph, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

decoy_intro.t <- t.test(df.decoy$introspect_rating ~ df.decoy$factor)
decoy_intro.t
p.vals = c(p.vals, decoy_intro.t$p.value)

# AM: splitting by whether they made choices suggesting that they showed the effect or not

df.decoy = df.decoy %>%
  mutate(choice.matches.condition = ifelse(condition == 'Decoy Present',
                                           choice == 'Brand N (Target)',
                                           choice == 'Brand J (Competitor)'))

df.decoy.intro.graph2 <- df.decoy %>% group_by(factor, choice.matches.condition) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))
ggplot(df.decoy.intro.graph2, aes(x = factor, y = introspect.m, fill = choice.matches.condition)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

decoy_intro2 = lm(introspect_rating ~ factor * choice.matches.condition, df.decoy)
summary(decoy_intro2)

for (i in 1:nrow(df)) {
  if (df$task_name[i] == 'decoy effect' & !is.na(df$introspect_rating[i])) {
    which.row = df.decoy$subject == df$subject[i]
    if (any(which.row)) {
      df$effect.size.fac[i] = df.decoy$choice.matches.condition[which.row]
    }
  }
}

decoy_intro2.t = df.decoy %>% filter(factor == 'Factor-Included') %$%
  t.test(introspect_rating ~ choice.matches.condition)

#* 7 Associative memory (within-subjects) ------------------------------------------------------
#** data preparation ----
df.mem = df %>% filter(task_name == 'associative memory') %>%
  mutate(choice.fac = factor(choice))
mem.unfamiliar <- df.mem %>% filter(familiarity == "No")
df.mem.include <- df.mem %>%
  filter(subject %in% mem.unfamiliar$subject) %>%
  filter(factor == 'Factor-Included') %>%
  filter(auxiliary_info1 == 'New') 

length(unique(df.mem.include$subject))

#** data visualization ----
ggplot(df.mem.include, aes(x = condition, fill = choice)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))

#** inferential statistics ----
# AM: this extracts the subject-level effect estimates
mem.model = glmer(choice.fac ~ condition + (condition | subject) + (1 | stimulus), df.mem.include, family = 'binomial')
summary(mem.model)
mem.subj.estimates = coef(mem.model)$subject

# p-value <2e-16

#** introspection ratings ----
df.mem.intro = df.mem %>% 
  filter(subject %in% mem.unfamiliar$subject) %>%
  filter(!is.na(introspect_rating))

length(unique(df.mem.intro$subject))

df.mem.intro.graph = df.mem %>% filter(!is.na(introspect_rating)) %>%
  group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.mem.intro.graph, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

mem_intro.t <- t.test(df.mem$introspect_rating ~ df.mem$factor)
mem_intro.t
p.vals = c(p.vals, mem_intro.t$p.value)


#** AM: splitting by whether people showed the effect or not ----

mem.subj = df.mem.include %>%
  mutate(choice.num = as.numeric(choice == 'Original')) %>%
  group_by(subject, condition) %>%
  summarize(choice.m = mean(choice.num)) %>%
  mutate(choice.diff = choice.m - lag(choice.m)) %>%
  filter(!is.na(choice.diff)) %>%
  select(-c(condition, choice.m))

df.mem.intro2 = df.mem %>%
  filter(!is.na(introspect_rating)) %>%
  filter(subject %in% mem.unfamiliar$subject)

df.mem.intro2$subj.effect = NA

for (i in 1:nrow(df.mem.intro2)) {
  #which.row = rownames(mem.subj.estimates) == df.mem.intro2$subject[i]
  which.row = mem.subj$subject == df.mem.intro2$subject[i]
  if (any(which.row)) {
    #df.mem.intro2$subj.effect[i] = mem.subj.estimates$conditionSleep[which.row]
    df.mem.intro2$subj.effect[i] = mem.subj$choice.diff[which.row]
  }
}
df.mem.intro2$subj.effect.fac = df.mem.intro2$subj.effect > 0

mem_intro2.t = t.test(df.mem.intro2$introspect_rating ~ df.mem.intro2$subj.effect.fac)


df.mem.intro.graph2 = df.mem.intro2 %>%
  group_by(factor, subj.effect.fac) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.mem.intro.graph2, aes(x = factor, y = introspect.m, fill = subj.effect.fac)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

ggplot(df.mem.intro2, aes(x = subj.effect, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm')

for (i in 1:nrow(df)) {
  if (df$task_name[i] == 'associative memory' & !is.na(df$introspect_rating[i])) {
    which.row = df.mem.intro2$subject == df$subject[i]
    if (any(which.row)) {
      df$effect.size[i] = df.mem.intro2$subj.effect[which.row]
      df$effect.size.fac[i] = df.mem.intro2$subj.effect.fac[which.row]
    }
  }
}




#* 8 Belief (within-subjects) ------------------------------------------------------------------
#** data preparation ----
df.belief <- df %>% filter(task_name == 'belief')
bel.unfamiliar <- df.belief %>% filter(familiarity == "No")
df.belief2 <- df.belief %>% filter(subject %in% bel.unfamiliar$subject) %>% 
  mutate(choice.fac = factor(choice), condition.fac = factor(condition, c('Unbelievable', 'Believable')))
df.belief.facinc = df.belief2 %>% filter(factor == "Factor-Included", subject %in% bel.unfamiliar$subject)

df.belief.include = df.belief %>% filter(subject %in% bel.unfamiliar$subject) %>%  
  filter(!is.na(choice))

length(unique(df.belief2$subject))
  
#** data visualization ----
df.bel.graph <- df.belief2 %>% filter(subject %in% bel.unfamiliar$subject) %>% 
  filter(!is.na(choice)) %>%
  filter(!is.na(condition)) %>% filter(factor == "Factor-Included") #%>%
  #filter(auxiliary_info1 == "Valid") ## ADAM: Why filter for only valid here?

ggplot(df.bel.graph, aes(x = condition.fac, fill = choice)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))

#** inferential statistics ----
belief <- table(df.belief.include$choice, df.belief.include$condition)
belief
beliefChi <- chisq.test(belief)
beliefChi$expected >= 5
beliefChi

#p-value =  3e-11

#** introspection ratings ----

df.belief.intro = df.belief %>% filter(!is.na(introspect_rating)) %>%
  group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.belief.intro, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

belief_intro.t <- t.test(df.belief$introspect_rating ~ df.belief$factor)
belief_intro.t
p.vals = c(p.vals, belief_intro.t$p.value)

#** AM: splitting by whether people showed the effect or not ----
belief_lmer = glmer(choice.fac ~ condition.fac + (condition.fac | subject),
              df.belief.facinc %>% filter(!is.na(condition)),
              family = binomial)
summary(belief_lmer)
belief.subj.estimates = coef(belief_lmer)$subject
hist(belief.subj.estimates$condition.fac) 

belief.subj = df.belief.facinc %>%
  mutate(choice.num = as.numeric(choice == 'Yes')) %>%
  group_by(subject, condition.fac) %>%
  summarize(choice.m = mean(choice.num)) %>%
  mutate(choice.diff = choice.m - lag(choice.m)) %>%
  filter(!is.na(choice.diff)) %>%
  select(-c(condition.fac, choice.m))

df.belief.intro2 = df.belief %>%
  filter(!is.na(introspect_rating)) %>%
  filter(subject %in% bel.unfamiliar$subject)
df.belief.intro2$subj.effect = NA
for (i in 1:nrow(df.belief.intro2)) {
  #which.row = rownames(belief.subj.estimates) == df.belief.intro2$subject[i]
  which.row = belief.subj$subject == df.belief.intro2$subject[i]
  if (any(which.row)) {
    #df.belief.intro2$subj.effect[i] = belief.subj.estimates$condition.facUnbelievable[which.row]
    df.belief.intro2$subj.effect[i] = belief.subj$choice.diff[which.row]
  }
}
df.belief.intro2$subj.effect.fac = df.belief.intro2$subj.effect > 0

belief_intro2.t = t.test(df.belief.intro2$introspect_rating ~ df.belief.intro2$subj.effect.fac)

df.belief.intro.graph2 = df.belief.intro2 %>%
  group_by(factor, subj.effect.fac) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.belief.intro.graph2, aes(x = factor, y = introspect.m, fill = subj.effect.fac)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

ggplot(df.belief.intro2, aes(x = subj.effect, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm')

for (i in 1:nrow(df)) {
  if (df$task_name[i] == 'belief' & !is.na(df$introspect_rating[i])) {
    which.row = df.belief.intro2$subject == df$subject[i]
    if (any(which.row)) {
      df$effect.size[i] = df.belief.intro2$subj.effect[which.row]
      df$effect.size.fac[i] = df.belief.intro2$subj.effect.fac[which.row]
    }
  }
}



#* 9 Mere exposure (within-subjects) ------------------------------------------------------
#** data preparation ----
df.mee = df %>% filter(task_name == 'mere exposure') %>% mutate(choice = as.numeric(choice))
mee.unfamiliar <- df.mee %>% filter(familiarity == "No")
df.mee <- df.mee %>% filter(subject %in% mee.unfamiliar$subject) 

df.mee.include <- df.mee %>% filter(factor == "Factor-Included") %>%
  mutate(choice = as.numeric(choice), auxiliary_info1 = factor(auxiliary_info1, levels = c('5', '25'))) %>%
  filter(!is.na(choice))

#** data visualization (general) ----

df.mee.graph = df.mee.include %>%
  group_by(condition) %>%
  summarize(choice.m = mean(choice), choice.se = se(choice))

ggplot(df.mee.graph, aes(x = condition, y = choice.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Condition", y = "Mean Liking Rating")
ggplot(df.mee.graph, aes(x = condition, y = choice.m)) +
  geom_point(color = 'white', size = 5) + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), color = 'white', size = 1, width = .1) +
  labs(x = "Condition", y = "Mean Liking\nRating") +
  theme_black() #+
  #scale_y_continuous(limits = c(45,60))

#** data visualization (by-subject) ----

df.mee.graph = df.mee.include %>%
  group_by(subject, condition) %>%
  summarize(choice.m = mean(choice), choice.se = se(choice))

ggplot(df.mee.graph, aes(x = condition, y = choice.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = choice.m - choice.se, ymax = choice.m + choice.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Condition", y = "Mean Liking Rating") + facet_wrap(~subject)

#** inferential statistics ----
mee.model <- lmer(choice ~ condition + (1 + condition | subject), data = df.mee.include %>% mutate(choice = scale(choice)))
summary(mee.model)
coef(mee.model)
hist(coef(mee.model)$subject[,2])
df.temp = data.frame(d = coef(mee.model)$subject[,2])
ggplot(data = df.temp, aes(x = d)) +
  geom_histogram(color = 'white') +
  theme_black() +
  labs(x = "Cohen's D", y = 'Number of\nsubjects') +
  scale_y_continuous(breaks = c()) +
  geom_vline(xintercept = 0, color = 'white', linetype = 2)

#introspection ratings
df.mee.intro <- df.mee %>% filter(!is.na(introspect_rating))

length(unique(df.mee.intro$subject))

df.mee.intro.graph <- df.mee.intro %>% group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.mee.intro.graph, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

mee_intro.t <- t.test(df.mee$introspect_rating ~ df.mee$factor)
mee_intro.t
p.vals = c(p.vals, mee_intro.t$p.value)


#** AM: splitting by whether people showed the effect or not ----
#mee.subj.estimates = coef(mee.model)$subject

mee.subj = df.mee.include %>%
  group_by(subject, condition) %>%
  summarize(choice.m = mean(choice)) %>%
  mutate(choice.diff = choice.m - lag(choice.m)) %>%
  filter(!is.na(choice.diff)) %>%
  select(-c(condition, choice.m))

df.mee.intro2 = df.mee %>%
  filter(!is.na(introspect_rating)) %>%
  filter(subject %in% bel.unfamiliar$subject)
df.mee.intro2$subj.effect = NA
for (i in 1:nrow(df.mee.intro2)) {
  #which.row = rownames(mee.subj.estimates) == df.mee.intro2$subject[i]
  which.row = mee.subj$subject == df.mee.intro2$subject[i]
  if (any(which.row)) {
    #df.mee.intro2$subj.effect[i] = mee.subj.estimates$condition25[which.row]
    df.mee.intro2$subj.effect[i] = mee.subj$choice.diff[which.row]
  }
}
df.mee.intro2$subj.effect.fac = df.mee.intro2$subj.effect > 0

mee_intro2.t = t.test(df.mee.intro2$introspect_rating ~ df.mee.intro2$subj.effect.fac)


df.mee.intro.graph2 = df.mee.intro2 %>%
  group_by(factor, subj.effect.fac) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.mee.intro.graph2, aes(x = factor, y = introspect.m, fill = subj.effect.fac)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

ggplot(df.mee.intro2, aes(x = subj.effect, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm')

for (i in 1:nrow(df)) {
  if (df$task_name[i] == 'mere exposure' & !is.na(df$introspect_rating[i])) {
    which.row = df.mee.intro2$subject == df$subject[i]
    if (any(which.row)) {
      df$effect.size[i] = df.mee.intro2$subj.effect[which.row]
      df$effect.size.fac[i] = df.mee.intro2$subj.effect.fac[which.row]
    }
  }
}

# INTROSPECTION RATINGS (ALL) -----------------------------------------------------------


## get introspection p values for each task & correct
p.vals = numeric(11)
p.vals = c(
  anchor_intro.t$p.value,
  mem_intro.t$p.value,
  avail_intro.t$p.value,
  belief_intro.t$p.value,
  cause_intro.t$p.value,
  contact_intro.t$p.value,
  decoy_intro.t$p.value,
  double_intro.t$p.value,
  mee_intro.t$p.value,
  numb_intro.t$p.value,
  simon_intro.t$p.value
)
p.vals.corrected = p.adjust(p.vals, method = 'holm')
p.vals.corrected < .05

## do same thing, but for second analysis
p.vals2 = numeric(11)
p.vals2 = c(
  anchor_intro2.t$p.value,
  mem_intro2.t$p.value,
  avail_intro2.t$p.value,
  belief_intro2.t$p.value,
  cause_intro2.t$p.value,
  contact_intro2.t$p.value,
  decoy_intro2.t$p.value,
  double_intro2.t$p.value,
  mee_intro2.t$p.value,
  numb_intro2.t$p.value,
  simon_intro2.t$p.value
)
p.vals2.corrected = p.adjust(p.vals2, method = 'holm')
p.vals2.corrected < .05


df.introspection = df %>%
  group_by(factor, task_name) %>%
  filter(!is.na(introspect_rating)) %>%
  filter(familiarity == "No") %>%
  mutate(introspect_rating = introspect_rating/10) %>% 
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

df.introspection.all = df %>%
  filter(task_name %in% new.tasks) %>% 
  group_by(factor) %>%
  filter(!is.na(introspect_rating)) %>%
  filter(familiarity == "No") %>%
  mutate(introspect_rating = introspect_rating/10) %>% 
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(df.introspection, aes(x = task_name, y = introspect.m)) +
  geom_point() +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  facet_wrap(~factor)


ggplot(df, aes(x = task_name, y = introspect_rating)) +
  geom_violin() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  facet_wrap(~factor)

new.tasks = c('anchoring', 'associative memory', 'availability', 'belief', 'causal inference', 'decoy effect', 'mere exposure')

ggplot(df.introspection %>% filter(task_name %in% new.tasks), aes(x = task_name, y = introspect.m, fill = factor)) +
  geom_point(size = 4,pch=21,color='white') +
  #geom_jitter(data = df.test %>% filter(task_name %in% new.tasks), pch=21, aes(y = introspect_rating), size = 1, alpha = .5, width = .2) +
  scale_fill_manual(values = c("Factor-Included" = "#4FADEA", "Factor-Excluded" = "#FFC000")) +
  scale_color_manual(values = c("Factor-Included" = "#4FADEA", "Factor-Excluded" = "#FFC000")) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se, color = factor), width = 0.5) +
  theme_black() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14)) +
  #theme(legend.position = 'none') +
  labs(x = 'Task', y = 'Rating of factor influence') +
  scale_y_continuous(limits = c(0,1))

df.test = df %>%
  group_by(factor, task_name) %>%
  filter(!is.na(introspect_rating)) %>%
  filter(familiarity == "No") %>%
  mutate(introspect_rating = introspect_rating/10)
  

ggplot(df.introspection.all, aes(x = factor, y = introspect.m)) +
  geom_point(color = 'white', size = 5) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), color = 'white', width = 0.2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  labs(x = 'Condition', y = 'Rating of factor influence') +
  theme_black()

introspect.model <- lmer(introspect_rating ~ factor + (1 | subject) + (1 | task_name), data = df %>% filter(familiarity == 'No', task_name %in% new.tasks))
summary(introspect.model)

# restricting to factor included, splitting by whether they showed effect
df.introspection2 = df %>%
  filter(factor == 'Factor-Included') %>%
  group_by(effect.size.fac, task_name) %>%
  filter(!is.na(introspect_rating)) %>%
  filter(familiarity == "No") %>%
  mutate(introspect_rating = introspect_rating/10) %>% 
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating)) %>%
  filter(!is.na(effect.size.fac))

df.introspection.all2 = df %>%
  filter(factor == 'Factor-Included', task_name %in% new.tasks) %>%
  group_by(effect.size.fac) %>%
  filter(!is.na(introspect_rating)) %>%
  filter(familiarity == "No") %>%
  mutate(introspect_rating = introspect_rating/10) %>% 
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating)) %>%
  filter(!is.na(effect.size.fac))

ggplot(df.introspection2 %>% filter(task_name %in% new.tasks), aes(x = task_name, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 4) +
  scale_color_manual(values = c('FALSE' = "red", 'TRUE' = "green")) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se)) +
  theme_black() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14)) +
  labs(x = 'Task', y = 'Rating of factor influence')

ggplot(df.introspection2, aes(x = task_name, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 4,pch=21,color='white') +
  #geom_jitter(data = df.test, pch=21, aes(y = introspect_rating), size = 1, alpha = .5, width = .2) +
  scale_fill_manual(values = c("Factor-Included" = "#4FADEA", "Factor-Excluded" = "#FFC000")) +
  scale_color_manual(values = c("Factor-Included" = "#4FADEA", "Factor-Excluded" = "#FFC000")) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se)) +
  theme_black() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
        legend.position = 'none')

ggplot(df.introspection.all2, aes(x = effect.size.fac, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('FALSE' = "red", 'TRUE' = "green")) +
  theme_black() +
  scale_y_continuous(limits = c(0.3, 0.7), breaks = c(0.3, 0.7)) +
  labs(x = '\nDemonstrated effect?', y = 'Rating of factor influence') +
  theme(legend.position = 'none')

df = df %>% group_by(task_name) %>%
  mutate(effect.size.scaled = scale(effect.size)) %>%
  ungroup()

ggplot(df %>% filter(!is.na(effect.size), task_name %in% new.tasks),
       aes(x = effect.size.scaled, y = introspect_rating)) +
  geom_point(color = 'gray', alpha = .5) +
  geom_smooth(method='lm', color = 'white') +
  theme_black() +
  labs(x = 'Size of factor effect', y = 'Rating of factor influence') +
  facet_wrap(~task_name)

introspect.model2 <- lmer(introspect_rating ~ effect.size.fac + (effect.size.fac | subject) + (1 + effect.size.fac | task_name),
                          data = df %>% filter(!is.na(effect.size.fac), task_name %in% new.tasks))
summary(introspect.model2)
introspect.model3 <- lmer(introspect_rating ~ effect.size + (effect.size | subject) + (effect.size | task_name),
                          data = df %>% filter(!is.na(effect.size), task_name %in% new.tasks))
summary(introspect.model3)

# DEMOGRAPHICS ----------------------------------------------------------------
length(unique(demo$subject))

# gender
table(demo$gender)

# age
table(demo$age)

ggplot(demo, aes(x = age)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))


# education
table(demo$education)

ggplot(demo, aes(x = education)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))


table(demo$income)


ggplot(demo, aes(x = income)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))

# FEEDBACK ----------------------------------------------------------------

df.feedback = df %>% filter(task_name == 'feedback')
View(df.feedback)

# QUESTIONNAIRES ----
#* functions and variables ----
as.string.vector = function(x) {
  return(strsplit(x,',')[[1]])
}
as.numeric.vector = function(x) {
  return(as.numeric(strsplit(gsub('\\[|\\]','',x),',')[[1]]))
}

df.introspection.subj = df %>% group_by(subject, factor) %>%
  filter(!is.na(introspect_rating)) %>%
  summarize(avg_introspect_rating = mean(introspect_rating)) %>%
  mutate (factor.condition = factor)

for (i in 1:nrow(demo)) {
  which.row = df.introspection.subj$subject == demo$subject[i]
  demo$factor.condition[i] = df.introspection.subj$factor[which.row]
  demo$avg_introspect_rating[i] = df.introspection.subj$avg_introspect_rating[which.row]
}

demo$factor.condition <- factor(demo$factor.condition, levels = c("Factor-Included","Factor-Excluded"), ordered=T)
  

#* Adding Data to Data Frame ----
#** audit score ----
all.audit.options = list(c("Never", "Monthly or less", "Two to four times a month", "Two to three times a week", "Four or more times a week"),
                         c("0 (Not Applicable)","1 or 2", "3 or 4", "5 or 6", "7 to 9", "10 or more"),
                         c("Never", "Less than monthly", "Monthly", "Weekly", "Daily or almost daily"),
                         c("No", "", "Yes, but not in the last year", "", "Yes, during the last year"))
audit.resp.numbers = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4)

for (i in 1:nrow(demo)) {
  response = as.string.vector(demo[i, 'audit'])
  where.response.falls.on.scale = numeric(length(response))
  
  where.yes = which(response == 'Yes')
  if (length(where.yes) == 1) {
    response[where.yes] = paste(response[where.yes],response[where.yes+1],sep=",")
    response = response[-(where.yes+1)]
  } else if (length(where.yes) == 2) {
    response[9] = paste(response[9],response[10],sep=",")
    response[11] = paste(response[11],response[12],sep=",")
    response = response[-c(10,12)]
  }
  
  for (j in 1:length(response)) {
    current.question.response = response[j]
    which.scale.number = audit.resp.numbers[j]
    which.scale = all.audit.options[[which.scale.number]]
    where.response.falls.on.scale[j] = which(current.question.response == which.scale) - 1
  }
  total.audit.response = sum(where.response.falls.on.scale)
  demo$audit.score[i] = total.audit.response
}

#** phq9 scores ----
for (i in 1:nrow(demo)) {
  response = as.numeric.vector(demo[i, 'phq91'])
  demo$phq9.score[i] = sum(response) + demo[i,'phq92']
}

#** gad7 scores ----
for (i in 1:nrow(demo)) {
  response = as.numeric.vector(demo[i, 'gad71'])
  demo$gad7.score[i] = sum(response) + demo[i,'gad72']
}

#** spq scores ----
for (i in 1:nrow(demo)) {
  response = as.numeric.vector(demo[i, 'spq'])
  demo$spq.score[i] = sum(response)
}

#** ocir scores ----
for (i in 1:nrow(demo)) {
  response = as.numeric.vector(demo[i, 'ocir'])
  demo$ocir.score[i] = sum(response)
}

#** binaries and adding to both data frames ----

demo = demo %>%
  mutate(audit.score.binary = factor(audit.score > median(demo$audit.score), c(F,T), c('Low', 'High')),
         phq9.score.binary = factor(phq9.score > median(demo$phq9.score), c(F,T), c('Low', 'High')),
         phq9.score.cutoffs = factor(cut(phq9.score, c(-1, 5, 10, 15, 20, 30), c('None', 'Mild', 'Moderate', 'Moderately-severe', 'Severe'))),
         gad7.score.binary = factor(gad7.score > median(demo$gad7.score), c(F,T), c('Low', 'High')),
         gad7.score.cutoffs = factor(cut(gad7.score, c(-1, 5, 10, 15, 25), c('None', 'Mild', 'Moderate', 'Severe'))),
         spq.score.binary = factor(spq.score > median(demo$spq.score), c(F,T), c('Low', 'High')),
         spq.score.cutoffs = factor(cut(spq.score, c(-1, 21, 42, 120), c('None', 'Moderate', 'Severe'))),
         ocir.score.binary = factor(ocir.score > median(demo$ocir.score), c(F,T), c('Low', 'High')),
         ocir.score.cutoffs = factor(cut(ocir.score, c(-1, 21, 50), c('No', 'Yes'))),
  )

for (i in 1:nrow(df)) {
  which.row = demo$subject == df$subject[i]
  df$audit.score[i] = demo$audit.score[which.row]
  df$phq9.score[i] = demo$phq9.score[which.row]
  df$gad7.score[i] = demo$gad7.score[which.row]
  df$spq.score[i] = demo$spq.score[which.row]
  df$ocir.score[i] = demo$ocir.score[which.row]
  df$audit.score.binary[i] = demo$audit.score.binary[which.row]
  df$phq9.score.binary[i] = demo$phq9.score.binary[which.row]
  df$gad7.score.binary[i] = demo$gad7.score.binary[which.row]
  df$spq.score.binary[i] = demo$spq.score.binary[which.row]
  df$ocir.score.binary[i] = demo$ocir.score.binary[which.row]
  df$phq9.score.cutoffs[i] = demo$phq9.score.cutoffs[which.row]
  df$gad7.score.cutoffs[i] = demo$gad7.score.cutoffs[which.row]
  df$spq.score.cutoffs[i] = demo$spq.score.cutoffs[which.row]
  df$ocir.score.cutoffs[i] = demo$ocir.score.cutoffs[which.row]
}

#* Descriptive Statistics ----
#** PHQ-9 ----
PHQ9_hist <- 
  ggplot(demo, aes(x=phq9.score)) +
  geom_histogram(binwidth=1, color="black", fill="lightgray") +
  labs(x = "PHQ-9 Score", y = "Frequency") +
  ylim(0,60) +
  my_theme +
  facet_wrap(.~factor.condition, ncol=1)

PHQ9_hist

# ggsave(PHQ9_hist, filename="PHQ9_histogram.png", 
#        height = 4, width = 4, units = c("in"))

#** GAD-7 ----
GAD7_hist <- 
  ggplot(demo, aes(x=gad7.score)) +
  geom_histogram(binwidth=1, color="black", fill="lightgray") +
  labs(x = "GAD-7 Score", y = "Frequency") +
  xlim(-1,30) +
  ylim(0,70) +
  my_theme +
  facet_wrap(.~factor.condition, ncol=1)

GAD7_hist

#** AUDIT ----

AUDIT_hist <-
  ggplot(demo, aes(x=audit.score)) +
  geom_histogram(binwidth=1, color="black", fill="lightgray") +
  labs(x = "AUDIT Score", y = "Frequency") +
  my_theme +
  facet_wrap(.~factor.condition, ncol=1)

AUDIT_hist

ggsave(AUDIT_hist, filename="AUDIT_histogram.png", 
       height = 4, width = 4, units = c("in"))

#** SPQ ----
SPQ_hist <-
  ggplot(demo, aes(x=spq.score)) +
  geom_histogram(binwidth=1, color="black", fill="lightgray") +
  labs(x = "SPQ-B Score", y = "Frequency") +
  my_theme +
  facet_wrap(.~factor.condition, ncol=1)

SPQ_hist

ggsave(SPQ_hist, filename="AUDIT_histogram.png", 
       height = 4, width = 4, units = c("in"))



#** OCIR ----
OCIR_hist <-
  ggplot(demo, aes(x=ocir.score)) +
  geom_histogram(binwidth=1, color="black", fill="lightgray") +
  labs(x = "OCI-R Score", y = "Frequency") +
  my_theme +
  facet_wrap(.~factor.condition, ncol=1)

OCIR_hist


ggsave(OCIR_hist, filename="OCIR_histogram.png", 
       height = 4, width = 4, units = c("in"))


#*Interaction Analyses ----
#** Audit ----
audit.graph = demo %>% group_by(factor.condition, audit.score.binary) %>%
  summarize(introspect.m = mean(avg_introspect_rating), introspect.se = se(avg_introspect_rating))

ggplot(audit.graph, aes(x = audit.score.binary, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values=c("coral", "turquoise"))

audit.model <- lmer(introspect_rating ~ factor * audit.score + (1 | subject) + (1 + factor | task_name) , data = df %>% mutate(audit.score = scale(audit.score), introspect_rating = scale(introspect_rating)))
summary(audit.model)

audit.interact <-
  plot_model(audit.model, type = "pred", terms=c("audit.score", "factor")) + 
  labs(x = "AUDIT Score", y = "Key Factor Rating") +
  interact_theme

audit.interact

ggsave(audit.interact, filename="AUDIT_interaction.png", 
       height = 5, width = 8, units = c("in"))

#** PHQ-9 ----
phq9.graph = demo %>% group_by(factor.condition, phq9.score.cutoffs) %>%
  summarize(introspect.m = mean(avg_introspect_rating),
            introspect.se = se(avg_introspect_rating))

ggplot(phq9.graph, aes(x = phq9.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#4FADEA', '#FFC000')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Depression score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('None', 'Mild', 'Moderate', 'Moderately-\nSevere', 'Severe'))

phq9.graph2 = df %>% 
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(factor.condition, phq9.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(phq9.graph2, aes(x = phq9.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#FFC000', '#4FADEA')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Depression score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)

phq9.model <- lmer(introspect_rating ~ phq9.score * factor.condition + (1 | subject),
                   data = df %>%
                     mutate(phq9.score = scale(phq9.score),
                     introspect_rating = scale(introspect_rating)))
summary(phq9.model)

phq9.model2 <- lm(introspect_rating ~ phq9.score * factor.condition,
                   data = df %>% filter(task_name == 'causal inference'))
summary(phq9.model2)

plot_model(phq9.model, type = "int", mdrt.values = 'meansd',
           terms=c("factor", "phq9.score")) + 
  labs(x = "PHQ-9 Score", y = "Key Factor Rating", title = '') +
  theme_black() +
  scale_color_manual(values = c('#FFC000', '#4FADEA')) +
  scale_fill_manual(values = c('#FFC000', '#4FADEA'))

#** gad7 ----
gad7.graph = demo %>% group_by(factor.condition, gad7.score.cutoffs) %>%
  summarize(introspect.m = mean(avg_introspect_rating), introspect.se = se(avg_introspect_rating))

ggplot(gad7.graph, aes(x = gad7.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#4FADEA', '#FFC000')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Anxiety score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('None', 'Mild', 'Moderate', 'Severe'))

gad7.graph2 = df %>% 
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(factor.condition, gad7.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(gad7.graph2, aes(x = gad7.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#FFC000', '#4FADEA')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Anxiety score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)

gad7.model <- lmer(introspect_rating ~ factor * gad7.score + (1 | subject) + (1 + factor | task_name) , data = df)
summary(gad7.model)

gad7.interact <-
  plot_model(gad7.model, type = "pred", terms=c("gad7.score", "factor")) + 
  labs(x = "GAD-7 Score", y = "Key Factor Rating", col = "IACP-Test Version") +
  theme(plot.title = element_blank())

gad7.interact

ggsave(gad7.interact, filename="GAD-7_interaction.png", 
       height = 5, width = 8, units = c("in"))

#** spq ----
spq.graph = demo %>% group_by(factor.condition, spq.score.cutoffs) %>%
  summarize(introspect.m = mean(avg_introspect_rating), introspect.se = se(avg_introspect_rating))

ggplot(spq.graph, aes(x = spq.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#4FADEA', '#FFC000')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Schizotypy score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('None', 'Moderate', 'Severe'))

spq.graph2 = df %>% 
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(factor.condition, spq.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(spq.graph2, aes(x = spq.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#FFC000', '#4FADEA')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Schizotypy score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)

spq.model <- lmer(introspect_rating ~ factor * spq.score + (1 | subject),
                  data = df %>% mutate(spq.score = scale(spq.score), introspect_rating = scale(introspect_rating)))
summary(spq.model)

#** ocir ----
ocir.graph = demo %>% group_by(factor.condition, ocir.score.cutoffs) %>%
  summarize(introspect.m = mean(avg_introspect_rating), introspect.se = se(avg_introspect_rating))

ggplot(ocir.graph, aes(x = ocir.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#4FADEA', '#FFC000')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Obsessive-compulsiveness score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('Low', 'High'))

ocir.graph2 = df %>% 
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(factor.condition, ocir.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(ocir.graph2, aes(x = ocir.score.cutoffs, y = introspect.m, group = factor.condition, color = factor.condition)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('#FFC000', '#4FADEA')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Obsessive-compulsiveness score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)

ocir.model <- lmer(introspect_rating ~ factor * ocir.score + (1 | subject), data = df %>% mutate(ocir.score = scale(ocir.score), introspect_rating = scale(introspect_rating)))
summary(ocir.model)

ocir.model2 <- lm(avg_introspect_rating ~ factor.condition * ocir.score.cutoffs,
                  data = demo)
summary(ocir.model2)

# all together

df_selected <- demo %>%
  select(subject, avg_introspect_rating, factor.condition, phq9.score.cutoffs, gad7.score.cutoffs, spq.score.cutoffs, ocir.score.cutoffs)

df_selected <- df_selected %>%
  mutate(phq9.score.cutoffs = as.numeric(phq9.score.cutoffs),
         gad7.score.cutoffs = as.numeric(gad7.score.cutoffs),
         spq.score.cutoffs = as.numeric(spq.score.cutoffs),
         ocir.score.cutoffs = as.numeric(ocir.score.cutoffs))

df_long <- df_selected %>%
  pivot_longer(cols = c(phq9.score.cutoffs, gad7.score.cutoffs, spq.score.cutoffs, ocir.score.cutoffs),
               names_to = "cutoff_type",
               values_to = "cutoff_value")

test.model <- lmer(avg_introspect_rating ~ factor.condition * cutoff_value +
                    (1 | cutoff_type),
                  data = df_long)
summary(test.model)



#* Exploratory Analysis 2 ----
#** PHQ-9 
phq9.explore = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  group_by(effect.size.fac, phq9.score.cutoffs) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(phq9.explore, aes(x = phq9.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Depression score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('None', 'Mild', 'Moderate', 'Moderately-\nSevere', 'Severe'))

phq9.explore.graph2 = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(effect.size.fac, phq9.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(phq9.explore.graph2, aes(x = phq9.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Depression score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)

phq9.exp.model <- lmer(introspect_rating ~ effect.size.fac * phq9.score + (1 | subject) + (1 + effect.size.fac | task_name), data = df %>% filter(factor == "Factor-Included"))
summary(phq9.exp.model)

#** GAD-7
gad7.explore = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  group_by(effect.size.fac, gad7.score.cutoffs) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(gad7.explore, aes(x = gad7.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Anxiety score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('None', 'Mild', 'Moderate', 'Moderately-\nSevere', 'Severe'))

gad7.explore.graph2 = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(effect.size.fac, gad7.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(gad7.explore.graph2, aes(x = gad7.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Anxiety score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)

gad7.explore.model <- lmer(introspect_rating ~ effect.size.fac * gad7.score.cutoffs + (1 | subject),
                     data = df %>%
                       filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)))
summary(gad7.explore.model)

gad7.explore.test = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  group_by(effect.size.fac, gad7.score.cutoffs, subject) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

gad7.explore.model2 <- lm(introspect.m ~ effect.size.fac * gad7.score.cutoffs,
                     data = gad7.explore.test)
summary(gad7.explore.model2)

#** AUDIT
audit.explore = df %>%
  filter(factor == 'Factor-Included') %>%
  group_by(effect.size.fac, audit.score.binary) %>%
  filter(!is.na(introspect_rating)) %>%
  filter(familiarity == "No") %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating)) %>%
  filter(!is.na(effect.size.fac)) %>%
  mutate(audit.score.binary = as.factor(audit.score.binary), audit.score.binary = recode(audit.score.binary, "TRUE" = "High", "FALSE" = "Low")) %>%
  mutate(effect.size.fac = as.factor(effect.size.fac), effect.size.fac = recode(effect.size.fac, "TRUE" = "Successful", "FALSE" = "Unsuccessful")) 

ggplot(audit.explore, aes(x = audit.score.binary, y = introspect.m, fill = effect.size.fac)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "AUDIT Score", y = "Key Factor Rating", fill = "Key Factor")

ggplot(audit.explore, aes(x = audit.score.binary, y = introspect.m, group = effect.size.fac, color = effect.size.fac)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values=c("coral", "turquoise"))

audit.explore <- lmer(introspect_rating ~ effect.size.fac * audit.score + (1 | subject) + (1 + effect.size.fac | task_name), data = df %>% filter(factor == "Factor-Included"))
summary(audit.explore)

#** SPQ
spq.explore = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  group_by(effect.size.fac, spq.score.cutoffs) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(spq.explore, aes(x = spq.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Schizotypy score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('None', 'Mild', 'Moderate', 'Moderately-\nSevere', 'Severe'))

spq.explore.graph2 = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(effect.size.fac, spq.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(spq.explore.graph2, aes(x = spq.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Schizotypy score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)
spq.explore <- lmer(introspect_rating ~ effect.size.fac * spq.score + (1 | subject) + (1 + effect.size.fac | task_name), data = df %>% filter(factor == "Factor-Included"))
summary(spq.explore)

#** OCIR
ocir.explore = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  group_by(effect.size.fac, ocir.score.cutoffs) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(ocir.explore, aes(x = ocir.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 10) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Obsessive-Compulsiveness score', y = 'Rating of factor influence') +
  scale_x_discrete(labels = c('None', 'Mild', 'Moderate', 'Moderately-\nSevere', 'Severe'))

ocir.explore.graph2 = df %>%
  filter(factor == 'Factor-Included', !is.na(introspect_rating), familiarity == "No", !is.na(effect.size.fac)) %>%
  filter(!(task_name %in% c('attention check 2', 'attention check 3', 'feedback'))) %>%
  group_by(effect.size.fac, ocir.score.cutoffs, task_name) %>%
  summarize(introspect.m = mean(introspect_rating, na.rm=T),
            introspect.se = se(introspect_rating))

ggplot(ocir.explore.graph2, aes(x = ocir.score.cutoffs, y = introspect.m, color = effect.size.fac)) +
  geom_point(size = 2) +
  geom_line(size = 2) +
  geom_errorbar(aes(ymin = introspect.m - introspect.se,
                    ymax = introspect.m + introspect.se), width = .2) +
  scale_color_manual(values = c('red', 'green')) +
  theme_black() +
  theme(legend.position = 'none') +
  labs(x = 'Obsessive-Compulsiveness score', y = 'Rating of factor influence') +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(~ task_name)
ocir.explore <- lmer(introspect_rating ~ effect.size.fac * ocir.score + (1 | subject) + (1 | task_name), data = df %>% filter(factor == "Factor-Included"))
summary(ocir.explore)

ocir.explore <- glmer(effect.size.fac ~ spq.score + (1 | subject) + (1 | task_name),
                      data = df %>% filter(factor == "Factor-Included"),
                      family = 'binomial')
summary(ocir.explore)
ocir.explore <- lmer(effect.size ~ phq9.score + (1 | subject) + (1 | task_name),
                      data = df %>% filter(factor == "Factor-Included"))
summary(ocir.explore)

# restricting to factor included, splitting by whether they showed effect
# df.introspection2 = df %>%
#   filter(factor == 'Factor-Included') %>%
#   group_by(effect.size.fac, task_name) %>%
#   filter(!is.na(introspect_rating)) %>%
#   filter(familiarity == "No") %>%
#   mutate(introspect_rating = introspect_rating/10) %>% 
#   summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating)) %>%
#   filter(!is.na(effect.size.fac))
# 
# df.introspection.all2 = df %>%
#   filter(factor == 'Factor-Included') %>%
#   group_by(effect.size.fac) %>%
#   filter(!is.na(introspect_rating)) %>%
#   filter(familiarity == "No") %>%
#   mutate(introspect_rating = introspect_rating/10) %>% 
#   summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating)) %>%
#   filter(!is.na(effect.size.fac))
# 
# ggplot(df.introspection2, aes(x = task_name, y = introspect.m, color = effect.size.fac)) +
#   geom_point() +
#   #scale_color_manual(values = c("Factor-Included" = "blue", "Factor-Excluded" = "red")) +
#   geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se)) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))
# 
# ggplot(df.introspection.all2, aes(x = effect.size.fac, y = introspect.m)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))
# 
# df = df %>% group_by(task_name) %>%
#   mutate(effect.size.scaled = scale(effect.size)) %>%
#   ungroup()
# 
# ggplot(df %>% filter(!is.na(effect.size)),
#        aes(x = effect.size.scaled, y = introspect_rating)) +
#   geom_point() +
#   geom_smooth(method='lm') +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10))
# 
# introspect.model2 <- lmer(introspect_rating ~ effect.size.fac + (effect.size.fac | subject) + (1 + effect.size.fac | task_name),
#                           data = df %>% filter(!is.na(effect.size.fac)))
# summary(introspect.model2)
# introspect.model3 <- lmer(introspect_rating ~ effect.size + (1 | subject),
#                           data = df %>% filter(!is.na(effect.size)))
# summary(introspect.model3)

save.image("final_analysis_executed_AM.rdata")

