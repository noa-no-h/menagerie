# Setup -------------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(sjPlot)
require(magrittr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
dodge <- position_dodge(width=0.9)

data <- read.csv('data.csv') %>%
  arrange(subject, task_name) %>%
  mutate(introspect_rating = as.numeric(introspect_rating))
head(data)

data = data %>%
  filter(familiarity != "Yes") %>% #only when people are not familiar with a task
  mutate(
    task_name = factor(task_name)  # Convert task_name to a factor
  )

View(data)

p.vals = c()

# 1 anchoring effect -------------------------------------------------



## 1.1 Do we see the effect -----------------------------------------------------------------------


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
summary_anchor_antarctica_data <- anchor_antarctica_data %>%
  group_by(condition) %>%
  summarize(
    mean_choice = mean(choice, na.rm = TRUE),
    se_choice = sd(choice, na.rm = TRUE) / sqrt(n())
  )


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

t_test_result <- t.test(low_anchor, no_anchor)

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

## 1.2 introspection -----------------------------------------------------------------------


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



# If we do this, we get no subjects because there are no people in the whale group showing the effect:
#common_subjects <- intersect(antarctic_subject_showing_effect, whale_subject_showing_effect)
#View(common_subjects)

#did the subjects who were anchored feel they were more affected than the factor-excluded subjects


introspection_anchoring <- data %>%
  filter(task_name == "anchoring") %>%
  filter(introspect_rating != "") %>%
  mutate(introspect_rating = as.numeric(introspect_rating))

anchored_introspection <- introspection_anchoring %>%
  filter(factor == 'Factor-Included') %>%
  pull(introspect_rating)

anchored_affected_introspection <- introspection_anchoring %>%
  filter(subject %in% antarctic_subject_showing_effect) %>%
  filter(factor == 'Factor-Included') %>%
  pull(introspect_rating)

anchored_unaffected_introspection <- introspection_anchoring %>%
  filter(!subject %in% antarctic_subject_showing_effect) %>%
  filter(factor == 'Factor-Included') %>%
  pull(introspect_rating)

unanchored_introspection <- introspection_anchoring %>%
  filter(factor == 'Factor-Excluded') %>%
  pull(introspect_rating)

#View(anchored_affected_introspection)
#View(anchored_unaffected_introspection)
#View(unanchored_introspection)

anchored_affected_stats <- mean_se(anchored_affected_introspection)
anchored_unaffected_stats <- mean_se(anchored_unaffected_introspection)
unanchored_stats <- mean_se(unanchored_introspection)

data_plot <- data.frame(
  Group = c("Anchored Affected", "Anchored Unaffected", "Unanchored"),
  Mean = c(anchored_affected_stats["y"], anchored_unaffected_stats["y"], unanchored_stats["y"]),
  SE = c(anchored_affected_stats["y"]-anchored_affected_stats["ymin"], anchored_unaffected_stats["y"]-anchored_unaffected_stats["ymin"], unanchored_stats["y"]-unanchored_stats["ymin"])
)

ggplot(data_plot, aes(x = Group, y = Mean)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Anchoring Introspection Ratings",
       x = "Group",
       y = "Introspection Rating") +
  theme_minimal()

t_test_result <- t.test(anchored_affected_introspection, unanchored_introspection)
print(t_test_result)
#p-value = 0.05

t_test_result <- t.test(anchored_introspection, unanchored_introspection)
print(t_test_result)

# 2 associative memory effect ----
## 2.1 do we see the effect? -----------------------------------------------------------------------

#plot
associative_data <- data %>%
  filter(task_name == "associative memory") %>%
  filter(stimulus != "") %>%
  filter(auxiliary_info1 == 'New') %>% 
  mutate(false_alarm = ifelse(choice == "Original", 1, 0))

factor_ex_associative = associative_data %>%
  filter(factor == "Factor-Excluded")

factor_in_associative = associative_data %>%
  filter(factor == "Factor-Included") 


false_alarm_summary <- associative_data %>%
  group_by(factor) %>%
  summarize(
    count_false_alarm = sum(false_alarm),
    n = n(), 
    p = mean(false_alarm),
    se_false_alarm = sqrt(p * (1 - p) / n)  # standard error for binomial 
  )

View(false_alarm_summary)

ggplot(false_alarm_summary, aes(x = factor, y = p)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = p - se_false_alarm, ymax = p + se_false_alarm),
                width = 0.2) +
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

## 2.2 introspection -----------------------------------------------------------------------


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

#View(associative_data)

ggplot(summary_data, aes(x = factor, y = mean_introspect_rating)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Associative Memory Introspection",
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

t_test_result <- t.test(factor_ex_introspect, factor_in_introspect)
print(t_test_result)

#p=0.1

# 3 availability effect -----------------------------------------------------------------------

##3.1 do we see the effect ----

availability_data = data %>%
  filter(task_name == "availability") %>%
  mutate(choice_binary = as.numeric(choice == "List 1"))

ggplot(availability_data, aes(x = factor, fill = choice)) +
  geom_bar(position = "dodge") +
  labs(title = "Availability Effect",
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

## 3.2 introspection TO DO -----------------------------------------------------------------------


data.avail = data %>%
  filter(task_name == "availability") %>%
  group_by(factor) 
  
data.avail.summary = data.avail %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

#View(data.avail)

ggplot(data.avail.summary, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

avail_intro.t <- t.test(data.avail$introspect_rating ~ data.avail$factor)
avail_intro.t
p.vals = c(p.vals, avail_intro.t$p.value)

###** AM: splitting by whether they made choices suggesting that they showed the effect or not ----

data.avail = data.avail %>%
  mutate(choice.matches.condition = ifelse(condition == 'Famous',
                                           choice == 'List 1',
                                           choice == 'List 2'))
data.avail.intro2 = data.avail %>%
  group_by(factor, choice.matches.condition) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(data.avail.intro2, aes(x = factor, y = introspect.m, fill = choice.matches.condition)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

avail_intro2 = lm(introspect_rating ~ factor * choice.matches.condition, data.avail)
summary(avail_intro2)

# transfer to main data for later analyses
for (i in 1:nrow(data)) {
  if (data$task_name[i] == 'availability' & !is.na(data$introspect_rating[i])) {
    which.row = data.avail$subject == data$subject[i]
    if (any(which.row)) {
      data$effect.size.fac[i] = data.avail$choice.matches.condition[which.row]
    }
  }
}

avail_intro2.t = data.avail %>% filter(factor == 'Factor-Included') %$%
  t.test(introspect_rating ~ choice.matches.condition)

print(avail_intro2.t)

# 4. Belief effect ----

#** data preparation ----
data.belief <- data %>% filter(task_name == 'belief')
data.belief2 <- data.belief %>% filter(subject %in% data.belief$subject) %>% 
  mutate(choice.fac = factor(choice), condition.fac = factor(condition, c('Unbelievable', 'Believable')))

data.belief.facinc = data.belief2 %>% filter(factor == "Factor-Included", subject %in% data.belief$subject)

data.belief.include = data.belief %>% filter(subject %in% data.belief$subject) %>%  
  filter(!is.na(choice))

length(unique(data.belief2$subject))

#** data visualization ----
data.bel.graph <- data.belief2 %>% filter(subject %in% data.belief$subject) %>% 
  filter(!is.na(choice)) %>%
  filter(!is.na(condition)) %>% filter(factor == "Factor-Included") #%>%
#filter(auxiliary_info1 == "Valid") ## ADAM: Why filter for only valid here?

ggplot(data.bel.graph, aes(x = condition.fac, fill = choice)) +
  geom_bar(position = "dodge") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))

#** inferential statistics ----
belief <- table(data.belief.include$choice, data.belief.include$condition)
belief
beliefChi <- chisq.test(belief)
beliefChi$expected >= 5
beliefChi

#p-value =  3e-11

#** introspection ratings ----

data.belief.intro = data.belief %>% filter(!is.na(introspect_rating)) %>%
  group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(data.belief.intro, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

belief_intro.t <- t.test(data.belief$introspect_rating ~ data.belief$factor)
belief_intro.t
p.vals = c(p.vals, belief_intro.t$p.value)

#** AM: splitting by whether people showed the effect or not ----
belief_lmer = glmer(choice.fac ~ condition.fac + (condition.fac | subject),
                    data.belief.facinc %>% filter(!is.na(condition)),
                    family = binomial)
summary(belief_lmer)
belief.subj.estimates = coef(belief_lmer)$subject
hist(belief.subj.estimates$condition.fac) 

belief.subj = data.belief.facinc %>%
  mutate(choice.num = as.numeric(choice == 'Yes')) %>%
  group_by(subject, condition.fac) %>%
  summarize(choice.m = mean(choice.num)) %>%
  mutate(choice.diff = choice.m - lag(choice.m)) %>%
  filter(!is.na(choice.diff)) %>%
  select(-c(condition.fac, choice.m))

data.belief.intro2 = data.belief %>%
  filter(!is.na(introspect_rating)) %>%
  filter(subject %in% data.belief$subject)
data.belief.intro2$subj.effect = NA
for (i in 1:nrow(data.belief.intro2)) {
  #which.row = rownames(belief.subj.estimates) == data.belief.intro2$subject[i]
  which.row = belief.subj$subject == data.belief.intro2$subject[i]
  if (any(which.row)) {
    #data.belief.intro2$subj.effect[i] = belief.subj.estimates$condition.facUnbelievable[which.row]
    data.belief.intro2$subj.effect[i] = belief.subj$choice.diff[which.row]
  }
}
data.belief.intro2$subj.effect.fac = data.belief.intro2$subj.effect > 0

belief_intro2.t = t.test(data.belief.intro2$introspect_rating ~ data.belief.intro2$subj.effect.fac)

data.belief.intro.graph2 = data.belief.intro2 %>%
  group_by(factor, subj.effect.fac) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(data.belief.intro.graph2, aes(x = factor, y = introspect.m, fill = subj.effect.fac)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2, position = dodge) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

ggplot(data.belief.intro2, aes(x = subj.effect, y = introspect_rating)) +
  geom_point() +
  geom_smooth(method='lm')

for (i in 1:nrow(data)) {
  if (data$task_name[i] == 'belief' & !is.na(data$introspect_rating[i])) {
    which.row = data.belief.intro2$subject == data$subject[i]
    if (any(which.row)) {
      data$effect.size[i] = data.belief.intro2$subj.effect[which.row]
      data$effect.size.fac[i] = data.belief.intro2$subj.effect.fac[which.row]
    }
  }
}


## 4.1 do we see the effect? TO DO-----------------------------------------------------------------------
## 4.2 introspection TO DO----

# 5. causal inference ----
## 5.1 do we see the effect?TO DO ----
## 5.2 introspectionTO DO----

#6. contact principle ----
## 6.1 do we see the effect? TO DO----
## 6.2 introspection ----

#note: just copied these over from the other doc. Have not looked them over or tweaked them yet. 
contact.data = data %>% 
  filter(task_name == 'contact principle') %>%
  mutate(introspect_rating = as.numeric(introspect_rating))

ggplot(contact.data, aes(x = condition, fill = choice)) +
  geom_bar(position = "dodge", color = 'white') + 
  theme_black()
contact.data.graph = contact.data %>% group_by(condition) %>%
  summarize(choice.m = mean(choice == 'Impermissible'),
            choice.se = se.prop(choice == 'Impermissible'))

ggplot(contact.data.graph, aes(x = condition, y = choice.m, fill = condition)) +
  geom_col(color = 'white') + 
  theme_black() +
  labs(x = 'Condition', y = '% saying the action\nwas morally wrong') +
  geom_errorbar(aes(ymin = choice.m - choice.se,
                    ymax = choice.m + choice.se),
                color = 'white', width = .2) +
  scale_fill_manual(values = c("Contact" = "#4FADEA", "No Contact" = "#FFC000")) +
  theme(legend.position = 'none')

#** inferential statistics ----
contact <- table(contact.data$choice, contact.data$condition)
contact
contactChi <- chisq.test(contact)
contactChi$expected >= 5
contactChi

#** introspection ratings ----

contact.data.graph <- contact.data %>% group_by(factor) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(contact.data.graph, aes(x = factor, y = introspect.m)) +
  geom_col(fill = "lightblue") + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), width = .2) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20)) +
  labs(x = "Test Version")

contact_intro.t <- t.test(contact.data$introspect_rating ~ contact.data$factor)
contact_intro.t

#p=0.2

#AM: splitting by whether they made choices suggesting that they showed the effect or not 

contact.data = contact.data %>%
  mutate(choice.matches.condition = ifelse(condition == 'Contact',
                                           choice == 'Impermissible',
                                           choice == 'Permissible'))
contact.data.graph2 <- contact.data %>%
  group_by(factor, choice.matches.condition) %>%
  summarize(introspect.m = mean(introspect_rating), introspect.se = se(introspect_rating))

ggplot(contact.data.graph2, aes(x = factor, y = introspect.m, fill = choice.matches.condition)) +
  geom_col(position = dodge) + 
  geom_errorbar(aes(ymin = introspect.m - introspect.se, ymax = introspect.m + introspect.se), position = dodge) +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  ) +
  labs(x = "Test Version")

contact_intro2 = lm(introspect_rating ~ factor * choice.matches.condition, contact.data)
summary(contact_intro2)

for (i in 1:nrow(data)) {
  if (data$task_name[i] == 'contact principle' & !is.na(data$introspect_rating[i])) {
    which.row = contact.data$subject == data$subject[i]
    if (any(which.row)) {
      data$effect.size.fac[i] = contact.data$choice.matches.condition[which.row]
    }
  }
}

contact_intro2.t = contact.data %>% filter(factor == 'Factor-Included') %$%
  t.test(introspect_rating ~ choice.matches.condition)

#7. decoy effect ----
## 7.1 do we see the effect? TO DO----
## 7.2 introspection TO DO----

#8. Double effect ----
##8.1 do we see the effect? TO DO----
##8.2 introspection TO DO----

#9. Halo Effect ----
##9.1 do we see the effect? ----


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

summary(lm(choice ~ condition, data = halo_bar_data))


##9.2 introspection TO DO----


summary_halo_data <- data %>%
  filter(task_name == "halo", stimulus == "") %>% 
  group_by(factor) %>%
  summarize(
    mean_choice = mean(as.numeric(introspect_rating), na.rm = TRUE),
    se_choice = se(introspect_rating)
  )

#10 hindsight bias ----
##10.1 do we see the effect? TO DO----
##10.2 introspection TO DO----

#11 mere exposure ----
##11.1 do we see the effect? TO DO----
##11.2 introspection TO DO----

#12 reference price ----
##12.1 do we see the effect? TO DO---
##12.2 introspection TO DO----

#13 representativeness ----
##13.1 do we see the effect TO DO----
##13.2 introspection TO DO----

#14 status quo ----
##14.1 do we see the effect TO DO ----
##14.2 introspection TO DO----





