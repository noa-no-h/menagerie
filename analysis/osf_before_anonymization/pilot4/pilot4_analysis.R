# Setup -------------------------------------------------------------------
if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'lme4', 'lmerTest', 'tidyverse', 'RColorBrewer', 'afex', 'this.path', 'brms', 'bayestestR')
p_load(char = pkg.names)

setwd(here())
set.seed(123)

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
se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}

# Load data ---------------------------------------------------------------

fields = c('Judgment and decision-making',
           'Cognitive psychology',
           'Social psychology',
           'Developmental psychology',
           'Psychology (other)',
           'Behavioral economics',
           'Economics (other)',
           'Other')

df = read.csv('pilot4_data.csv', header = T) %>%
  mutate(Progress = as.numeric(Progress)) %>% 
  filter(DistributionChannel == 'anonymous', Progress >= 90) %>% 
  select(Subject = ResponseId,
         Role = Q11,
         Field = Q24,
         Gender = q27,
         Age = Q28,
         starts_with('heuristic')) %>%
  mutate(Role.fac = factor(Role, c('Graduate student', 'Postdoc', 'Assistant Professor', 'Associate Professor', 'Full Professor', 'Not in academia', 'Other')),
         Gender.fac = factor(Gender, c('Man', 'Woman', 'Some other way')),
         Age = as.numeric(Age),
         Field.fac = factor(Field, fields))

for (field in fields) {
  df = df %>% mutate("{field}" := grepl(field, df$Field, fixed = T))
}

heuristic_cols = 6:22
heuristic_labels = c('Extremely unlikely', 'Moderately unlikely', 'Slightly unlikely', 'Neither likely nor unlikely', 'Slightly likely', 'Moderately likely', 'Extremely likely')
for (i in heuristic_cols) {
  df[,i] = factor(df[,i], heuristic_labels, 1:length(heuristic_labels))
}

heuristic_names = c("Anchoring", "Availability heuristic", "Belief bias", "Causal judgment: Abnormal selection", "Decoy effect", "DRM effect", "Halo effect", "Hindsight bias", "Illusion of truth", "Imaginability bias", "Mere exposure effect", "Omission effect in moral judgment", "Recognition heuristic", "Reference price effect", "Representativeness heuristic", "Status quo bias", "Sunk cost bias")

df.long = df %>% 
  pivot_longer(cols = all_of(heuristic_cols),
               names_to = "heuristic_index",
               values_to = "heuristic_prediction") %>% 
  mutate(heuristic_name = factor(heuristic_index, paste0("heuristic_", 1:17), heuristic_names),
         heuristic_prediction = as.numeric(heuristic_prediction)) %>% 
  filter(heuristic_name != 'Imaginability bias') # we ended up dropping this from our set of heuristics/biases after running this pilot because we could not replicate the effect

# Sample details ----------------------------------------------------------
nrow(df) # number of subjects
table(df$Role.fac) # split by academic position

# Analyze heuristic data --------------------------------------------------

# analyze average overall response, aggregating across heuristics
mean(df.long$heuristic_prediction, na.rm = T)
analysis.overall = brm(heuristic_prediction ~ 1 + (1 | Subject) + (1 | heuristic_name),
                       df.long)
summary(analysis.overall)
hdi(analysis.overall)

# split by heuristic
df.byheuristic = df.long %>% 
  filter(heuristic_name != 'DRM effect') %>% 
  group_by(heuristic_name) %>% 
  summarize(mean_prediction = mean(heuristic_prediction, na.rm = T),
            se_prediction = se(heuristic_prediction),
            pct.high.predictions = mean(heuristic_prediction >= 6, na.rm = T),
            pct.low.predictions = mean(heuristic_prediction <= 2, na.rm = T),
            test.t = t.test(heuristic_prediction, mu = 4)$statistic,
            test.df = t.test(heuristic_prediction, mu = 4)$parameter,
            test.p = t.test(heuristic_prediction, mu = 4)$p.value)

ggplot(df.byheuristic %>% filter(!(heuristic_name %in% c('DRM effect', 'Hindsight bias', 'Status quo bias', 'Sunk cost bias'))), aes(x = heuristic_name, y = mean_prediction)) +
  geom_jitter(data = df.long %>% filter(!(heuristic_name %in% c('DRM effect', 'Hindsight bias', 'Status quo bias', 'Sunk cost bias'))), aes(y = heuristic_prediction),
              width = 0, alpha = 0.5, height = 0.2,
              color = 'white') +
  geom_point(size = 5, color = 'red') +
  geom_errorbar(aes(ymin = mean_prediction - 1.96*se_prediction,
                    ymax = mean_prediction + 1.96*se_prediction),
                width = 0.2, color = 'red') +
  theme_black() +
  geom_hline(yintercept = 4, linetype = 'dashed', color = 'white') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(limits = c(1,7), breaks = c(1,4,7), labels = c('Extremely unlikely', 'Neither likely\nnor unlikely', 'Extremely likely')) +
  labs(x = '', y = 'Prediction')

# split by subject
df.bysubject = df.long %>% 
  group_by(Subject) %>% 
  summarize(
    mean.prediction = mean(heuristic_prediction, na.rm = T),
    se.prediction = se(heuristic_prediction),
    any.above.midpoint = any(heuristic_prediction > 4, na.rm = T),
    prediction.range = range(heuristic_prediction,na.rm = T)[2] - range(heuristic_prediction, na.rm = T)[1]) %>% 
  filter(!is.infinite(prediction.range))
hist(df.bysubject$prediction.range)
mean(df.bysubject$prediction.range)
mean(df.bysubject$mean.prediction > 4)
mean(df.bysubject$any.above.midpoint, na.rm = T)

summary(lmer(heuristic_prediction ~ 1 + (1 | Subject) + (1 | heuristic_name),
             df.long %>% mutate(heuristic_prediction = heuristic_prediction - 4)))

# split by role
df.byroleandsubject = df.long %>% 
  filter(!is.na(Role.fac)) %>% 
  group_by(Role.fac, Subject) %>% 
  summarize(heuristic_prediction.m = mean(heuristic_prediction, na.rm = T)) %>% 
  filter(!is.nan(heuristic_prediction.m))
df.byrole = df.byroleandsubject %>% 
  group_by(Role.fac) %>% 
  summarize(mean_prediction = mean(heuristic_prediction.m, na.rm = T),
            se_prediction = se(heuristic_prediction.m))

ggplot(df.byrole %>% filter(Role.fac != 'Other'), aes(x = Role.fac, y = mean_prediction)) +
  geom_jitter(data = df.byroleandsubject %>% filter(Role.fac != 'Other'), mapping = aes(y = heuristic_prediction.m),
              width = 0, alpha = 0.5, height = 0.2, color = 'white') +
  geom_point(size = 5, color = 'red') +
  geom_errorbar(aes(ymin = mean_prediction - 1.96*se_prediction,
                    ymax = mean_prediction + 1.96*se_prediction),
                width = 0.2, color = 'red') +
  theme_black() +
  geom_hline(yintercept = 4, linetype = 'dashed', color = 'white') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(limits = c(1,7), breaks = c(1,4,7), labels = c('Extremely unlikely', 'Neither likely\nnor unlikely', 'Extremely likely')) +
  labs(x = '', y = 'Prediction')

summary(lmer_alt(scale(heuristic_prediction) ~ Role.fac +
               (1 | Subject) +
               (Role.fac || heuristic_name),
             df.long))

# Compare to results of pilots 1 & 2 --------------------------------------

load('../pilot1/pilot1_alltasks.rdata')
load('../pilot2/pilot2_alltasks.rdata')

all_data_introspection_experience = all_data_introspection_experience_pilot1 %>% 
  rbind(all_data_introspection_experience_pilot2)

all_bytask_introspection_experience = all_data_introspection_experience %>% 
  group_by(task_name) %>% 
  summarize(task_cor = cor(introspect_rating, effect_size_range, use = 'pairwise.complete.obs'),
            task_cor_se = cor.test())

df.byheuristic.filt = df.byheuristic %>%
  filter(!(heuristic_name %in% c('DRM effect', 'Hindsight bias', 'Status quo bias', 'Sunk cost bias')))
df.byheuristic.filt$actual_cor = all_bytask_introspection_experience$task_cor

ggplot(df.byheuristic.filt, aes(x = mean_prediction, y = actual_cor)) +
  geom_point(size = 3, color = 'white') +
  geom_smooth(method = 'lm') +
  theme_black() +
  labs(y = 'Task-level awareness',
       x = 'Predicted likelihood\nof awareness')


ggplot(df.byheuristic.filt, aes(x = heuristic_name, y = actual_cor)) +
  geom_point(size = 5, color = 'red') +
  #geom_errorbar(aes(ymin = mean_prediction - 1.96*se_prediction,
  #                  ymax = mean_prediction + 1.96*se_prediction),
  #              width = 0.2, color = 'red') +
  theme_black() +
  #geom_hline(yintercept = 4, linetype = 'dashed', color = 'white') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  #scale_y_continuous(limits = c(1,7), breaks = c(1,4,7), labels = c('Extremely unlikely', 'Neither likely\nnor unlikely', 'Extremely likely')) +
  labs(x = '', y='') +
  #scale_x_discrete(labels = NULL, breaks = NULL) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))

analysis.byheuristic = brm(mean_prediction ~ actual_cor,
                            df.byheuristic.filt %>% mutate(mean_prediction = scale(mean_prediction),
                                                           actual_cor = scale(actual_cor)),
                            prior = set_prior("normal(0,1)", class = 'b'),
                            save_pars = save_pars(group = F))
summary(analysis.byheuristic)
hdi(analysis.byheuristic)

# Save output --------------------------------------------------------------

save.image('pilot4_output.rdata')
