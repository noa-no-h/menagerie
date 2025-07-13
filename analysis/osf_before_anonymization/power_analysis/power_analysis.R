if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'this.path', 'brms', 'bayestestR', 'rstan', 'posterior', 'parallel', 'doParallel')
p_load(char = pkg.names)

setwd(here())
set.seed(123)
default_priors <- set_prior("normal(0,1)", class = 'b')

se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
dodge <- position_dodge(width=0.9)

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

fill_missing_tasks <- function(df, full_data) {
  completed_tasks <- unique(df$task_name)
  missing_tasks <- setdiff(task_list, completed_tasks)
  
  if (length(missing_tasks) > 0) {
    for (task in missing_tasks) {
      task_data <- full_data %>% filter(task_name == task)
      if (nrow(task_data) > 0) {
        new_row <- task_data %>% slice_sample(n = 1)
      } else {
        # If the task doesn't exist, sample a random row and assign the missing task name
        new_row <- full_data %>% slice_sample(n = 1)
        new_row$task_name <- task
      }
      
      new_row$subject <- unique(df$subject) # Assign to the sampled subject
      df <- bind_rows(df, new_row)
    }
  }
  return(df)
}

## load data
load('../pilot1/pilot1_alltasks.rdata')
load('../pilot2/pilot2_alltasks.rdata')

combined_data_introspection_experience = all_data_introspection_experience_pilot1 %>%
  mutate(introspect_rating = (introspect_rating - 40) / 40,
         study = 'pilot1') %>%
  rbind(all_data_introspection_experience_pilot2 %>% mutate(introspect_rating = introspect_rating / 50, study = 'pilot2')) %>%
  mutate(introspect_rating = scale(introspect_rating),
         effect_size_range = scale(effect_size_range))

all_summary_introspection_experience = combined_data_introspection_experience %>% 
  group_by(showed_effect) %>% 
  filter(!is.na(showed_effect)) %>% 
  summarize(mean_introspect_rating = mean(introspect_rating),
            se_introspect_rating = se(introspect_rating))

# Combined plot 1
plot1= ggplot(all_summary_introspection_experience,
       aes(x = showed_effect, y = mean_introspect_rating, fill = showed_effect)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_introspect_rating - se_introspect_rating, ymax = mean_introspect_rating + se_introspect_rating), width = 0.2) +
  labs(title = "", x = "Influenced by heuristic?", y = "Influence rating") +
  scale_fill_manual(values = effect_no) +
  theme_custom() +
  scale_x_discrete(labels = c('Yes', 'No')) +
  guides(fill = "none") +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_jitter(color = 'darkgray', alpha = 0.1,
              mapping = aes(y = introspect_rating),
              data = combined_data_introspection_experience %>% 
                filter(!is.na(showed_effect)),
              width = .1, height = 0) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  "plot1.pdf",
  plot = plot1,
  device = cairo_pdf,
  width = 8,  
  height = 6, 
  units = "in",
  dpi = 300     
)

# Combined plot 2
plot2=ggplot(combined_data_introspection_experience,
       aes(x = effect_size_range, y = introspect_rating)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm') +
  theme_custom() +
  labs(x = 'Influence magnitude', 
       y = 'Influence rating') +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  "plot2.pdf",
  plot = plot2,
  device = cairo_pdf,
  width = 8, 
  height = 6, 
  units = "in",
  dpi = 300    
)

# Combined plot 3
all_bysubject_introspection_experience = combined_data_introspection_experience %>%
  group_by(subject) %>% 
  summarize(subject_cor = cor(effect_size_range, introspect_rating))

plot3 = ggplot(all_bysubject_introspection_experience, aes(x = subject_cor)) +
  geom_histogram(color = 'black') +
  theme_custom() +
  labs(x = 'Participant-level correlation between\ninfluence ratings and influence magnitudes',
       y = 'Number of subjects') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T), color = 'red') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) - se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = mean(all_bysubject_introspection_experience$subject_cor, na.rm = T) + se(all_bysubject_introspection_experience$subject_cor), color = 'red', linetype = 'dashed') +
  scale_y_continuous(labels = c(), expand = expansion(mult = c(0, 0.05))) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  "plot3.pdf",
  plot = plot3,
  device = cairo_pdf,
  width = 8, 
  height = 6, 
  units = "in",
  dpi = 300    
)

# ## run combined analyses
# 
# combined_analysis_introspection_continuous = brm(introspect_rating ~ effect_size_range + (effect_size_range || subject) + (effect_size_range || task_name),
#                                                  combined_data_introspection_experience %>% mutate(introspect_rating = scale(introspect_rating),
#                                                                                                    effect_size_range = scale(effect_size_range)),
#                                                  prior = default_priors,
#                                                  #save_pars = save_pars(group = F),
#                                                  cores = 4,
#                                                  control = list(adapt_delta = 0.95))
# summarise_draws(combined_analysis_introspection_continuous)
# check_divergences(combined_analysis_introspection_continuous$fit)
# summary(combined_analysis_introspection_continuous)
# hdi(combined_analysis_introspection_continuous)
# 
# save(combined_data_introspection_experience, combined_analysis_introspection_continuous, file='power_analysis_prereq.rdata')

## run power analysis from existing data
load('power_analysis_prereq.rdata')

task_list = c(unique(combined_data_introspection_experience$task_name), paste0("newtask_",1:5))
num_tasks = length(task_list)

original_analysis = T
observer_analysis = F

observer_coefficient_mean = -0.15
observer_coefficient_sd = 0.10
sigma_est <- mean(as_draws_df(combined_analysis_introspection_continuous)$sigma)

sample_sizes = c(400, 600, 800)
num_runs_per = 20
numCores = 20

registerDoParallel(numCores)

results_all = vector(mode = 'list', length = length(sample_sizes))
results_all_obs = vector(mode = 'list', length = length(sample_sizes))

for (i in 1:length(sample_sizes)) {
  sample_size = sample_sizes[i]
  
  new_data_template = combined_data_introspection_experience %>%
    distinct(subject) %>%
    slice_sample(n = sample_size, replace = T) %>%
    group_by(subject) %>%
    mutate(instance = row_number()) %>%
    ungroup() %>%
    left_join(combined_data_introspection_experience, by = 'subject') %>%
    mutate(subject = str_c(subject, instance, sep = "_")) %>%
    group_by(subject) %>% # fill in missing tasks
    group_modify(~ fill_missing_tasks(.x, combined_data_introspection_experience)) %>%
    ungroup() %>%
    select(!c(introspect_rating, instance, study))
  
  post_draws = posterior_predict(combined_analysis_introspection_continuous,
                                 newdata = new_data_template,
                                 ndraws = num_runs_per,
                                 allow_new_levels = T)
  
  new_data_template_obs = rbind(new_data_template %>% mutate(participant_type = 0, actor_id = subject),
                                new_data_template %>% mutate(actor_id = subject, subject = paste0(subject, "_obs"), participant_type = 1)) %>% 
    mutate(effect_size_range = scale(effect_size_range)) %>% 
    group_by(subject) %>% 
    mutate(observer_coefficient = rnorm(1, mean = observer_coefficient_mean, sd = observer_coefficient_sd)) %>% 
    ungroup()
  
  post_draws_obs_orig <- posterior_predict(combined_analysis_introspection_continuous,
                                           newdata = new_data_template_obs,
                                           ndraws = num_runs_per,
                                           allow_new_levels = T)
  
  interaction_term <- new_data_template_obs$observer_coefficient * new_data_template_obs$effect_size_range * new_data_template_obs$participant_type
  post_draws_obs_new <- post_draws_obs_orig + matrix(rep(interaction_term, times = nrow(post_draws_obs_orig)), nrow = nrow(post_draws_obs_orig), byrow = T)
  post_draws_obs_new <- post_draws_obs_new + matrix(rnorm(length(post_draws_obs_new), mean = 0, sd = sigma_est), nrow = nrow(post_draws_obs_new), ncol = ncol(post_draws_obs_new))
  
  if (original_analysis) {
    results = foreach(j = 1:num_runs_per, .combine = "rbind") %dopar% {
      new_data = new_data_template
      new_data$introspect_rating = t(post_draws)[,j]
      
      power_analysis = brm(introspect_rating ~ effect_size_range + (effect_size_range || subject) + (effect_size_range || task_name),
                           new_data %>% mutate(introspect_rating = scale(introspect_rating),
                                               effect_size_range = scale(effect_size_range)),
                           prior = default_priors,
                           save_pars = save_pars(group = F),
                           control = list(adapt_delta = 0.95))
      
      power_analysis_hdi = bayestestR::hdi(power_analysis)
      coef_estimate = summary(power_analysis)$fixed$Estimate[2]
      hdi_high = power_analysis_hdi$CI_high[2]
      hdi_low = power_analysis_hdi$CI_low[2]
      list(coef_estimate, hdi_low, hdi_high)
    }
    
    results_df <- as.data.frame(results)
    results_df = data.frame(lapply(results_df, unlist))
    colnames(results_df) <- c("coef_estimate", "hdi_low", "hdi_high")
    rownames(results_df) <- paste0("Run_", 1:num_runs_per)
    results_all[[i]] = results_df %>% 
      mutate(hdi_width = hdi_high - hdi_low,
             hdi_significant = hdi_low > .05)
  }
  
  if (observer_analysis) {
    results_obs = foreach(j = 1:num_runs_per, .combine = "rbind") %dopar% {
      new_data_obs = new_data_template_obs
      new_data_obs$introspect_rating = t(post_draws_obs_new)[,j]
      
      power_analysis = brm(introspect_rating ~ effect_size_range * participant_type +
                           (effect_size_range * participant_type || actor_id) +
                           (1 | actor_id:subject) +
                           (effect_size_range || task_name),
                           new_data_obs %>% mutate(introspect_rating = scale(introspect_rating),
                                                   effect_size_range = scale(effect_size_range)),
                           prior = default_priors,
                           save_pars = save_pars(group = F))
      
      power_analysis_hdi = bayestestR::hdi(power_analysis)
      coef_estimate = summary(power_analysis)$fixed$Estimate[4]
      hdi_high = power_analysis_hdi$CI_high[4]
      hdi_low = power_analysis_hdi$CI_low[4]
      list(coef_estimate, hdi_low, hdi_high)
    }
    
    results_obs_df <- as.data.frame(results_obs)
    results_obs_df = data.frame(lapply(results_obs_df, unlist))
    colnames(results_obs_df) <- c("coef_estimate", "hdi_low", "hdi_high")
    rownames(results_obs_df) <- paste0("Run_", 1:num_runs_per)
    results_all_obs[[i]] = results_obs_df %>% 
      mutate(hdi_width = hdi_high - hdi_low,
             hdi_significant = hdi_high < -.05)
  }
}

save(sample_sizes, num_runs_per, observer_coefficient_mean, observer_coefficient_sd, results_all, results_all_obs, file='power_analysis_results_original_null.rdata')

# analyze
lapply(results_all, colMeans)
lapply(results_all_obs, colMeans)
