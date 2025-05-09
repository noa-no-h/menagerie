if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'this.path', 'brms', 'bayestestR', 'rstan', 'posterior', 'parallel', 'doParallel')
p_load(char = pkg.names)

setwd(here())
set.seed(123)
default_priors <- set_prior("normal(0,1)", class = 'b')

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
# load('../pilot1/pilot1_alltasks.rdata')
# load('../pilot2/pilot2_alltasks.rdata')
# 
# combined_data_introspection_experience = all_data_introspection_experience_pilot1 %>% 
#   mutate(introspect_rating = (introspect_rating - 50) / 40,
#          study = 'pilot1') %>% 
#   rbind(all_data_introspection_experience_pilot2 %>% mutate(introspect_rating = introspect_rating / 50, study = 'pilot2')) %>% 
#   mutate(introspect_rating = scale(introspect_rating),
#          effect_size_range = scale(effect_size_range))
# 
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
