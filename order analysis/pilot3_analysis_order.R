# Setup -------------------------------------------------------------------
if (!require('pacman')) {
  install.packages('pacman')
  require('pacman')
}

pkg.names = c('ggplot2', 'tidyverse', 'RColorBrewer', 'extrafont',
              'this.path', 'brms', 'bayestestR', 'rstan', 'posterior', 'parallel', 'doParallel', 'emmeans')
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

data <- read.csv('april_order_data.csv') %>%
  filter(subject != "") %>%
  arrange(subject, task_name) %>%
  mutate(factor = recode(factor, "F" = "Factor-Included"))%>%
  mutate(factor = factor(factor, c("Factor-Included", "Factor-Excluded"), c("experience", "control"))) %>%
  filter(version == "pilot3b") 

subjects_all = data %>%
  pull(subject) %>%
  unique()

#find subjects who need to be excluded

attention_exclude <- data %>%
  filter((`task_name` == "attention check 2" & `auxiliary_info1` == "Failure") |
           (`task_name` == "attention check 3" & `auxiliary_info1` == "Incorrect")) %>%
  pull(subject)

events <- read.csv('browser_events.csv') %>%
  arrange(subject)

events_subj <- events %>%
  filter(browser_event == "blur") %>%
  filter(version == "pilot3b") %>%
  group_by(subject) %>%
  summarize(blurs = n())

ggplot(events_subj, aes(x = blurs)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(title = "Blur Histogram", x = "Number of Blurs", y = "Count") +
  theme_custom()

tab_away_exclude <- events_subj %>% 
  filter(blurs > 20) %>%
  pull(subject)

to_exclude <- union(attention_exclude, tab_away_exclude)

number_subjects <- n_distinct(data$subject)
number_to_exclude <- length(to_exclude)
print(number_subjects)
print(number_to_exclude)

data <- data %>%
  filter(!subject %in% to_exclude)

#font_import(pattern = "Optima", prompt = FALSE)
loadfonts(device = "pdf")

# Order effect ----

primacy_data <- data %>%
  filter(task_name == "primacy order") %>%
  filter(version == "pilot3b") %>%
  filter(choice != "")%>%
  mutate(factor = recode(factor, "F" = "Factor-Included"))%>%
  mutate(choice_fac = ifelse(choice == "car1",
                         "chose primacy car",
                         "chose other car")) %>%
  mutate(chose_primacy_car = as.numeric(choice_fac == "chose primacy car")) %>%
  mutate(chose_car_2 = ifelse(choice == "car2",
                         1,
                         0)) %>%
  mutate(chose_car_3 = ifelse(choice == "car3",
                         1,
                         0)) 

primacy_graph_data <- primacy_data %>%
  group_by(choice) %>%
  summarise(count = n()) %>%
  mutate(percent = (count / sum(count)) * 100)

# Plot the data
ggplot(primacy_graph_data, aes(x = choice, y = percent, fill = choice)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Percentage of Choices for Each Car",
    x = "Car Choice",
    y = "Percentage",
    fill = "Car"
  ) +
  theme_custom()+
  guides(fill = FALSE)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


# Prepare data for brms
data <- data.frame(
  success = sum(primacy_data$choice_binary),    # Number of successes (1s)
  trials = length(primacy_data$choice_binary)   # Total number of trials
)

# Fit a Bayesian binomial model
fit <- brm(
  success | trials(trials) ~ 1,          # Model formula: proportion of successes
  data = data,
  family = binomial(link = "identity"), # Binomial likelihood with identity link
  prior = prior(beta(1, 1), class = "Intercept"), # Uniform prior on proportion
  iter = 2000, chains = 4               # Number of iterations and chains
)

# Summary of the model
summary(fit)


binary_primacy_data <- primacy_data %>%
  filter(choice != "car3")
  mutate(car_1_or_2 = ifelse(choice == "car1", 1, 0)) %>%
  select(car_1_or_2) 
  

# Prepare data for brms
data <- data.frame(
  success = sum(primacy_data$car_1_or_2),    # Number of successes (1s)
  trials = length(primacy_data$car_1_or_2)   # Total number of trials
)

# Fit a Bayesian binomial model
fit <- brm(
  success | trials(trials) ~ 1,          # Model formula: proportion of successes
  data = data,
  family = binomial(link = "identity"), # Binomial likelihood with identity link
  prior = prior(beta(1, 1), class = "Intercept"), # Uniform prior on proportion
  iter = 2000, chains = 4               # Number of iterations and chains
)

# Summary of the model
summary(fit)


# Save image --------------------------------------------------------------

save.image('pilot3_output.rdata')
