# Libraries
library(tidyverse)
library(ggthemes)
library(lme4)
library(lmerTest)

# Data Import
df_raw <- read_tsv("./data/eye_tracker1.tsv")
summary(df_raw)

# Data Cleaning
q1 <- quantile(df_raw$Average_duration_of_fixations, 0.25, na.rm = TRUE)
q3 <- quantile(df_raw$Average_duration_of_fixations, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

df_clean <- df_raw %>%
    select(c(Participant, clause_type, number, Stimulus, Total_duration_of_fixations, Average_duration_of_fixations, Number_of_fixations, Duration_of_first_fixation, Number_of_saccades_in_AOI)) %>% # Select relevant columns
    na.omit() %>% # Remove rows with missing values
    mutate(Stimulus = ifelse(Stimulus == "s1", 1, ifelse(Stimulus == "s2", 2, ifelse(Stimulus == "s3", 3, ifelse(Stimulus == "s4", 4, 5))))) %>% # Encode segments as numbers
    mutate(number = ifelse(number == "ss", 1, ifelse(number == "pp", 2, ifelse(number == "sp", 3, 4)))) %>% # Encode number as numbers
    mutate(clause_type = ifelse(clause_type == "sr", 1, 2)) %>% # Encode clause type as numbers
    filter(Average_duration_of_fixations > lower_bound & Average_duration_of_fixations < upper_bound) %>% # Removing outliers
    mutate(Average_duration_of_fixations = log(Average_duration_of_fixations)) %>% # Use logarithmic scale to normalize data
    filter(is.finite(Average_duration_of_fixations)) # Remove infinite values

df_norm <- ggplot(df_clean, aes(x = Average_duration_of_fixations)) +
    geom_density() # Checking for normality
df_norm

# Linear Regression Model
df_lm <- lmer(
    data = df_clean,
    Average_duration_of_fixations ~ clause_type + number + Stimulus + (1 | Stimulus)
)
summary(df_lm)

# Data Visualization
df_boxplot <- df_clean %>%
    ggplot(aes(x = clause_type, y = Average_duration_of_fixations)) +
    geom_boxplot()
df_boxplot
