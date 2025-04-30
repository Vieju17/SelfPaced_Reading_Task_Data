library(tidyverse)
library(ggthemes)
library(lmerTest)
library(gt)

# IMPORTING AND PROCESSING DATA
# Example Data Set
df_example <- read.csv("./data/32.csv")
summary(df_example)

# Importing and Merging all Data Sets
df_raw <- reduce(
    list(
        select(read.csv("./data/32.csv"), part_id, task, item_number, item_type, clause_type, number, segment, rt, correct)
    ),
    rbind
)
summary(df_raw)

# Cleaning General Data
df_clean <- df_raw %>%
    .[order(.$part_id), ] %>%
    filter(.$item_type == "exp_item" & .$task == "item") %>%
    mutate(
        item_number = as.numeric(item_number),
        task = as.factor(task),
        item_type = as.factor(item_type),
        clause_type = as.factor(clause_type),
        number = as.factor(number),
        matching = as.factor(ifelse(number == "ss" | number == "pp", "match", "mismatch")),
        segment = as.factor(segment)
    ) %>%
    relocate(matching, .after = number)
summary(df_clean)

# Checking Normality
skewed_plot <- df_clean %>%
    ggplot(aes(x = rt)) +
    geom_density() +
    theme_calc()
skewed_plot

# Removing Outliers
q1 <- quantile(df_clean$rt, 0.25)
q3 <- quantile(df_clean$rt, 0.75)
iqr <- q3 - q1
lower_limit <- q1 - iqr * 1.5
upper_limit <- q3 + iqr * 1.5

df_clean <- df_clean %>%
    filter(.$rt > lower_limit & .$rt < upper_limit)
summary(df_clean)

no_outlier_plot <- df_clean %>%
    ggplot(aes(x = rt)) +
    geom_density() +
    theme_calc()
no_outlier_plot

# Normalizing Data
df_clean <- df_clean %>%
    mutate(rt = log(rt))
summary(df_clean)

normal_plot <- df_clean %>%
    ggplot(aes(x = rt)) +
    geom_density() +
    theme_calc()
normal_plot

# REGRESSION MODELS
lm_clause <- lmer(data = df_clean, rt ~ clause_type + (1 | item_number)) # ADD + (1 | part_id)
summary(lm_clause)

lm_matching <- lmer(data = df_clean, rt ~ clause_type * matching + (1 | item_number)) # ADD + (1 | part_id)
summary(lm_matching)

lm_segment <- lmer(data = df_clean, rt ~ clause_type * segment + (1 | item_number)) # ADD + (1 | part_id)
summary(lm_segment)

qqnorm(resid(lm_clause))
qqline(resid(lm_clause))

# VISUALIZING DATA
# Plots
lm_clause_plot <- df_clean %>%
    ggplot(aes(y = rt, x = clause_type, color = clause_type)) +
    geom_boxplot(show.legend = FALSE, staplewidth = 0.5, notch = TRUE) +
    theme_calc()
lm_clause_plot

lm_segment_plot <- df_clean %>%
    filter(segment == "s3" | segment == "s4" | segment == "s5") %>%
    ggplot(aes(y = rt, x = segment, color = segment)) +
    facet_wrap(~clause_type) +
    geom_boxplot(notch = TRUE, staplewidth = 0.5, show.legend = FALSE) +
    theme_calc()
lm_segment_plot

lm_matching_plot <- df_clean %>%
    ggplot(aes(y = rt, x = matching, color = matching)) +
    geom_boxplot(notch = TRUE, staplewidth = 0.5, show.legend = FALSE) +
    facet_wrap(df_clean$clause_type) +
    theme_calc()
lm_matching_plot

# Tables
