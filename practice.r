# Library Import
library(tidyverse)
library(ggthemes)

# Data Import
sample <- read.csv("./data/0x0.csv")
head(sample)

df1 <- select(read.csv("./data/0x0.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df2 <- select(read.csv("./data/bzm.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df3 <- select(read.csv("./data/dj9.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df4 <- select(read.csv("./data/r5t.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df5 <- select(read.csv("./data/s62.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer)

df_raw <- reduce(list(df1, df2, df3, df4, df5), rbind)
summary(df_raw)

# Data Cleaning
df_clean <- df_raw[order(df_raw$part_id), ] %>% # Order by participant ID
    filter((task == "item" | task == "question") & segment != "s0" & item_type != "pr_item") %>% # Remove fixations and s0 segments
    mutate(rt = as.numeric(rt)) %>% # Making RTs a numeric value
    mutate(clause_type = ifelse(clause_type == "sr", 1, 2)) %>% # Recoding clause type
    mutate(number = ifelse(number == "ss", 1,
        ifelse(number == "pp", 2,
            ifelse(number == "sp", 3, 4)
        )
    )) %>% # Making number a numeric value
    mutate(segment = ifelse(segment == "s1", 1,
        ifelse(segment == "s2", 2,
            ifelse(segment == "s3", 3,
                ifelse(segment == "s4", 4, 5)
            )
        )
    )) # Making number a numeric value
summary(df_clean)

# Outlier Detection Variables
Q1 <- quantile(df_clean$rt, 0.25) # nolint
Q3 <- quantile(df_clean$rt, 0.75) # nolint
IQR <- Q3 - Q1 # nolint
inner_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Data Frame for Performance
df_rt <- filter(select(df_clean, part_id, task, item_type, clause_type, number, segment, rt), task == "item" & item_type == "exp_item") %>%
    filter(rt > inner_bound & rt < upper_bound) %>%
    mutate(rt = log(rt, base = 2))
summary(df_rt)

# Data Frame for Accuracy
df_acc <- filter(select(df_clean, part_id, task, item_type, clause_type, number, correct_answer, answer), task == "question") %>%
    mutate(correct = ifelse(correct_answer == answer, 0, 1))
summary(df_acc)

# Data Analysis
lm_rt <- lm(data = df_rt, rt ~ clause_type + number + segment)
summary(lm_rt)

lm_acc <- lm(data = df_acc, correct ~ clause_type + number)
summary(lm_acc)

# Data Visualization
plot_rt <- df_rt %>%
    ggplot(aes(
        x = clause_type,
        y = rt,
        color = number,
        fill = number
    )) +
    geom_boxplot(
        notch = TRUE,
        notchwidth = 0.5,
        staplewidth = 0.5,
        show.legend = FALSE,
        outlier.alpha = 0.5,
        linewidth = 1.5,
        alpha = 0.5,
    ) +
    facet_wrap(~number) +
    theme_calc(base_size = 12, base_family = "Verdana") +
    theme(
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
    ) +
    labs(
        title = "Reaction Time by Clause Type",
        x = "Clause Type",
        y = "Reaction Time (ms)",
    )
plot_rt

plot_acc <- df_acc %>%
    ggplot(aes(x = clause_type)) +
    geom_bar()
plot_acc
