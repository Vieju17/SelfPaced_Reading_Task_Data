# Library Import
library(tidyverse)
library(ggthemes)

# Data Import
df_raw <- reduce(list(
    select(read.csv("./data/7yo.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/8vw.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/cdj.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/cz0.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/dgr.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/oeo.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/qpv.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/tds.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/vbv.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer),
    select(read.csv("./data/kjd.csv"), part_id, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
), rbind)
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
lm_rt <- lm(data = df_rt, rt ~ segment + clause_type + number)
summary(lm_rt)

lm_acc <- lm(data = df_acc, correct ~ clause_type + number)
summary(lm_acc)

# Data Visualization
lmplot_rt <- df_rt %>%
    ggplot(aes(x = segment, y = rt, color = clause_type)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_calc(base_size = 12, base_family = "Verdana") +
    theme(
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
    ) +
    labs(
        title = "Reaction Time by Segment",
        x = "Segment",
        y = "Reaction Time (ms)",
    )
lmplot_rt

boxplot_rt <- df_rt %>%
    ggplot(aes(
        y = rt,
        color = segment,
        fill = segment
    )) +
    facet_wrap(~segment) +
    aes(color = as.factor(segment), fill = as.factor(segment)) +
    geom_boxplot(
        notch = TRUE,
        notchwidth = 0.5,
        staplewidth = 0.5,
        show.legend = FALSE,
        outlier.alpha = 0.5,
        linewidth = 1.5,
        alpha = 0.5,
    ) +
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
boxplot_rt

plot_acc <- df_acc %>%
    ggplot(aes(x = clause_type)) +
    geom_bar()
plot_acc
