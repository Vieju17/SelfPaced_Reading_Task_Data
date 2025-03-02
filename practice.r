# Library Import
library("tidyverse")
library("ggthemes")

# Data Import
df1 <- select(read.csv("./data/7co.csv"), part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df2 <- select(read.csv("./data/ayb.csv"), part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df3 <- select(read.csv("./data/ggn.csv"), part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df4 <- select(read.csv("./data/h1n.csv"), part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df5 <- select(read.csv("./data/qkh.csv"), part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt, correct_answer, answer)
df6 <- select(read.csv("./data/y8r.csv"), part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt, correct_answer, answer)

# Data Cleaning
df_raw <- reduce(list(df1, df2, df3, df4, df5, df6), rbind)
summary(df_raw)

df_clean <- df_raw[order(df_raw$part_id), ] %>%
    filter(task == "item" | task == "question") %>%
    mutate(across(c(lang_ca, lang_en, lang_sp, correct_answer, answer), as.logical)) %>%
    mutate(rt = as.numeric(rt))
summary(df_clean)

df_clean$lang_ca[is.na(df_clean$lang_ca)] <- FALSE
df_clean$lang_sp[is.na(df_clean$lang_sp)] <- FALSE
df_clean$lang_en[is.na(df_clean$lang_en)] <- FALSE

Q1 <- quantile(df_clean$rt, 0.25) # nolint
Q3 <- quantile(df_clean$rt, 0.75) # nolint
IQR <- Q3 - Q1 # nolint
inner_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

df_rt <- filter(select(df_clean, part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt), task == "item" & item_type == "exp_item") %>%
    filter(rt > inner_bound & rt < upper_bound) %>%
    mutate(rt = log(rt, base = 2))
summary(df_rt)

df_acc <- filter(select(df_clean, part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, correct_answer, answer), task == "question") %>%
    mutate(correct = ifelse(correct_answer == answer, TRUE, FALSE))
summary(df_acc)

# Data Analysis
lm_rt <- lm(data = df_rt, rt ~ clause_type + number)
summary(lm_rt)

lm_acc <- lm(data = df_acc, correct ~ clause_type + number)
summary(lm_acc)

# Data Visualization
plot_rt <- df_rt %>%
    ggplot(aes(
        x = clause_type,
        y = rt,
        color = clause_type,
        fill = clause_type
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
