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

df_clean <- df_raw[order(df_raw$part_id), ] %>%
    filter(task == "item" | task == "question") %>%
    mutate(across(c(lang_ca, lang_en, lang_sp, correct_answer, answer), as.logical))

df_clean$lang_ca[is.na(df_clean$lang_ca)] <- FALSE
df_clean$lang_sp[is.na(df_clean$lang_sp)] <- FALSE
df_clean$lang_en[is.na(df_clean$lang_en)] <- FALSE

df_rt <- filter(select(df_clean, part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, segment, rt), task == "item")

df_acc <- filter(select(df_clean, part_id, gender, age, lang_ca, lang_sp, lang_en, task, item_type, clause_type, number, correct_answer, answer), task == "question") %>%
    mutate(correct = ifelse(correct_answer == answer, TRUE, FALSE))

# Data Analysis
rt_lm <- lm(data = df_rt, rt ~ clause_type + number)
summary(rt_lm)

acc_lm <- lm(data = df_acc, correct ~ clause_type + number)
summary(acc_lm)
