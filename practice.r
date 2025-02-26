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
df <- reduce(list(df1, df2, df3, df4, df5, df6), rbind)
df_clean <- df[order(df$part_id), ] %>%
    filter(task == "item" | task == "question") %>%
    mutate(across(c(lang_ca, lang_en, lang_sp, correct_answer, answer), as.logical))

df_clean$lang_ca[is.na(df_clean$lang_ca)] <- FALSE
df_clean$lang_sp[is.na(df_clean$lang_sp)] <- FALSE
df_clean$lang_en[is.na(df_clean$lang_en)] <- FALSE
