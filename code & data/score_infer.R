library(openxlsx)
library(tidyverse)
library(readr)
library(stringr)
library(textclean)
library(tidytext)
library(RcppMeCab)
library(tidyr)
library(jsonlite)
setwd("C:/Users/hwwe1/Desktop/ytb_tm_files/project_mj/train")

setwd('C:\\Users\\hwwe1\\Desktop\\ytb_tm_files\\project_mj\\train\\vader & blob')
intergrated_scores3 <- tibble(read.xlsx("intergrated_scores3.xlsx"))

all_correct1 <- intergrated_scores3 %>%
  filter((binary_nsr == binary_nsmc) & 
           (binary_nsmc == category_blob) &
           (category_blob == category_vader))

all_correct1 <- all_correct1[c(1:5, 7)] %>%
  rename(label = binary_nsr)


all_correct2 <- intergrated_scores3 %>%
  filter((binary_nsr_past == binary_nsmc) & 
           (binary_nsmc == category_blob) &
           (category_blob == category_vader))
all_correct2 <- all_correct2[1:6] %>%
  rename(label = binary_nsr_past)

intergrated_scores3 %>% 
  count(category_vader)

intergrated_scores3 %>% 
  count(category_vader, category_blob)


all_correct2 %>%
  count(label) %>%
  mutate(ratio = n/sum(n) * 100)


all_corct_sub <- all_correct2 %>%
  sample_n(size = 30000)

all_corct_sub %>%
  count(label) %>%
  mutate(ratio = n/sum(n) * 100)

sub1 <- all_corct_sub %>% filter(label == 0) %>% sample_n(size = 10000)
sub2 <- all_corct_sub %>% filter(label == 1) %>% sample_n(size = 10000)

corct_sub <- rbind(sub1,sub2)
corct_sub

write.xlsx(corct_sub, "corct_sub.xlsx")

test_unseen <- all_correct2 %>% 
  sample_n(size = 40000)

test_unseen %>%
  count(label) %>%
  mutate(ratio = n/sum(n) * 100)

test_unseen <- test_unseen %>%
  arrange(label)

write.xlsx(test_unseen, "test_unseen.xlsx")

pr <- tibble(read.xlsx("pr.xlsx"))

pr <- pr %>% 
  arrange(real_label)
write.xlsx(pr, "pr2.xlsx")


pr2 <- tibble(read.xlsx("pr2.xlsx"))

# 긍정 사전 호출
pos_words_not_dict <- read_json("pos_words(not_dict).json")
pos_words_not_dict <- paste0(pos_words_not_dict)


# 부정 사전 호출
neg_words <- read_json("neg_words.json")
neg_words <- paste0(neg_words)


pr2 %>%
  filter((real_label == 0) & (str_detect(comment, pos_words_not_dict))) %>%
  select(comment, real_label)

View(pr2 %>%
       filter((real_label == 0) & (str_detect(comment, pos_words_not_dict))) %>%
       select(comment, real_label))



pr2_pos <- pr2 %>%
  filter(real_label == 1)

pr2_neg <- pr2 %>%
  filter(real_label == 0)

#-----------------------------------------------------
pr2 %>%
  filter(real_label == 0) %>%
  filter(str_detect(comment, "포상")) %>%
  select(comment, real_label) %>% 
  print(n=Inf)

pr2_neg %>%
  filter(str_detect(comment, "감동")) %>%
  select(comment, real_label) %>% 
  print(n=Inf)

pr2_neg <- pr2_neg %>%
  mutate(real_label = ifelse(str_detect(comment, "감동"), 1, real_label))

pr2_neg %>%
  count(real_label)

write.xlsx(rbind(pr2_pos,pr2_neg),"pr2_revised1.xlsx")


pr2_neg %>%
  mutate(real_label = ifelse(str_detect(comment, "어벤져스"), 1, real_label)) %>%
  filter(str_detect(comment, "어벤져스")) %>% print(n=Inf)
#------------------------------------

pr %>%
  count(category_vader)

pr2 %>%
  count(real_label)



pr2 <- pr2 %>%
  filter(!str_detect(comment, "동물"))
write.xlsx(pr2,"pr2_revised4.xlsx")

pr2 %>%
  filter(str_detect(comment, "동물")) %>%
  print(n=Inf)
#=======================
pr2 <- tibble(read.xlsx("pr2_revised4.xlsx"))

pr2_pos <- pr2 %>%
  filter(real_label == 1)

pr2_neg <- pr2 %>%
  filter(real_label == 0)


write.xlsx(rbind(pr2_pos,pr2_neg),"pr2_revised4.xlsx")


pr2 %>%
  filter(real_label == 0) %>%
  filter(str_detect(comment, "유공")) %>%
  select(comment, real_label) %>% 
  print(n=Inf)

pr2_neg %>%
  filter(str_detect(comment, "유공")) %>%
  select(comment, real_label) %>% 
  print(n=Inf)

pr2_neg <- pr2_neg %>%
  mutate(real_label = ifelse(str_detect(comment, "유공"), 1, real_label))

pr2_neg %>%
  count(real_label)




pr2 %>%
  filter(real_label == 1) %>%
  filter(str_detect(comment, "상어")) %>%
  select(comment, real_label) %>% 
  print(n=Inf)

pr2_pos %>%
  filter(str_detect(comment, "믿거")) %>%
  select(comment, real_label) %>% 
  print(n=Inf)

pr2_pos <- pr2_pos %>%
  mutate(real_label = ifelse(str_detect(comment, "믿거"), 0, real_label))

pr2_pos %>%
  count(real_label)


pr2 %>%
  filter((vader_polarity == 0) & (blob_polarity == 0)) %>%
  select(comment, vader_polarity, blob_polarity, real_label) %>%
  sample_n(size = 100) %>% 
  print(n=Inf)


pr2 %>%
  count(real_label,prediction_label)

pr2 %>%
  count(real_label,binary_nsmc)

pr2 %>%
  count(real_label,binary_nsr)


pr2 %>%
  count(real_label,category_vader)                      


pr2 %>%
  count(real_label,category_blob)             







pr2 <- tibble(read.xlsx("pr2_revised4.xlsx"))




pr2 %>%
  filter(str_detect(comment, "성이름")) %>%
  mutate(comment = str_replace_all(comment, "성이름", " "),
         comment = str_squish(comment))
         


pr2 <- pr2 %>%
  mutate(comment = str_replace_all(comment, "성이름", " "),
         comment = str_squish(comment))


write.xlsx(pr2,"pr2_revised4.xlsx")


pr2 %>% 
  count(real_label)

pr2_pos <- pr2 %>% 
  filter(real_label == 0) %>%
  sample_n(size = 80000)


pr2_neg <- pr2 %>% 
  filter(real_label == 1) %>%
  sample_n(size = 80000)



write.xlsx(rbind(pr2_pos,pr2_neg),"last_train_data.xlsx")










