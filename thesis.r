# 2022-12-16
# 2022-12-17
# Model 1 - 4
setwd('C:/Users/zzha642/Downloads')
# getwd()
library(dplyr)
library(readr)
library(tidyr)
# 1
# r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility
read_csv("nonnegative.csv") %>% inner_join(read_csv("r_d.csv")) %>% inner_join(read_csv("major_customer.csv")) %>% inner_join(read_csv("bm.csv")) %>% inner_join(read_csv("e_index.csv")) %>% inner_join(read_csv("female_ceo.csv")) %>% inner_join(read_csv("growth.csv")) %>% inner_join(read_csv("leverage.csv")) %>% inner_join(read_csv("pay_turn.csv")) %>% inner_join(read_csv("roa.csv")) %>% inner_join(read_csv("size.csv")) %>% inner_join(read_csv("tangibility.csv")) %>% separate(gvkey_fyear, c("gvkey", "fyear"), sep = "_") %>% write_csv("data_1.csv")


