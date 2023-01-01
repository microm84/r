setwd('C:/Users/zzha642/Downloads')
# getwd()
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
d1 = read_csv("lo7nx5blfgd3i9yc.csv") %>% select(gvkey, fyear, sale) %>% drop_na(sale)
map_dfr(2015:2021, function(x) {
    d1 %>% filter(fyear %in% (x - 1):x) %>% group_by(gvkey) %>% filter(n_distinct(fyear) == 2) %>% arrange(fyear, .by_group = TRUE) %>% mutate(lag_sale = lag(sale)) %>% ungroup() %>% drop_na(lag_sale) %>% filter(lag_sale != 0) %>% mutate(growth = sale / lag_sale - 1) %>% select(gvkey, fyear, growth) %>% unite(gvkey_fyear, gvkey, fyear)
}) %>% arrange(gvkey_fyear) %>% write_csv("growth.csv")

