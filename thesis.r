
# 2022-11-18
# Only customer identifier (cid), customer name (cnms), customer type (ctype), and customer sales (salecs) are related.
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
d1 = read_csv("fchwcphj5t0zkfwv.csv") %>% select(gvkey, srcdate, ctype, salecs) %>% filter(ctype == "COMPANY") %>% select(gvkey, srcdate, salecs) %>% drop_na(salecs) %>% mutate(fyear = str_sub(srcdate, start = 1, end = 4)) %>% select(gvkey, fyear, salecs) %>% unite(col = gvkey_fyear, gvkey, fyear)
d2 = read_csv("8qjhjummhtlzjbdk.csv") %>% select(gvkey, fyear, sale) %>% drop_na(sale) %>% filter(sale != 0) %>% unite(gvkey_fyear, gvkey, fyear)
d3 = d1 %>% right_join(d2) %>% replace_na(list(salecs = 0)) %>% arrange(gvkey_fyear)
# major_customer
d3 %>% mutate(salecs_to_sale = salecs / sale) %>% select(gvkey_fyear, salecs_to_sale) %>% group_by(gvkey_fyear) %>% summarize(major_customer = if_else(any(salecs_to_sale >= 0.1), 1, 0)) %>% write_csv("major_customer.csv")
# customer_hhi
d3 %>% mutate(square_of_salecs_to_sale = (salecs / sale)^2) %>% select(gvkey_fyear, square_of_salecs_to_sale) %>% group_by(gvkey_fyear) %>% summarize(customer_hhi = sum(square_of_salecs_to_sale)) %>% write_csv("customer_hhi.csv")
# Note. summarize(.group = NULL) is the default, so ungroup() is unnecessary.
