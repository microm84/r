library(dplyr)
library(readr)
library(tidyr)
library(purrr)
d1 = read_csv("sample_1.csv") %>% 
select(gvkey, fyear, major_customer, industry) %>% 
group_by(industry, fyear) %>% filter(n() != 1) %>% 
mutate(industry_major_customer = (sum(major_customer) - major_customer) / (n() - 1)) %>% 
ungroup() %>%
select(gvkey, fyear, industry_major_customer)

d2 = map_dfr(2018:2021, function(x) {
    d1 %>% filter(fyear %in% (x - 3):x) %>% group_by(gvkey) %>% filter(n_distinct(fyear) == 4) %>% arrange(fyear, .by_group = TRUE) %>%
mutate(two_year_lagged_industry_major_customer = lag(industry_major_customer, 2), three_year_lagged_industry_major_customer = lag(industry_major_customer, 3)) %>% ungroup() %>% filter(fyear == x) %>% unite(gvkey_fyear, gvkey, fyear)
}) %>% arrange(gvkey_fyear)

d3 = read_csv("sample_1.csv") %>% unite(gvkey_fyear, gvkey, fyear) %>% inner_join(d2)

fit = lm(industry_major_customer ~ two_year_lagged_industry_major_customer + three_year_lagged_industry_major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, d3) 
d4 = d3 %>% mutate(predicted_industry_major_customer = fitted(fit)) 

fit2 = lm(r_d ~ predicted_industry_major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, d4)

# now test 
library(ivreg)
fit3 = ivreg(r_d ~ bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility | two_year_lagged_industry_major_customer + three_year_lagged_industry_major_customer | predicted_industry_major_customer, d4)












#

map_dfr(2017:2021, function(x) {
    d1 %>% filter(fyear %in% (x - 2):x) %>% group_by(gvkey) %>% filter(n_distinct(fyear) == 3) %>% summarize(fyear = x, earnings_volatility = sd(roa))
}) %>% unite(gvkey_fyear, gvkey, fyear) %>% arrange(gvkey_fyear) %>% write_csv("haha.csv")

map_dfr(2015:2021, function(x) {
    d1 %>% filter(fyear %in% (x - 1):x) %>% group_by(gvkey) %>% filter(n_distinct(fyear) == 2) %>% arrange(fyear, .by_group = TRUE) %>% mutate(lag_sale = lag(sale)) %>% ungroup() %>% drop_na(lag_sale) %>% filter(lag_sale != 0) %>% mutate(growth = sale / lag_sale - 1) %>% select(gvkey, fyear, growth) %>% unite(gvkey_fyear, gvkey, fyear)
}) %>% arrange(gvkey_fyear) %>% write_csv("growth.csv")

map_dfr(2015:2021, function(x) {
    d1 %>% filter(fyear %in% (x - 1):x) %>% group_by(gvkey) %>% filter(n_distinct(fyear) == 2) %>% arrange(fyear, .by_group = TRUE) %>% mutate(lag_ap = lag(ap), lag_invt = lag(invt)) %>% ungroup() %>% drop_na(lag_ap, lag_invt) %>% filter((ap + lag_ap) != 0) %>% mutate(pay_turn = (cogs + invt - lag_invt) / ((ap + lag_ap) /2)) %>% select(gvkey, fyear, pay_turn) %>% unite(gvkey_fyear, gvkey, fyear)
}) %>% arrange(gvkey_fyear) %>% write_csv("pay_turn.csv")







# test
sample_1 %>% group_by(industry, fyear) %>% filter(n() != 1) %>% 
mutate(industry_major_customer = (sum(major_customer) - major_customer) / (n() - 1)) %>%
mutate(sum = sum(major_customer), num = sum(major_customer) - major_customer, n = n(), den = n() - 1) %>%
arrange(industry, fyear) %>%
write_csv("t.csv")
