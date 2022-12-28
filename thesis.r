# Model 1.1 - 1.6
# getwd()
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(broom)
sample = read_csv("sample_1.csv")
formulas = list(r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(industry), r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(fyear), r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(industry) + factor(fyear), r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(gvkey), r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(gvkey) + factor(fyear))
f_1 = function(x) {
    lm(x, sample) %>% tidy() %>% mutate(significance = symnum(p.value, cutpoints = c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ""))) %>% mutate_at(vars(estimate, statistic), ~ round(., digits = 3)) %>% mutate(result = paste0(estimate, significance, "\n(", statistic, ")")) %>% select(term, result)
}
map_dfr(formulas, f_1) %>% write_csv("f_1.csv")
nrow(sample)
f_2 = function(x) {
    lm(x, sample) %>% glance() %>% select(r.squared) %>% round(digits = 3)
}
map_dfr(formulas, f_2) %>% write_csv("f_2.csv")