# Model 1 (Model 1 in results.r)
# getwd()
library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(MatchIt)
sample <- read_csv("sample_1.csv")
m_out <- matchit(major_customer ~ bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + 
factor(industry) + factor(fyear), sample, "full")
m_data <- match.data(m_out)
fit <- lm(r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + 
factor(industry) + factor(fyear), m_data)
fit %>% tidy() %>% 
mutate(significance = symnum(p.value, cutpoints = c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ""))) %>% 
mutate_at(vars(estimate, statistic), ~ round(., digits = 3)) %>% 
mutate(result = paste0(estimate, significance, "\n(", statistic, ")")) %>% select(term, result) %>% write_csv("f_1.csv")
nrow(m_data)
fit %>% glance() %>% select(r.squared) %>% round(digits = 3) %>% write_csv("f_2.csv")

# Model 3.1 - 3.4
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(broom)
sample = read_csv("sample_3.csv")
formulas = list(leverage ~ major_customer + bm + e_index + female_ceo + growth + intangible + pay_turn + roa + size + tangibility, 
leverage ~ major_customer + bm + e_index + female_ceo + growth + intangible + pay_turn + roa + size + tangibility
 + factor(industry), 
leverage ~ major_customer + bm + e_index + female_ceo + growth + intangible + pay_turn + roa + size + tangibility
 + factor(fyear), 
leverage ~ major_customer + bm + e_index + female_ceo + growth + intangible + pay_turn + roa + size + tangibility
 + factor(industry) + factor(fyear))
f_1 = function(x) {
    lm(x, sample) %>% tidy() %>% 
    mutate(significance = symnum(p.value, cutpoints = c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ""))) %>% 
    mutate_at(vars(estimate, statistic), ~ round(., digits = 3)) %>% 
    mutate(result = paste0(estimate, significance, "\n(", statistic, ")")) %>% select(term, result)
}
map_dfr(formulas, f_1) %>% write_csv("f_1.csv")
nrow(sample)
f_2 = function(x) {
    lm(x, sample) %>% glance() %>% select(r.squared) %>% round(digits = 3)
}
map_dfr(formulas, f_2) %>% write_csv("f_2.csv")
