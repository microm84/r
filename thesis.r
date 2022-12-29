# Model 1.1 - 1.6
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
