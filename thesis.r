# 1
library(readr)
library(dplyr)
library(MatchIt)
# r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility
sample_1 <- read_csv("data_1.csv")
m.out0 <- matchit(major_customer ~ bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, 
sample_1, method = NULL)
summary(m.out0)

m.out1 <- matchit(major_customer ~ bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, 
sample_1)
summary(m.out1)

m.out2 <- matchit(major_customer ~ bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, 
sample_1, "full")
summary(m.out2)

jpeg("haha.jpg")
plot(summary(m.out2))
dev.off()


m.data <- match.data(m.out2)
# m.data %>% write_csv("m_data.csv")

fit <- lm(r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, m.data)
summary(fit)

# 7
library(readr)
library(dplyr)
library(MatchIt)
# z_score ~ major_customer + bm + e_index + female_ceo + growth + intangible + leverage + pay_turn + roa + size + tangibility





# Model 1.1 - 1.6
# getwd()
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(broom)
sample = read_csv("sample_1.csv")
formulas = list(r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility, 
r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(industry), 
r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(fyear), 
r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + 
factor(industry) + factor(fyear), 
r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + factor(gvkey), 
r_d ~ major_customer + bm + e_index + female_ceo + growth + leverage + pay_turn + roa + size + tangibility + 
factor(gvkey) + factor(fyear))
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
