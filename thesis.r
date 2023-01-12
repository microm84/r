library(dplyr)
library(tibble)
library(tidyr)
library(readr)

data = tibble(t_PR = c(19, 14, 3, 1.5, 1.5, NA, 1.5, 1.5, NA, NA, NA, NA, NA, 1.5, NA, 1.5, 1.5, 1.5, 1.5, NA, 1.5, NA, NA, NA, NA, NA, NA, NA),
t_PD = c(Inf, Inf, Inf, 13, Inf, NA, 12, 7.5, 6, 6, 11, 7.5, 8, 9, 7, NA, 7, 7, NA, 4, 6, NA, 4.5, 4.5, 5.5, 3, 3, 3))

PFS = data %>% drop_na(t_PD) %>% select(t_PD) %>% rename(PFS = t_PD) %>% arrange(PFS)

DOR = data %>% drop_na() %>% mutate(DOR = t_PD - t_PR) %>% select(DOR) %>% arrange(DOR)

# only test this line!
time_to_reponse = data %>% drop_na(t_PR) %>% select(t_PR) %>% rename(time_to_reponse = t_PR) 

# test
data = tibble(patient = c(1, 2, 3, 4, 5, 7, 8, 14, 16, 17, 18, 19, 21), 
PR = c(19, 14, 3, 1.5, 1.4, 1.5, 1.6, 1.5, 1.4, 1.5, 1.4, 1.6, 1.3),
PD = c(NA, NA, NA, 12.5, NA, 11.5, 7.5, 9, 16.5, 7, 6.9, NA, 5.5)) %>% 
mutate(DOR = PD - PR)
data %>% write_csv("ha.csv")

median(data$DOR, na.rm = TRUE) 

data %>% select(patient, DOR) %>% drop_na() %>% arrange(DOR) %>% write_csv("haha2.csv")



data_2 = tibble(t_PR = c(1.5, 4.5, 3, 4.5, 1.5, 1.5, 1.5),
t_PD = c(14, 11.5, 7.5, 7, 5.5, 4, NA))
DOR = data_2 %>% drop_na() %>% mutate(DOR = t_PD - t_PR) %>% select(DOR) %>% arrange(DOR)
