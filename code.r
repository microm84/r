# setwd('C:/Users/zzha642/Downloads')
# getwd()
# [1] "H:/Documents"
library(dplyr)
library(readr)
library(tidyr)
library(tibble)

# part b ii
# Table 1
read_csv('bii.csv') %>% select(Cowa, Hectares, Fertiliser, Wages, Opex, Feed, MS) %>% summarize_all(list(mean, median, sd, min, max)) %>% round(0) %>% as.matrix() %>% matrix(7,5) %>% as_tibble() %>% mutate(V6 = V5 - V4) %>% write_csv('2.csv')

# check
a = read_csv('bii.csv') %>% select(Cowa, Hectares, Fertiliser, Wages, Opex, Feed, MS) 
a %>% summarize_all(mean)
a %>% summarize_all(median)
a %>% summarize_all(sd)
a %>% summarize_all(min)
a %>% summarize_all(max)

# Table 2

# part b ii scale efficiency
a = read_csv('b ii scale efficiency.csv') %>% mutate(scale_efficiency_input = CRS_input_efficiency / VRS_input_efficiency, scale_efficiency_output = CRS_output_efficiency / VRS_output_efficiency) 
a %>% filter(scale_efficiency_input == 1) %>% select(Cows) %>% summarize_all(mean)
# 378
a %>% filter(scale_efficiency_output == 1) %>% select(Cows) %>% summarize_all(mean)
# 375

# Technical part c ii
read_csv('1.csv') %>% separate(DMU, c('farm', 'year'), '-') %>% filter(year %in% c('08', '09')) %>% filter(Region %in% c(6, 8)) %>% unite('DMU', c('farm', 'year'), sep = '_') %>% write_csv('2.csv')

# c iii
a = read_csv('c iii.csv') %>% mutate(scale_efficiency = CRS_input_efficiency / VRS_input_efficiency) %>% separate(DMU, c('farm', 'year'), '-') %>% group_by(year, Region) %>% summarize(VRS_input_efficiency_mean = mean(VRS_input_efficiency), scale_efficiency_mean = mean(scale_efficiency), number_of_efficient_DMUs = sum(VRS_input_efficiency == 1), VRS_input_efficiency_min = min(VRS_input_efficiency)) 
a %>% pull(VRS_input_efficiency_mean) %>% round(4) %>% matrix(8, 3) %>% as.data.frame() %>% write_csv('c iii 1.csv')
a %>% pull(scale_efficiency_mean) %>% round(4) %>% matrix(8, 3) %>% as.data.frame() %>% write_csv('c iii 2.csv')
a %>% pull(number_of_efficient_DMUs) %>% matrix(8, 3) %>% as.data.frame() %>% write_csv('c iii 3.csv')
a %>% pull(VRS_input_efficiency_min) %>% round(4) %>% matrix(8, 3) %>% as.data.frame() %>% write_csv('c iii 4.csv')

# c iii part 2
y = read_csv('ciii2.csv') %>% separate(DMU, c('farm', 'year'), '-') %>% group_by(year)
VRS_input = y %>% summarize(mean = mean(VRS_input_efficiency), sd = sd(VRS_input_efficiency), number_of_efficient_DMUs = sum(VRS_input_efficiency == 1), range = max(VRS_input_efficiency) - min(VRS_input_efficiency))
VRS_output = y %>% summarize(mean = mean(VRS_output_efficiency), sd = sd(VRS_output_efficiency), number_of_efficient_DMUs = sum(VRS_output_efficiency == 1), range = max(VRS_output_efficiency) - min(VRS_output_efficiency))
CRS_input = y %>% summarize(mean = mean(CRS_input_efficiency), sd = sd(CRS_input_efficiency), number_of_efficient_DMUs = sum(CRS_input_efficiency == 1), range = max(CRS_input_efficiency) - min(CRS_input_efficiency))
CRS_output = y %>% summarize(mean = mean(CRS_output_efficiency), sd = sd(CRS_output_efficiency), number_of_efficient_DMUs = sum(CRS_output_efficiency == 1), range = max(CRS_output_efficiency) - min(CRS_output_efficiency))
Scale_efficiency = y %>% summarize(mean = mean(Scale_efficiency), sd = sd(Scale_efficiency), number_of_efficient_DMUs = sum(Scale_efficiency == 1), range = max(Scale_efficiency) - min(Scale_efficiency))
VRS_input
VRS_output
CRS_input
CRS_output
Scale_efficiency

# print
VRS_input %>% write_csv('VRS_input.csv')
VRS_output %>% write_csv('VRS_output.csv')
CRS_input %>% write_csv('CRS_input.csv')
CRS_output %>% write_csv('CRS_output.csv') 
Scale_efficiency %>% write_csv('Scale_efficiency.csv')

# c iii part 3
r = read_csv('ciii3.csv') %>% group_by(Region)
VRS_input = r %>% summarize(mean = mean(VRS_input_efficiency), sd = sd(VRS_input_efficiency), number_of_efficient_DMUs = sum(VRS_input_efficiency == 1), range = max(VRS_input_efficiency) - min(VRS_input_efficiency))
VRS_output = r %>% summarize(mean = mean(VRS_output_efficiency), sd = sd(VRS_output_efficiency), number_of_efficient_DMUs = sum(VRS_output_efficiency == 1), range = max(VRS_output_efficiency) - min(VRS_output_efficiency))
CRS_input = r %>% summarize(mean = mean(CRS_input_efficiency), sd = sd(CRS_input_efficiency), number_of_efficient_DMUs = sum(CRS_input_efficiency == 1), range = max(CRS_input_efficiency) - min(CRS_input_efficiency))
CRS_output = r %>% summarize(mean = mean(CRS_output_efficiency), sd = sd(CRS_output_efficiency), number_of_efficient_DMUs = sum(CRS_output_efficiency == 1), range = max(CRS_output_efficiency) - min(CRS_output_efficiency))
Scale_efficiency = r %>% summarize(mean = mean(Scale_efficiency), sd = sd(Scale_efficiency), number_of_efficient_DMUs = sum(Scale_efficiency == 1), range = max(Scale_efficiency) - min(Scale_efficiency))
VRS_input
VRS_output
CRS_input
CRS_output
Scale_efficiency

# print
VRS_input %>% write_csv('VRS_input.csv')
VRS_output %>% write_csv('VRS_output.csv')
CRS_input %>% write_csv('CRS_input.csv')
CRS_output %>% write_csv('CRS_output.csv') 
Scale_efficiency %>% write_csv('Scale_efficiency.csv')

# c v
cv = read_csv('cv.csv')
cv$D2_1[cv$D2_1 > 3] = 3
cv$D1_2[cv$D1_2 > 3] = 3
cv1 = cv %>% mutate(efficiency_change = D2_2 / D1_1, technical_change = sqrt((D1_2 / D2_2) * (D1_1 / D2_1)), Malmquist_productivity_index = efficiency_change * technical_change)
efficiency_change = cv1 %>% summarize(mean = mean(efficiency_change), sd = sd(efficiency_change))
technical_change = cv1 %>% summarize(mean = mean(technical_change), sd = sd(technical_change))
Malmquist_productivity_index = cv1 %>% summarize(mean = mean(Malmquist_productivity_index), sd = sd(Malmquist_productivity_index))
efficiency_change
technical_change
Malmquist_productivity_index
