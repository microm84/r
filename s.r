library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
filter(gapminder, year %in% c(1962, 2012)) %>% 
ggplot(aes(fertility, life_expectancy, col = continent)) + 
geom_point() + 
facet_grid(continent ~ year)

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
filter(gapminder, year %in% c(1962, 2012)) %>% 
ggplot(aes(fertility, life_expectancy, col = continent)) + 
geom_point() + 
facet_grid(year ~ .)

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder %>%
filter(year %in% c(1962, 1970, 1980, 1990, 2000, 2012), continent %in% c('Asia', 'Europe')) %>% 
ggplot(aes(fertility, life_expectancy, col = continent)) + 
geom_point() + 
facet_wrap(~year)

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder %>%
filter(country == 'United States') %>%
ggplot(aes(year, fertility)) +
geom_line()

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
countries = c('Germany', 'South Korea')
gapminder %>%
filter(country %in% countries) %>%
ggplot(aes(year, fertility, color = country)) +
geom_line()

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
countries = c('Germany', 'South Korea')
labels = data.frame(x = c(1965, 1975), y = c(60,72), country = countries)
#I think should be 1965, 1975
gapminder %>%
filter(country %in% countries) %>%
ggplot(aes(year, life_expectancy, color = country)) +
geom_line() +
geom_text(data = labels, aes(x, y, label = country))

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
countries = c('South Korea', 'Germany')
labels = data.frame(country = countries, x = c(1965, 1975), y = c(60,72))
#I think should be 1965, 1975
gapminder %>%
filter(country %in% countries) %>%
ggplot(aes(year, life_expectancy, color = country)) +
geom_line() +
geom_text(data = labels, aes(x, y, label = country), size = 5) +
theme(legend.position = 'none')

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
labels = data.frame(country = c('Germany', 'South Korea'), x = c(1965, 1975), y = c(60,72))
gapminder %>%
filter(country %in% countries) %>%
ggplot(aes(year, life_expectancy, color = country)) +
geom_line() +
geom_text(data = labels, aes(x, y, label = country)) +
theme(legend.position = 'none')


library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
labels = data.frame(country = c('Germany', 'South Korea'), x = c(1975, 1965), y = c(72, 60))
gapminder %>%
filter(country %in% countries) %>%
ggplot(aes(year, life_expectancy, color = country)) +
geom_line() +
geom_text(data = labels, aes(x, y, label = country)) +
theme(legend.position = 'none')

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
gapminder %>%
filter(year == 2010 & !is.na(gdp)) %>%
ggplot(aes(dollars_per_day)) +
geom_histogram(binwidth = 1, color = 'black')

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
gapminder %>%
filter(year == 1970 & !is.na(gdp)) %>%
ggplot(aes(log2(dollars_per_day))) +
geom_histogram(binwidth = 1, color = 'black')

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
gapminder %>%
filter(year == 2010 & !is.na(gdp)) %>%
ggplot(aes(dollars_per_day)) +
geom_histogram(binwidth = 1, color = 'black') +
scale_x_continuous(trans = 'log2')

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
gapminder %>%
filter(year == c(1960, 1970, 1980, 1990, 2000, 2010) & !is.na(gdp)) %>%
ggplot(aes(dollars_per_day)) +
geom_histogram(binwidth = 1, color = 'black') +
scale_x_continuous(trans = 'log2') +
facet_wrap(. ~ year)

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
gapminder %>%
filter(year == c(2010) & !is.na(gdp)) %>%
ggplot(aes(region, dollars_per_day)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
gapminder %>%
filter(year == c(2010) & !is.na(gdp)) %>%
mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
ggplot(aes(region, dollars_per_day, fill = continent)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 90)) +
scale_y_continuous(trans = 'log2') + geom_point()

# 1:12
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
filter(year == 1970 & !is.na(gdp)) %>%
mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>%
ggplot(aes(dollars_per_day)) +
geom_histogram(binwidth = 1) +
scale_x_continuous(trans = 'log2') +
facet_grid(. ~ group)

# 1:49
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
filter(year %in% c(1970, 2010) & !is.na(gdp)) %>%
mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>%
ggplot(aes(dollars_per_day)) +
geom_histogram(binwidth = 1) +
scale_x_continuous(trans = 'log2') +
facet_grid(year ~ group)

# 3:51
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
gapminder %>%
filter(year %in% c(1970, 2010) & country %in% country_1970_2010) %>%
mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>%
ggplot(aes(dollars_per_day)) +
geom_histogram(binwidth = 1) +
scale_x_continuous(trans = 'log2') +
facet_grid(year ~ group)

# 4:31
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
gapminder %>%
filter(year %in% c(1970, 2010) & country %in% country_1970_2010) %>%
mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
ggplot(aes(region, dollars_per_day, color = continent)) +
theme(axis.text.x = element_text(angle = 90)) +
scale_y_continuous(trans = 'log2') +
geom_boxplot() +
facet_grid(year ~ .)

# 5:46
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
gapminder %>%
filter(year %in% c(1970, 2010) & country %in% country_1970_2010) %>%
mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
ggplot(aes(region, dollars_per_day, fill = factor(year))) +
theme(axis.text.x = element_text(angle = 90)) +
scale_y_continuous(trans = 'log2') +
geom_boxplot()

# 1
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
filter(year == 2010 & country %in% country_1970_2010) %>%
mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>%
group_by(group) %>%
summarize(n())

# 2
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
filter(year %in% c(1970, 2010) & country %in% country_1970_2010) %>%
mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>%
ggplot(aes(dollars_per_day, ..count.., color = group)) +
scale_x_continuous(trans = 'log2') +
geom_density(alpha = 0.2, bw = 0.75) +
facet_grid(year ~ .)

#3
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
filter(year %in% c(1970, 2010) & country %in% country_1970_2010) %>%
mutate(group = case_when(
    .$region %in% west ~ 'West',
    .$region %in% c('Eastern Asia', 'South-Eastern Asia') ~ 'East Asia',
    .$region %in% c('Caribbean', 'Cantral America', 'South America') ~ 'Latin America',
    .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa',
    TRUE ~ 'Others')
) %>%
mutate(group = factor(group)) %>%
ggplot(aes(dollars_per_day, ..count.., color = group)) +
scale_x_continuous(trans = 'log2') +
geom_density(alpha = 0.2, bw = 0.75) +
facet_grid(year ~ .)

#4
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
filter(year %in% c(1970, 2010) & country %in% country_1970_2010) %>%
mutate(group = case_when(
    .$region %in% west ~ 'West',
    .$region %in% c('Eastern Asia', 'South-Eastern Asia') ~ 'East Asia',
    .$region %in% c('Caribbean', 'Cantral America', 'South America') ~ 'Latin America',
    .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa',
    TRUE ~ 'Others')
) %>%
mutate(group = factor(group, levels = c('Others', 'Latin America', 'East Asia', 'Sub-Saharan Africa', 'West'))) %>%
ggplot(aes(dollars_per_day, ..count.., color = group)) +
scale_x_continuous(trans = 'log2') +
geom_density(alpha = 0.2, bw = 0.75, position = 'stack') +
facet_grid(year ~ .)

# 5
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder = gapminder %>% mutate(dollars_per_day = gdp / population / 365)
country_1970 = gapminder %>% filter(year == 1970 & !is.na(dollars_per_day)) %>% .$country
country_2010 = gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>% .$country
country_1970_2010 = intersect(country_1970, country_2010)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
filter(year %in% c(1970, 2010) & country %in% country_1970_2010) %>%
mutate(group = case_when(
    .$region %in% west ~ 'West',
    .$region %in% c('Eastern Asia', 'South-Eastern Asia') ~ 'East Asia',
    .$region %in% c('Caribbean', 'Cantral America', 'South America') ~ 'Latin America',
    .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa',
    TRUE ~ 'Others')
) %>%
mutate(group = factor(group, levels = c('Others', 'Latin America', 'East Asia', 'Sub-Saharan Africa', 'West'))) %>%
ggplot(aes(dollars_per_day, color = group, weight = population)) +
scale_x_continuous(trans = 'log2') +
geom_density(alpha = 0.2, bw = 0.75) +
facet_grid(year ~ .)

# 1
logit = function(p) {
    log(p / (1 - p))
}
logit(0.9)
logit(0.99)
logit(0.999)

# 2
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
a = gapminder %>%
mutate(group = case_when(
    .$region %in% west ~ 'West',
    .$region == 'Northern Africa' ~ 'Northern Africa',
    .$region %in% c('Eastern Asia', 'South-Eastern Asia') ~ 'East Asia',
    .$region == 'Southern Asia' ~ 'Southern Asia',
    .$region %in% c('Caribbean', 'Cantral America', 'South America') ~ 'Latin America',
    .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa',
    .$region %in% c('Melanesia', 'Micronesia', 'Polynesia') ~ 'Pacific Islands')
)
surv_income = a %>%
filter(year == 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
group_by(group) %>%
summarize(income = sum(gdp) / sum(population) / 365, infant_survival_rate = 1 - sum(infant_mortality / 1000 * population) / sum(population))
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
scale_x_continuous(trans = 'log2', limit = c(0.25, 150)) +
scale_y_continuous(trans = 'logit', limit = c(0.875, 0.9981), breaks = c(0.85, 0.90, 0.95, 0.99, 0.995, 0.998)) +
geom_label()

#3
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
west = c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
a = gapminder %>%
mutate(group = case_when(
    .$region %in% west ~ 'West',
    .$region == 'Northern Africa' ~ 'Northern Africa',
    .$region %in% c('Eastern Asia', 'South-Eastern Asia') ~ 'East Asia',
    .$region == 'Southern Asia' ~ 'Southern Asia',
    .$region %in% c('Caribbean', 'Cantral America', 'South America') ~ 'Latin America',
    .$continent == 'Africa' & .$region != 'Northern Africa' ~ 'Sub-Saharan Africa',
    .$region %in% c('Melanesia', 'Micronesia', 'Polynesia') ~ 'Pacific Islands')
)
surv_income = a %>%
filter(year == 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
summarize(country = country, income = gdp / population / 365, infant_survival_rate = 1 - infant_mortality /1000, group = group)
surv_income %>% ggplot(aes(income, infant_survival_rate, label = country, color = group)) +
scale_x_continuous(trans = 'log2') +
scale_y_continuous(trans = 'logit') +
geom_label()

#6
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)
gapminder %>% filter(year %in% seq(1960, 2010,1), country == 'Cambodia') %>% ggplot(aes(year, life_expectancy)) + geom_line()

#14
library(dslabs)
library(dplyr)
library(ggplot2)
data(gapminder)

a = gapminder %>%
filter(year %in% c(1970, 2010) & continent == 'Africa' & !is.na(gdp) & !is.na(infant_mortality)) %>%
mutate(dollars_per_day = gdp / population / 365)
a %>% ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
geom_point() +
scale_x_continuous(trans = 'log2') +
geom_text() +
facet_grid(year ~ .)

#1 home runs vs wins
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
ggplot(aes(HR_per_game, R_per_game)) +
geom_point(alpha = 0.5)

#2 stolen bases vs wins
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
a = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(SB_per_game = SB / G, R_per_game = R / G)
a %>%
ggplot(aes(SB_per_game, R_per_game)) +
geom_point(alpha = 0.5)
cor(a$SB_per_game, a$R_per_game)

#3 bases on balls vs runs
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
ggplot(aes(BB_per_game, R_per_game)) +
geom_point(alpha = 0.5)

# HR -> R, HR -> BB -> R

#4 at bats vs runs
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
ggplot(aes(AB_per_game, R_per_game)) +
geom_point(alpha = 0.5)

#5 wins per game vs errors per game
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
a = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(E_per_game = E / G, W_per_game = W / G)

a %>% ggplot(aes(E_per_game, W_per_game)) +
geom_point(alpha = 0.5)
cor(a$E_per_game, a$W_per_game)

#6 
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
a = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(X2B_per_game = X2B / G, X3B_per_game = X3B / G)
a %>% ggplot(aes(X2B_per_game, X3B_per_game)) +
geom_point(alpha = 0.5)
cor(a$X2B_per_game, a$X3B_per_game)

#1
library(HistData)
#library(dslabs)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
#set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(childNum  == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>%
summarize(mean(father), sd(father), mean(son), sd(son), cor(father, son))
galton_heights %>%
ggplot(aes(father, son)) +
geom_point(alpha = 0.5)

#1 using slice_smaple instead
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)

#2 Monte Carlo
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
R = replicate(1000, {
    slice_sample(galton_heights, n = 179, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
a = data.frame(r = R)
mean(a$r)
sd(a$r)
a %>% ggplot(aes(r)) + geom_histogram(binwidth = 0.05, color = 'black')

#3 qq plot to test samplle R against normal distribution
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
R = replicate(1000, {
    slice_sample(galton_heights, n = 25, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
data.frame(R) %>%
ggplot(aes(sample = R)) +
geom_qq() +
geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(25-2)))

#4/#7 at bats vs runs
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
a = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(AB_per_game = AB / G, R_per_game = R / G)
cor(a$AB_per_game, a$R_per_game)
cor(a$R_per_game, a$AB_per_game)
#ggplot(aes(AB_per_game, R_per_game)) +
#geom_point(alpha = 0.5)

#5#8 wins per game vs errors per game
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
a = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(E_per_game = E / G, W_per_game = W / G)

a %>% ggplot(aes(E_per_game, W_per_game)) +
geom_point(alpha = 0.5)
cor(a$E_per_game, a$W_per_game)

#6#9
library(Lahman)
library(dslabs)
library(dplyr)
library(ggplot2)
data(Teams)
a = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(X2B_per_game = X2B / G, X3B_per_game = X3B / G)
a %>% ggplot(aes(X2B_per_game, X3B_per_game)) +
geom_point(alpha = 0.5)
cor(a$X2B_per_game, a$X3B_per_game)

#0
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
mf = mean(galton_heights$father)
sf = sd(galton_heights$father)
#test whether the result is 1.14
(72 - mf) / sf


#1 conditional average, stratifying father's height.
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
ms = mean(galton_heights$son)
ss = sd(galton_heights$son)
#conditional_mean = galton_heights %>%
#filter(round(father) == 72) %>%
#summarize(mean(son))
conditional_son = galton_heights %>% filter(round(father) == 72)
(mean(conditional_son$son) - ms) / ss

#1.2. conditional mean of son, stratifying father's height.
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
conditional_mean = galton_heights %>%
filter(round(father) == 72) %>%
summarize(mean(son)) %>%
.$mean

#2
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>%
mutate(father_strata = factor(round(father))) %>%
ggplot(aes(father_strata, son)) +
geom_boxplot() +
geom_point()

# 3:15
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>%
mutate(father = round(father)) %>%
group_by(father) %>%
summarize(conditional_mean_of_son = mean(son)) %>%
ggplot(aes(father, conditional_mean_of_son)) +
geom_point()

#test
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
x = galton_heights$father
y = galton_heights$son
r = cor(x, y)
galton_heights %>%
ggplot(aes(scale(x), scale(y))) +
geom_point() +
geom_abline(slope = r)
mean(scale(x) * scale(y))

# 3:44
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
r = galton_heights %>% summarize(r = cor(father, son)) %>% .$r
r2 = galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
galton_heights %>%
mutate(father = round(father)) %>%
group_by(father) %>%
summarize(son = mean(son)) %>%
ggplot(aes(scale(father), scale(son))) +
geom_point() +
geom_abline(slope = r)

#2:17
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>%
mutate(z_father = round((father - mean(father)) / sd(father))) %>%
filter(z_father %in% -3:3) %>%
ggplot() +
geom_qq(aes(sample = son)) +
facet_wrap(. ~ z_father)

#0:50
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
galton_heights = GaltonFamilies %>%
filter(childNum == 1 & gender == 'male') %>%
select(father, childHeight) %>%
rename(son = childHeight)
x = galton_heights$father
y = galton_heights$son
mean(scale(x) * scale(y))
r = cor(x, y)
b1 = r * sd(y) / sd(x)
a1 = mean(y) - b1 * mean(x)
b2 = r * sd(x) / sd(y)
a2 = mean(x) - b2 * mean(y)
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
c(b1, a1, b2, a2)
c(m_1, b_1, m_2, b_2)
galton_heights %>%
ggplot(aes(father, son)) +
geom_point() +
geom_abline(intercept = a1, slope = b1, color = 'blue') +
geom_abline(intercept = a2, slope = b2, color = 'red')

#8
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
library(dplyr)
library(ggplot2)
female_heights <- GaltonFamilies%>%
filter(gender == "female") %>%
group_by(family) %>%
sample_n(1) %>%
ungroup() %>%
select(mother, childHeight) %>%
rename(daughter = childHeight)

x = female_heights$mother
y = female_heights$daughter
c(mean(x), sd(x), mean(y), sd(y), cor(x, y))
b = cor(x, y) * sd(y) / sd(x)
a = mean(y) - b * mean(x)
cor(x, y) ^ 2
a + b * 60

#1
library(dplyr)
library(Lahman)
data(Teams)
get_slope = function(x, y) {
    cor(x, y) *sd(y) / sd(x)
}
slope = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(BB_per_game = BB / G, Singles_per_game = (H - HR - X2B - X3B) / G, R_per_game = R / G) %>%
summarize(BB_R_slope = get_slope(BB_per_game, R_per_game), Singles_R_slope = get_slope(Singles_per_game, R_per_game))
slope
#before stratifying HR / G, slope of x = BB / G & y = R / G was 0.735.
data(Teams)
cors = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(BB_per_game = BB / G, HR_per_game = HR / G, Singles_per_game = (H - HR - X2B - X3B) / G) %>%
summarize(cor(BB_per_game, HR_per_game), cor(Singles_per_game, HR_per_game), cor(BB_per_game, Singles_per_game))
cors

#0:26 stratifying HR / G, slope of x = BB / G, y = R / G
library(dplyr)
library(ggplot2)
library(Lahman)
data(Teams)
da = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(HR_strata = round(HR / G, digits = 1), BB_per_game = BB / G, R_per_game = R / G) %>%
filter(HR_strata >= 0.4 & HR_strata <= 1.2)
da %>%
group_by(HR_strata) %>%
ggplot(aes(BB_per_game, R_per_game)) +
geom_point() +
geom_smooth(method = 'lm') +
facet_wrap(. ~ HR_strata)

#0:56 after stratifying HR / G, slope of x = BB / G & y = R / G dropped down from 0.735 to around 0.400.
library(dplyr)
library(ggplot2)
library(Lahman)
data(Teams)
da = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(HR_strata = round(HR / G, digits = 1), BB_per_game = BB / G, R_per_game = R / G) %>%
filter(HR_strata >= 0.4 & HR_strata <= 1.2)
da %>%
group_by(HR_strata) %>%
summarize(slope = cor(BB_per_game, R_per_game) * sd(R_per_game) / sd(BB_per_game))

#1:38
library(dplyr)
library(ggplot2)
library(Lahman)
data(Teams)
da = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(BB_strata = round(BB / G, digits = 1), HR_per_game = HR / G, R_per_game = R / G) %>%
filter(BB_strata >= 2.8 & BB_strata <= 3.9)
da %>%
#group_by(BB_strata) %>%
ggplot(aes(HR_per_game, R_per_game)) +
geom_point() +
geom_smooth(method = 'lm') +
facet_wrap(. ~ BB_strata)

#1:47 stratifying BB / G, slope of x = HR / G & y = R / G
library(dplyr)
library(ggplot2)
library(Lahman)
data(Teams)
da = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(BB_strata = round(BB / G, digits = 1), HR_per_game = HR / G, R_per_game = R / G) %>%
filter(BB_strata >= 2.8 & BB_strata <= 3.9)
da %>%
group_by(BB_strata) %>%
summarize(slope = cor(HR_per_game, R_per_game) * sd(R_per_game) / sd(HR_per_game), a = mean(R_per_game) - slope * mean(HR_per_game))

#1:48 slope of x = HR / G & y = R / G does not change before or after stratifying BB / G.
library(dplyr)
library(Lahman)
data(Teams)
get_slope = function(x, y) {
    cor(x, y) *sd(y) / sd(x)
}
slope = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
summarize(HR_R_slope = get_slope(HR_per_game, R_per_game))
slope

#1
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
rss = function(b0, b1) {
    res = galton_heights$son - (b0 + b1 * galton_heights$father)
    sum(res^2)
}
b1 = seq(0, 1, len = nrow(galton_heights))
results = data.frame(b1 = b1, rss = sapply(b1, rss, b0 = 25))
results %>% ggplot(aes(b1, rss)) +
geom_line()

fit = lm(son ~ father, data = galton_heights)
fit

# test sapply
fun = function(a1, a2) {a1 + a2}
a1 = 1:10
re = sapply(a1, fun, a2 = 1)
re

#2
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
lse_coef = replicate(1000, {
    slice_sample(galton_heights, n = 50, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})
lse = data.frame(b0 = lse_coef[1, ], b1 = lse_coef[2, ])
p1 = lse %>%
ggplot(aes(b0)) + geom_histogram(binwidth = 5)
p2 = lse %>%
ggplot(aes(b1)) + geom_histogram(binwidth = 0.1)
grid.arrange(p1, p2, ncol = 2)

#2
slice_sample(galton_heights, n = 50, replace = TRUE) %>%
lm(son ~ father, data = .) %>%
summary %>%
.$coef

#3
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
lse_coef = replicate(1000, {
    slice_sample(galton_heights, n = 50, replace = TRUE) %>%
#    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})
cor(lse_coef[1, ], lse_coef[2, ])
mean(scale(lse_coef[1, ]) * scale(lse_coef[2, ]))

#1:03
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>%
ggplot(aes(son, father)) +
geom_point() +
geom_smooth(method = 'lm')

#1:21
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>%
mutate(y_hat = predict(lm(son ~ father, data = .))) %>%
ggplot(aes(father, y_hat)) +
geom_line()

#1:37
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
y_hat = predict(lm(son ~ father, data = galton_heights), se.fit = TRUE)
y_hat

#2
library(dplyr)
library(Lahman)
data(Teams)
da = Teams %>%
filter(yearID %in% 1961:2001) %>%
mutate(R_per_game = R / G, BB_per_game = BB / G, HR_per_game = HR / G)
lm(R_per_game ~ BB_per_game + HR_per_game, data = da)

#5.1
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>% ggplot(aes(father, son)) +
geom_point() +
geom_smooth()

#5.2
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)
galton_heights %>% ggplot(aes(father, son)) +
geom_point() +
geom_smooth(method = "lm")

#5.3
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
    geom_line(color = "blue", size = 1) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
    geom_point(data = galton_heights, aes(x = father, y = son))

#5.4
library(HistData)
library(dplyr)
library(ggplot2)
data(GaltonFamilies)
set.seed(1983)
galton_heights = GaltonFamilies %>%
filter(gender == 'male') %>%
group_by(family) %>%
slice_sample(n = 1) %>%
ungroup() %>%
select(father, childHeight) %>%
rename(son = childHeight)

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
    geom_line(color = "blue", size = 1) + 
    geom_point(data = galton_heights, aes(x = father, y = son))

#7 - 8
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits = 3)
female_heights <- GaltonFamilies %>%
    filter(gender == "female") %>%
    group_by(family) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(mother, childHeight) %>%
    rename(daughter = childHeight)

lm(mother ~ daughter, data = female_heights)

#9
library(dplyr)
library(Lahman)
data(Batting)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)

#test
library(dplyr)
library(Lahman)
data(Batting)
b = Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    mutate(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
    select(playerID, yearID, singles, bb, mean_singles, mean_bb)

b1 = b %>% filter(mean_singles > 0.2)
b2 = b %>% filter(mean_bb > 0.2)

#test again!
library(dplyr)
da = data.frame(playerID = c('Bob', 'Sam', 'Bob', 'Sam', 'Bob', 'Sam'), yearID = c(1999, 1999, 2000, 2000, 2001, 2001), grade = c(80, 70, 30, 50, 70, 90))
d2 = da %>% filter(yearID %in% 1999:2001) %>%
group_by(playerID) %>%
# Groups:   playerID [2]
mutate(mean_grade = mean(grade)) %>%
# Groups:   playerID [2], calculated the mean_grade
select(playerID, mean_grade)

d3 = data.frame(playerID = c('Bob', 'Tom'))
j = inner_join(d2, d3)

#10
library(dplyr)
library(Lahman)
data(Batting)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)
b = Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    mutate(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
    select(mean_singles, mean_bb)
j = inner_join(bat_02, b)
cor(j$singles, j$mean_singles)
cor(j$bb, j$mean_bb)

#10 upgraded
library(dplyr)
library(Lahman)
data(Batting)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    summarize(mean_singles = mean(singles), mean_bb = mean(bb))
j = inner_join(bat_02, bat_99_01)
cor(j$singles, j$mean_singles)
cor(j$bb, j$mean_bb)

#11
library(dplyr)
library(Lahman)
library(ggplot2)
data(Batting)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    summarize(mean_singles = mean(singles), mean_bb = mean(bb))
j = inner_join(bat_02, bat_99_01)
p1 = j %>% ggplot(aes(singles, mean_singles)) +
geom_point()
p2 = j %>% ggplot(aes(bb, mean_bb)) +
geom_point()

#12
library(dplyr)
library(Lahman)
library(ggplot2)
data(Batting)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    summarize(mean_singles = mean(singles), mean_bb = mean(bb))
j = inner_join(bat_02, bat_99_01)
lm(singles ~ mean_singles, data = j)
lm(bb ~ mean_bb, data = j)

#1
library(dplyr)
library(Lahman)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR = round(HR/G, 1), BB = BB/G, R = R/G) %>%
select(HR, BB, R) %>%
filter(HR >= 0.4 & HR<=1.2)

dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

#2
library(dplyr)
library(Lahman)
data(Teams)
dat = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR = round(HR/G, 1), BB = BB/G, R = R/G) %>%
select(HR, BB, R) %>%
filter(HR >= 0.4 & HR<=1.2) %>%
group_by(HR)
get_coef = function(data) {
    lm = lm(R ~ BB, data = data)
    data.frame(coef = lm$coef[2])
}
dat %>% do(get_coef(.))

#2:42
library(dplyr)
library(Lahman)
data(Teams)
dat = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR = round(HR/G, 1), BB = BB/G, R = R/G) %>%
select(HR, BB, R) %>%
filter(HR >= 0.4 & HR<=1.2) %>%
group_by(HR)
get_coef = function(data) {
    lm = lm(R ~ BB, data = data)
    data.frame(term = names(lm$coef), est = lm$coef)
}
dat %>% do(get_coef(.))

#1
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
dat = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR = round(HR/G, 1), BB = BB/G, R = R/G) %>%
select(HR, BB, R) %>%
filter(HR >= 0.4 & HR<=1.2) %>%
group_by(HR) %>%
do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
filter(term == 'BB') %>%
mutate(BB_estimate = estimate) %>%
select(HR, BB_estimate, conf.low, conf.high) %>%
ggplot(aes(HR, BB_estimate, ymin = conf.low, ymax = conf.high)) +
geom_point() +
geom_errorbar()

#2
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
dat = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR = round(HR/G, 1), BB = BB/G, R = R/G) %>%
select(HR, BB, R) %>%
filter(HR >= 0.4 & HR<=1.2)
lm = lm(R ~ BB, data = dat)
glance(lm)

#7.1
library(dplyr)
library(Lahman)
library(broom)
data(Teams)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR = HR/G, R = R/G) %>%
select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

#7.2
library(dplyr)
library(Lahman)
library(broom)
data(Teams)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(HR = HR/G, R = R/G) %>%
select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

#8
#library(tidyverse)
library(tidyr)
library(dplyr)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")
galton <- GaltonFamilies %>%
    group_by(family, gender) %>%
    sample_n(1) %>%
    ungroup() %>% 
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child"))
g = galton %>%
group_by(pair) %>%
summarize(n())
sum(g$pair == 'father_daughter')
sum(g$pair == 'mother_son')
sum(g$pair == 'father_son')
sum(g$pair == 'mother_daughter')

#9
library(tidyr)
library(dplyr)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")
galton <- GaltonFamilies %>%
    group_by(family, gender) %>%
    sample_n(1) %>%
    ungroup() %>% 
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child"))

g = galton %>%
select(childHeight, pair, parentHeight) %>%
group_by(pair) %>%
summarize(cor = cor(childHeight, parentHeight))

#10
library(tidyr)
library(dplyr)
library(HistData)
library(broom)
library(ggplot2)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")
galton <- GaltonFamilies %>%
    group_by(family, gender) %>%
    sample_n(1) %>%
    ungroup() %>% 
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child"))

g = galton %>%
select(childHeight, pair, parentHeight) %>%
group_by(pair) %>%
do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>%
filter(term == 'parentHeight') %>%
ggplot(aes(pair, estimate, ymin = conf.low, ymax = conf.high)) +
geom_errorbar() +
geom_point()

g2 = g = galton %>%
select(childHeight, pair, parentHeight) %>%
group_by(pair) %>%
do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>%
filter(term == 'parentHeight') %>%
summarize(conf.high - conf.low)

#1
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
dat = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB = BB/G, HR = HR/G, R = R/G)
lm = lm(R ~ BB + HR, data = dat)
tidy(lm, conf.int = T)

#2:06
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
dat = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB = BB/G,
singles = (H-X2B-X3B-HR)/G,
doubles = X2B/G,
triples = X3B/G,
HR = HR/G,
R = R/G) %>%
lm(R ~ BB + singles + doubles + triples + HR, data = .)

tidy(dat, conf.int = T)

#2:35 - 3:06
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
fit = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB = BB/G,
singles = (H-X2B-X3B-HR)/G,
doubles = X2B/G,
triples = X3B/G,
HR = HR/G,
R = R/G) %>%
lm(R ~ BB + singles + doubles + triples + HR, data = .)

Teams %>% filter(yearID == 2002) %>%
mutate(BB = BB/G,
singles = (H-X2B-X3B-HR)/G,
doubles = X2B/G,
triples = X3B/G,
HR = HR/G,
R = R/G) %>%
mutate(R_hat = predict(fit, newdata = .)) %>%
ggplot(aes(R_hat, R, label = teamID)) +
geom_point() +
geom_text(nudge_x = 0.1) +
geom_abline ()

#4:33
# test1 we see the AB+BB, sum(AB+BB), and max(G) for teamID == 'ANA'
library(dplyr)
library(Lahman)
data(Batting)

a = Batting %>%
filter(yearID == 2002) %>%
filter(teamID == 'ANA') %>%
select(AB, BB, G) %>%
mutate (AB_BB = AB+BB, sum_AB_BB = sum(AB_BB), max_G = max(G))

#4:33
library(dplyr)
library(Lahman)
data(Batting)
pa_per_game = Batting %>%
filter(yearID == 2002) %>%
group_by(teamID) %>%
summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
.$pa_per_game %>%
mean

#5:02
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
fit = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB = BB/G,
singles = (H-X2B-X3B-HR)/G,
doubles = X2B/G,
triples = X3B/G,
HR = HR/G,
R = R/G) %>%
lm(R ~ BB + singles + doubles + triples + HR, data = .)

data(Batting)
pa_per_game = Batting %>%
filter(yearID == 2002) %>%
group_by(teamID) %>%
summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
.$pa_per_game %>%
mean

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
group_by(playerID) %>%
mutate(PA = BB + AB) %>%
summarize(G = sum(PA)/pa_per_game,
BB = sum(BB)/G,
singles = sum(H-X2B-X3B-HR)/G,
doubles = sum(X2B)/G,
triples = sum(X3B)/G,
HR = sum(HR)/G,
AVG = sum(H)/sum(AB),
PA = sum(PA)) %>%
filter(PA >= 300) %>%
select(-G) %>% #remove the G column
mutate(R_hat = predict(fit, newdata = .)) %>%
ggplot(aes(R_hat)) +
geom_histogram(binwidth = 0.5)

#6:08
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
fit = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB = BB/G,
singles = (H-X2B-X3B-HR)/G,
doubles = X2B/G,
triples = X3B/G,
HR = HR/G,
R = R/G) %>%
lm(R ~ BB + singles + doubles + triples + HR, data = .)

data(Batting)
pa_per_game = Batting %>%
filter(yearID == 2002) %>%
group_by(teamID) %>%
summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
.$pa_per_game %>%
mean

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
group_by(playerID) %>%
mutate(PA = BB + AB) %>%
summarize(G = sum(PA)/pa_per_game,
BB = sum(BB)/G,
singles = sum(H-X2B-X3B-HR)/G,
doubles = sum(X2B)/G,
triples = sum(X3B)/G,
HR = sum(HR)/G,
AVG = sum(H)/sum(AB),
PA = sum(PA)) %>%
filter(PA >= 300) %>%
select(-G) %>% #remove the G column
mutate(R_hat = predict(fit, newdata = .))

data(Salaries)
players = Salaries %>%
filter(yearID == 2002) %>%
select(playerID, salary) %>%
# 846 rows * 2
right_join(players, by = 'playerID')
#should be 465 rows * 10 columns

#7:58
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
fit = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB = BB/G,
singles = (H-X2B-X3B-HR)/G,
doubles = X2B/G,
triples = X3B/G,
HR = HR/G,
R = R/G) %>%
lm(R ~ BB + singles + doubles + triples + HR, data = .)

data(Batting)
pa_per_game = Batting %>%
filter(yearID == 2002) %>%
group_by(teamID) %>%
summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
.$pa_per_game %>%
mean

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
group_by(playerID) %>%
mutate(PA = BB + AB) %>%
summarize(G = sum(PA)/pa_per_game,
BB = sum(BB)/G,
singles = sum(H-X2B-X3B-HR)/G,
doubles = sum(X2B)/G,
triples = sum(X3B)/G,
HR = sum(HR)/G,
AVG = sum(H)/sum(AB),
PA = sum(PA)) %>%
filter(PA >= 300) %>%
select(-G) %>% #remove the G column
mutate(R_hat = predict(fit, newdata = .))
#465 * 9

data(Salaries)
players = Salaries %>%
filter(yearID == 2002) %>%
select(playerID, salary) %>%
#846 * 2
right_join(players, by = 'playerID')
#465 * 10

data(Fielding)
players = Fielding %>%
filter(yearID == 2002 & !POS %in% c('OF', 'P')) %>%
#720 * 18
group_by(playerID) %>%
#720 * 18, 424 playerID
slice_max(order_by = G, n = 1, with_ties = F) %>%
#424 * 18, playerID [424]
ungroup() %>%
select(playerID, POS) %>%
right_join(players, by = 'playerID') %>%
#465 * 11
filter(!is.na(POS) & !is.na(salary))
#242 *11

data(Master)
players = Master %>%
select(playerID, nameFirst, nameLast, debut) %>%
right_join(players, by = 'playerID')
#242 * 14

players %>%
filter(debut < 1998) %>%
ggplot(aes(salary, R_hat, color = POS)) +
geom_point() +
scale_x_log10()

players %>%
filter(debut < 1998) %>%
select(nameFirst, nameLast, POS, salary, R_hat) %>%
slice_max(order_by = R_hat, n = 10)

#1:34
library(Lahman)
library(dplyr)
data(Fielding)
data(Master)
playerInfo = Fielding %>%
group_by(playerID) %>%
slice_max(order_by = G, n = 1, with_ties = F) %>% #19,698 × 18, playerID [19,698]
ungroup() %>% #19,698 * 18
left_join(Master, by = 'playerID') %>% #Master 20,093 * 26, after left_join 19,698 rows * 43
select(playerID, nameFirst, nameLast, POS) #19,698 * 4

data(AwardsPlayers)
data(Batting) #108,789 * 22
ROY = AwardsPlayers %>%
filter(awardID == 'Rookie of the Year') %>% #142 * 6
left_join(playerInfo, by = 'playerID')  %>% #142 * 9.
rename(rookie_year = yearID) %>%
right_join(Batting, by = 'playerID') %>% #108,789 * 30
mutate(AVG = H/AB) %>% #108,789 * 31
filter(POS != 'P') #1,448 × 31

library(tidyr)
ROY = ROY %>%
filter(yearID == rookie_year | yearID == rookie_year + 1) %>% #210 × 31
group_by(playerID) %>% #playerID 104
mutate(rookie = ifelse(yearID == min(yearID), 'rookie', 'sophomore')) %>%
filter(n() == 2) %>% #204 × 32, playerID 102
ungroup() %>%
select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG) %>% #204*6 
pivot_wider(names_from = rookie, values_from = AVG) %>% arrange(desc(rookie))

mean(ROY$sophomore - ROY$rookie <= 0)

#3:29
library(Lahman)
library(dplyr)
library(tidyr)
playerInfo = Fielding %>%
group_by(playerID) %>%
slice_max(order_by = G, n = 1, with_ties = F) %>% #19,698 × 18, playerID [19,698]
ungroup() %>% #19,698 * 18
left_join(Master, by = 'playerID') %>% #Master 20,093 * 26, after left_join 19,698 rows * 43
select(playerID, nameFirst, nameLast, POS) #19,698 * 4

two_years <- Batting %>%
    filter(yearID %in% 2013:2014) %>%
    group_by(playerID, yearID) %>%
    filter(sum(AB) >= 130) %>% #Groups: playerID, yearID [801], (playerID [489], using unique)
    summarize(AVG = sum(H)/sum(AB), .groups = 'keep') %>% #801*3, Groups: playerID, yearID [801]
    ungroup %>%
    pivot_wider(names_from = yearID, values_from = AVG) %>% #489*3
    filter(!is.na(`2013`) & !is.na(`2014`)) %>% #312*3
    left_join(playerInfo, by="playerID") %>% #312*6
    filter(POS!="P") %>% #312*6, actually a useless code, because no POS == P
    arrange(desc(`2013`)) %>%
    select(nameFirst, nameLast, `2013`, `2014`)
#two_years

#arrange(two_years, `2013`)

library(ggplot2)
two_years %>%
ggplot(aes(`2013`,`2014`)) +
geom_point()

cor(two_years$`2013`, two_years$`2014`)

#1
library(dslabs)
library(dplyr)
library(ggplot2)
library(broom)
#data(falling_object)

f = rfalling_object()
f %>% 
ggplot(aes(time, observed_distance)) + 
geom_point()

fit = f %>% 
mutate(t_sq = time^2) %>% 
lm(observed_distance ~ time + t_sq, data = .)
tidy(fit)

augment(fit) %>% 
ggplot() + 
geom_point(aes(time, observed_distance)) + 
geom_line(aes(time, .fitted))

tidy(fit, conf.int = T)

#3
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
dat = Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(BB = BB/G,
singles = (H-X2B-X3B-HR)/G,
doubles = X2B/G,
triples = X3B/G,
HR = HR/G,
R = R/G) %>%
lm(R ~ BB + singles + doubles + triples + HR, data = .)

#tidy(dat, conf.int = T)

a=tidy(dat)
b=data.frame(f=a$estimate,A=c(1,2,4,1,0,1),B=c(1,1,6,2,1,0))%>%
mutate(a2=f*A,b2=f*B)%>% 
summarize(sum(a2),sum(b2))

#9
library(dplyr)
library(Lahman)
library(broom)
library(ggplot2)
data(Teams)
d = Teams %>% filter(yearID %in% 1961:2018) %>% group_by(yearID) %>% 
do(tidy(lm(R ~ BB + HR, data = .), conf.int = T)[2,]) 

d %>% ggplot(aes(yearID,estimate)) + geom_point() + geom_smooth(method = 'lm')
tidy(lm(estimate ~ yearID, data = d))

#1
library(dplyr)
N = 25
g = 100000
d = data.frame(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N, g))

res = d %>% 
group_by(group) %>% 
summarize(r = cor(x, y)) %>% 
arrange(desc(r))
res

library(ggplot2)
d %>% filter(group == res$group[which.max(res$r)]) %>% 
ggplot(aes(x, y)) +
geom_point() + 
geom_smooth(method = 'lm')

res %>% ggplot(aes(r)) + 
geom_histogram(binwidth = 0.1)

library(broom)
d %>% filter(group == res$group[which.max(res$r)]) %>% 
do(tidy(lm(y ~ x, data = .)))

#2
library(ggplot2)
set.seed(1)
x = rnorm(100, 100, 1)
y = rnorm(100, 84, 1)
#a = scale(x[-23])
#b = scale(y[-23])
x[-23] = scale(x[-23])
y[-23] = scale(y[-23])
data.frame(x,y) %>% ggplot(aes(x, y)) + geom_point()
c(cor(x, y), cor(x[-23], y[-23]), cor(rank(x), rank(y)))
cor(x, y, method = 'spearman')

#3
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))

#4
library(dslabs)
data(admissions)
admissions

a=admissions
sum(a$admitted) / sum(a$applicants)

library(dplyr)
a=admissions
a %>% group_by(gender) %>% 
summarize(ar = sum(admitted / 100 * applicants) / sum(applicants)) #difference is 14%

a=admissions
a %>% group_by(gender) %>% 
  summarize(total_admitted = sum(admitted / 100 * applicants), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

library(tidyr)
a=admissions
a %>% select(major, gender, admitted) %>%
pivot_wider(names_from = gender, values_from = admitted) %>%
  mutate(women_minus_men = women - men)

library(ggplot2)
a=admissions
a %>% group_by(major) %>% 
summarize(m = sum(admitted / 100 * applicants) / sum(applicants), 
g = sum(applicants * (gender == 'women')) / sum(applicants)) %>% 
ggplot(aes(m, g, label = major)) + geom_text()

library(ggplot2)
a=admissions
a %>% mutate(a = admitted / 100 * applicants / sum(applicants)) %>% 
ggplot(aes(gender, a, fill = major)) + geom_bar(stat = 'identity')

library(ggplot2)
a=admissions
a %>% ggplot(aes(major, admitted, color = gender, size = applicants)) + geom_point()

#1
library(dslabs)
library(ggplot2)
library(dplyr)
data(heights)
heights %>% ggplot(aes(sex, height)) + geom_point() + geom_jitter(width = 0.5, alpha = 0.1)

#2
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
    ggplot(aes(x, y, color = col)) +
    geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)

#test
library(ggplot2)
library(dplyr)
a = data.frame(x = 1:8, y = 1:8)
a %>% ggplot(aes(x, y)) + geom_point(color = '#999999')

#1
library(dplyr)
library(ggplot2)
library(dslabs)
dat <- us_contagious_diseases %>%
filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)

dat$rate == rate

#test
library(dplyr)
library(ggplot2)
library(dslabs)
d = us_contagious_diseases %>%
filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% 
mutate(rate = count / population * 10000 * 52 / weeks_reporting)
#head(d)
state <- d$state

#4
library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders %>% mutate(rate = total/population*100000) %>% mutate(region = reorder(region, rate, median)) %>% 
ggplot(aes(region, rate)) +
geom_boxplot() + geom_point()

#1
library(tidyr)
library(dslabs)
data(gapminder)
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
d <- gapminder %>%
    filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

library(ggplot2)
d %>% ggplot(aes(year, life_expectancy, label = country)) + geom_line(aes(color = country)) + geom_label()

library(tidyr)
d %>% 
select(country, year, life_expectancy) %>% 
pivot_wider(names_from = year, values_from = life_expectancy) %>% 
mutate(mean = (`2010` + `2015`) / 2, dif = `2015` - `2010`) %>% 
ggplot(aes(mean, dif, label = country)) + geom_point() + geom_label(nudge_y = 0.02)

d %>%
    mutate(location = ifelse(year == 2010, 1, 2),
           location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                             location + 0.22, location),
           hjust = ifelse(year == 2010, 1, 0)) %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(year, life_expectancy, group = country)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
    xlab("") +
    ylab("Life Expectancy") 

library(ggrepel)
d %>%
    mutate(year = paste0("life_expectancy_", year)) %>%
    select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
    mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
                difference = life_expectancy_2015 - life_expectancy_2010) %>%
    ggplot(aes(average, difference, label = country)) +
    geom_point() +
    geom_text_repel() +
    geom_abline(lty = 2) +
    xlab("Average of 2010 and 2015") +
    ylab("Difference between 2015 and 2010")

#
library(dslabs)
library(dplyr)
data(us_contagious_diseases)

d = us_contagious_diseases %>% 
filter(!state %in% c("Hawaii", "Alaska") & disease == 'Measles') %>%
mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
mutate(state = reorder(state, rate))

library(ggplot2)
d %>% filter(state == 'California' & !is.na(rate)) %>% 
ggplot(aes(year, rate)) + geom_line() + geom_vline(xintercept = 1963, color = 'red')

#4:03 - 4:25
library(RColorBrewer)
display.brewer.all(type='seq')

display.brewer.all(type='div')

#4:53
library(dslabs)
library(dplyr)
data(us_contagious_diseases)

d = us_contagious_diseases %>% 
filter(!state %in% c("Hawaii", "Alaska") & disease == 'Measles') %>%
mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
mutate(state = reorder(state, rate))

d %>% ggplot(aes(year, state, fill=rate)) +
    geom_tile(color = 'grey50') +
    scale_x_continuous(expand = c(0,0)) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
    geom_vline(xintercept = 1963, col = "blue") +
    theme_minimal() + theme(panel.grid = element_blank()) +
    ggtitle('Measles') +
    ylab("") +
    xlab("")

#test
library(dslabs)
library(dplyr)
data(us_contagious_diseases)

d = us_contagious_diseases %>% 
filter(!state %in% c("Hawaii", "Alaska") & disease == 'Measles') %>%
mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
mutate(state = reorder(state, rate))

library(ggplot2)
d %>%
ggplot(aes(year, state, fill = rate)) +
geom_tile() + 
scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, 'Greens')) + 
geom_vline(xintercept = 1963, color = 'red')

#5:25
library(dslabs)
library(dplyr)
data(us_contagious_diseases)

d = us_contagious_diseases %>% 
filter(!state %in% c("Hawaii", "Alaska") & disease == 'Measles') %>%
mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
    filter(disease == 'Measles') %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
d %>%
    filter(!is.na(rate)) %>%
    ggplot() +
    geom_line(aes(year, rate, group = state), color = "grey50", 
        show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
    ggtitle("Cases per 10,000 by state") +
    xlab("") +
    ylab("") +
    geom_text(data = data.frame(x = 1955, y = 50),
        mapping = aes(x, y, label = "US average"), color = "black") +
    geom_vline(xintercept = 1963, col = "blue")

#test
library(dslabs)
library(dplyr)
data(us_contagious_diseases)

d = us_contagious_diseases %>% 
filter(!state %in% c("Hawaii", "Alaska") & disease == 'Measles') %>%
mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
    filter(disease == 'Measles') %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

d %>% 
ggplot() + 
geom_line(aes(year, rate, group = state), alpha = 0.1) + 
geom_line(aes(year, us_rate), data =avg) + 
geom_vline(xintercept = 1963)

#1
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
   filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>% 
   mutate(rate = count / population * 10000) %>% 
   mutate(state = reorder(state, rate))

#2
options(digits = 3)
library(dplyr)
library(titanic)
data(titanic_train)
#titanic
t <- titanic_train %>%
    select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
    mutate(Survived = factor(Survived),
           Pclass = factor(Pclass),
           Sex = factor(Sex))

#2.1-2.2
t %>%
ggplot(aes(Age, fill = Sex)) + 
geom_density(alpha = 0.2)
#2.3-2.4
t %>%
ggplot(aes(Age, ..count.., fill = Sex)) + 
geom_density(alpha = 0.2)
#2.5-2.6
t %>%
ggplot(aes(Age, group = Sex)) + 
geom_density() + 
facet_grid(Sex ~ .)

t$Sex[which.max(t$Age)]
#The oldest was male.

sum(t$Sex == 'female')

t %>% filter(Age == 40.00) %>% group_by(Sex) %>% summarize(n = n())

t %>% filter(Age >= 18.00 & Age <= 35.00) %>% group_by(Sex) %>% summarize(n = n())
t %>% filter(Age <= 17.00) %>% group_by(Sex) %>% summarize(n = n())

#3
params <- t %>%
    filter(!is.na(Age)) %>%
    summarize(mean = mean(Age), sd = sd(Age))
t %>% 
filter(!is.na(Age)) %>% 
ggplot(aes(sample = Age)) + 
geom_qq(dparams = params) + 
geom_abline()

#4
t %>% 
ggplot(aes(Survived, fill = Sex)) + 
geom_bar()

t %>% 
ggplot(aes(Survived, fill = Sex)) + 
geom_bar(position = position_dodge())

t %>% 
ggplot(aes(Sex, fill = Survived)) + 
geom_bar()

#5
t %>%
ggplot(aes(Age, ..count.., fill = Survived)) + 
geom_density(alpha = 0.2)

t %>% filter(Survived == 0) %>% 
ggplot(aes(Age)) + 
geom_histogram()

t %>% filter(Survived == 0) %>% filter(Age >= 18 & Age <= 30)
#191
t %>% filter(Survived == 0) %>% filter(Age >= 30 & Age <= 50)
#154

#6
t %>% filter(Fare != 0) %>% 
ggplot(aes(Survived, Fare)) + geom_boxplot() + geom_jitter() + scale_y_continuous(trans = 'log2')

#7.1
t %>% 
ggplot(aes(Pclass, fill = Survived)) + geom_bar()
#7.2
t %>% 
ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = position_fill())
#7.3
t %>% 
ggplot(aes(Survived, fill = Pclass)) + geom_bar(position = position_fill())

#8.1
t %>% ggplot(aes(Age, ..count..)) + geom_density(alpha = 0.2) + facet_grid(Sex ~ Pclass)
#8.2
t %>% ggplot(aes(Age)) + geom_density() + facet_grid( ~ Pclass)
#8.3
t %>% ggplot(aes(Sex)) + geom_bar() + facet_grid( ~ Pclass)
#8.4
t %>% ggplot(aes(Survived)) + geom_bar() + facet_grid(Sex ~ Pclass)
#8.5
t %>% ggplot(aes(Age, ..count.., fill = Survived)) + geom_density(position = 'stack') + facet_grid(Sex ~ Pclass)
library(dslabs)
library(ggplot2)
ds_theme_set()
take_poll(25)

library(dslabs)
take_poll(25)

N = 25
p = seq(0, 1, length.out = 100)
se = sqrt(p * (1 - p) / N)
plot(p, se, ylim = c(0, 0.1))

p = seq(0, 1, length.out = 100)
s = c(25, 100, 1000)
for(N in s){
    se = sqrt(p * (1 - p) / N)
    plot(p, se, ylim = c(0, 0.1))
}

take_sample = function(p, N) {
    s = sample(c(1, 0), N, replace = T, prob = c(p, 1 - p))
    mean(s)
}
take_sample(0.45, 100)

B = 10000
errors = replicate(B, take_sample(0.45, 100))
mean(errors)

N = 2500
se = sqrt(0.5^2/N)

library(dslabs)
data(polls_us_election_2016)
library(dplyr)
polls = polls_us_election_2016 %>%
filter(enddate >= as.Date('2016-10-31') & state == 'U.S.')
N = polls[1, 6]
X_hat = polls[1, 8] / 100
se_hat = sqrt(X_hat * (1- X_hat) / N)
ci = c(X_hat + qnorm(0.025) * se_hat, X_hat + qnorm(0.975) * se_hat)
ci

library(dslabs)
data(polls_us_election_2016)
library(dplyr)
polls = polls_us_election_2016 %>% 
filter(enddate >= as.Date('2016-10-31') & state == 'U.S.')
pollster_results = polls %>% 
mutate(X_hat = polls[ , 8] / 100, 
se_hat = sqrt(X_hat * (1- X_hat) / polls[ , 6]), 
lower = X_hat + qnorm(0.025) * se_hat,
upper = X_hat + qnorm(0.975) * se_hat) %>% 
select(pollster, enddate, X_hat, se_hat, lower, upper)
avg_hit = pollster_results %>% 
mutate(hit = lower <= 0.482 & 0.482 <= upper) %>% 
summarize(mean(hit))

library(dslabs)
data(polls_us_election_2016)
library(dplyr)
polls = polls_us_election_2016 %>% 
filter(enddate >= '2016-10-31' & state == 'U.S.') %>% 
mutate(d_hat = rawpoll_clinton - rawpoll_trump)
N = polls[1, 6]
N
d_hat = polls[1, 16]/100
d_hat
X_hat = (d_hat + 1) / 2
se_hat = 2 * sqrt(X_hat * (1- X_hat) / N)
se_hat
ci = c(d_hat + qnorm(0.025) * se_hat, d_hat + qnorm(0.975) * se_hat)

library(dslabs)
data(polls_us_election_2016)
library(dplyr)
polls = polls_us_election_2016 %>% 
filter(enddate >= '2016-10-31' & state == 'U.S.')
pollster_results = polls %>% 
mutate(d_hat = (rawpoll_clinton - rawpoll_trump) / 100, 
X_hat = (d_hat + 1) / 2, 
se_hat = 2 * sqrt(X_hat * (1 - X_hat) / samplesize), 
lower = d_hat + qnorm(0.025) * se_hat, 
upper = d_hat + qnorm(0.975) * se_hat) %>% 
select(pollster, enddate, d_hat, lower, upper)
avg_hit = pollster_results %>% 
mutate(hit = lower <= 0.021 & 0.021 <= upper) %>% 
summarize(mean(hit))
library(ggplot2)
pollster_results %>% 
mutate(errors = d_hat - 0.021) %>% 
ggplot(aes(pollster, errors)) + 
geom_point() + 
theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(dslabs)
data(polls_us_election_2016)
library(dplyr)
polls = polls_us_election_2016 %>% 
filter(enddate >= '2016-10-31' & state == 'U.S.')
pollster_results = polls %>% 
mutate(d_hat = (rawpoll_clinton - rawpoll_trump) / 100, 
X_hat = (d_hat + 1) / 2, 
se_hat = 2 * sqrt(X_hat * (1 - X_hat) / samplesize), 
lower = d_hat + qnorm(0.025) * se_hat, 
upper = d_hat + qnorm(0.975) * se_hat) %>% 
select(pollster, enddate, d_hat, lower, upper)
pollster_results %>% 
mutate(errors = d_hat - 0.021) %>% 
group_by(pollster) %>% 
filter(n() >= 5) %>% 
ggplot(aes(pollster, errors)) + 
geom_point()

x = c(271, 193, 191, 135, 121, 119, 118, 115, 115, 100, 
94, 88, 86, 83, 73, 70, 69, 67, 66, 65, 
60, 59, 55, 55, 53, 53, 51, 50, 48, 45, 
44, 37, 37, 37, 35, 33, 33, 33, 33, 32,
32, 32, 31, 30, 30, 30, 30, 29, 29, 29, 
29, 28, 28, 28, 27, 26, 26, 26, 26, 26, 
26, 25, 25, 25, 24, 24, 24, 24, 24, 24, 
24, 23, 23, 22, 22, 22, 22, 22, 22, 21, 
21, 21, 21, 21, 21, 20, 20, 20, 20, 20, 
19, 19, 19, 19, 19, 19, 18, 18, 18, 18)
hist(x)

x = c(271, 193, 191, 135, 121, 119, 118, 115, 115, 100)
hist(x)

a = c(204, 146, 126, 124, 115, 111, 91, 88, 88, 85, 
84, 80, 77, 75, 75, 71, 67, 65, 64, 63, 
62, 61, 60, 57, 57, 55, 54, 54, 53, 50, 
50, 50, 49, 49, 49, 48, 48, 47, 47, 47, 
45, 45, 45, 44, 44, 43, 43, 43, 43, 42, 
42, 42, 41, 40, 40, 40, 39, 39, 38, 38, 
37, 37, 37, 37, 37, 37, 36, 36, 36, 36, 
35, 35, 35, 35, 35, 35, 35, 34, 34, 34, 
34, 34, 33, 33, 33, 33, 33, 33, 32, 32, 
32, 32, 32, 31, 31, 31, 31, 31, 31, 31)
hist(a)

b = c(10, 7, 6, 5, 5, 4, 4, 4, 4, 4, 
4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
hist(b)

# Bayes
p = 0.00025
N = 100000
o = sample(c("d", "h"), N, replace = TRUE, prob = c(p, 1 - p))
nd = sum(o == "d")
nh = sum(o == 'h')

t = vector('character', N)

a = 0.99
t[o == 'd'] = sample(c('+', '-'), nd, replace = TRUE, prob = c(a, 1 - a))
t[o == 'h'] = sample(c('-', '+'), nh, replace = TRUE, prob = c(a, 1 - a))

sum(t == '+')

#test
p = 0.2
N = 100
o = sample(c('d', 'h'), N, replace = TRUE, prob = c(p, 1 - p))
nd = sum(o == "d")
nh = sum(o == 'h')
t = 1:100

a = c(2847, 2797, 2187, 2068, 2048, 1671, 1656, 1544, 1518, 1516, 
1450, 1402, 1347, 1342, 1332, 1309, 1290, 1263, 1242, 1238, 
1241, 1159, 1153, 1148, 1146, 1131, 1128, 1123, 1108, 1104, 
1081, 1074, 1074, 1073, 1066, 1066, 1056, 1050, 1045, 1034, 
1029, 1028, 1027, 1025, 1023, 1017, 1006, 1005)
hist(a)

a = c(2,2,2,2,2,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1)
hist(a)

#
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
results = polls %>% summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))

mu <- 0
tau <- 0.01
sigma = results$se
Y = results$avg
B = sigma^2 / (sigma^2 + tau^2)
d = B * mu + (1 - B) * Y
se = sqrt(sigma^2*tau^2/(sigma^2+tau^2))
ci = c(d + qnorm(0.025) * se, d + qnorm(0.975) * se)

#
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
results = polls %>% summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))

mu <- 0
sigma <- results$se
Y <- results$avg
taus <- seq(0.005, 0.05, len = 100)
p_calc = function(tau) {
    B = sigma^2/(sigma^2 + tau^2)
    se = sqrt(1/(1/sigma^2 + 1/tau^2))
    p = pnorm(0, mu, se)
}
ps = sapply(taus, p_calc)
plot(taus, ps)

#test

taus <- 1:10
p_calc = function(tau) {
    tau^2
}
ps = sapply(taus, p_calc)
plot(taus, ps)

#test2
taus <- seq(0.005, 0.05, len = 100)
p_calc = function(tau) {
    B = sigma^2/(sigma^2 + tau^2)
    e = B * mu + (1 - B) * Y
    se = sqrt(1/(1/sigma^2 + 1/tau^2))
    p = pnorm(0, e, se)
}
ps = sapply(taus, p_calc)
plot(taus, ps)

#c4
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>%
  .$height
mean(x)
sd(x)

N <- 50
X = sample(x, size = N, replace = TRUE)
mean(X)
sd(X)
se = sd(X) / sqrt(N)
ci = mean(X) + se * qnorm(c(0.025, 0.975))

#test
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
N <- 50
B <- 10000
res = replicate(B, expr = {
    X = sample(x, N, replace = TRUE)
    interval = mean(X) + sd(X) / sqrt(N) * qnorm(c(0.025, 0.975))
    between(mu, interval[1], interval[2])
})
mean(res)

#
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
polls %>% group_by(pollster) %>% ggplot(aes(pollster, spread)) + geom_boxplot() + geom_point()

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma = polls %>% 
group_by(pollster) %>% 
summarize(s = sd(spread))
# Print the contents of sigma to the console
sigma

#
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
res = polls %>% 
group_by(pollster) %>% 
summarize(d = mean(spread), sd = sd(spread), N = length(spread))
estimate = res$d[2]-res$d[1]
estimate
se_hat = sqrt(res$sd[2]^2/res$N[2] + res$sd[1]^2/res$N[1])
se_hat
ci = estimate + se_hat * qnorm(c(0.025, 0.975))
# two tailed t test
(1 - pnorm(estimate / se_hat)) * 2
pnorm(- estimate / se_hat) * 2

#
library(dslabs)
library(dplyr)
data("polls_us_election_2016")
p = polls_us_election_2016 %>%
  filter(state =="Wisconsin" &
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
p = p %>%
  mutate(state = as.character(state))
p = p %>% 
  left_join(results_us_election_2016, by = "state") %>% 
  mutate(actual = clinton/100 - trump/100) %>%
  summarize(actual = first(actual), avg = mean(spread), 
            sd = sd(spread), n = n()) %>%
  select(actual, avg, sd, n)

#
pt(-2, df = 3) + 1 - pt(2, df = 3)

#
df = seq(3, 50)
pt_func = function(df) {
    pt(-2, df = df) + 1 - pt(2, df = df)
}
probs = sapply(df, pt_func)
plot(df, probs)

#
library(dslabs)
library(dplyr)
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
N <- 15
B <- 10000
res = replicate(B, expr = {
  s = sample(x, size = N, replace = TRUE)
  interval = mean(s) + qnorm(c(0.05, 0.95)) * sd(s) / sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)

#old
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
N <- 50
B <- 10000
res = replicate(B, expr = {
    X = sample(x, N, replace = TRUE)
    interval = mean(X) + sd(X) / sqrt(N) * qnorm(c(0.025, 0.975))
    between(mu, interval[1], interval[2])
})
mean(res)

#test again
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
set.seed(1)
N <- 50
B <- 10000
res = replicate(B, expr = {
    X = sample(x, N, replace = TRUE)
    interval = mean(X) + sd(X) / sqrt(N) * qnorm(c(0.025, 0.975))
    between(mu, interval[1], interval[2])
})
mean(res)

#test again when N = 15
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
set.seed(1)
N <- 15
B <- 10000
res = replicate(B, expr = {
    X = sample(x, N, replace = TRUE)
    interval = mean(X) + sd(X) / sqrt(N) * qnorm(c(0.025, 0.975))
    between(mu, interval[1], interval[2])
})
mean(res)

# official 
library(dslabs)
library(dplyr)
data(heights)
x <- heights %>% filter(sex == "Male") %>%
  .$height
mu <- mean(x)
N <- 15
B <- 10000
set.seed(1)
res = replicate(B, expr = {
    X = sample(x, N, replace = TRUE)
    interval = mean(X) + sd(X) / sqrt(N) * c(qnorm(0.975),qnorm(0.025))
    between(mu, interval[2], interval[1])
})
mean(res)
#0.9331
#
library(dslabs)
library(dplyr)
data(heights)
x <- heights %>% filter(sex == "Male") %>%
  .$height
mu <- mean(x)
N <- 15
B <- 10000
set.seed(1)
res = replicate(B, expr = {
    X = sample(x, N, replace = TRUE)
    interval = mean(X) + sd(X) / sqrt(N) * c(qnorm(0.025),qnorm(0.975))
    between(mu, interval[1], interval[2])
})
mean(res)

#
library(tidyverse)
library(dslabs)
library(ggplot2)
data("research_funding_rates")
#research_funding_rates %>% select(discipline, applications_total, success_rates_total) %>% 
#ggplot(aes(discipline, success_rates_total)) + geom_point()

t <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum)

#
a = matrix(c(3,1,1,3),2,2)
fisher.test(a,alternative='greater')

a=c(TRUE,FALSE,TRUE,FALSE)
b=c(FALSE,FALSE,TRUE)
a||b 
a|b 
a&&b 
a&b

pt(-1.847, df = 24) + 1 - pt(1.847, df = 24)
#0.07704
pnorm(-1.847) + 1 - pnorm(1.847)

pt(-3.604, df = 24) + 1 - pt(3.604, df = 24)
#0.00142
pt(-1.880, df = 24) + 1 - pt(1.880, df = 24)

pt(-0.181, df = 24) + 1 - pt(0.181, df = 24)

pt(-1.413, df = 24) + 1 - pt(1.413, df = 24)

pt(-0.883, df = 24) + 1 - pt(0.883, df = 24)

#
tt = data.frame(men = c(1345, 290), women = c(1011, 177))
chisq.test(tt)
#test
a = matrix(c(1345, 290, 1011, 177), 2, 2)
chisq.test(a)
b = matrix(c(1345, 290, 1011, 177) * 10, 2, 2)
chisq.test(b)

#
p = system.file(package = 'dslabs')
list.files(path = p)
wd = getwd()
#
d = system.file('extdata', package = 'dslabs')
p = file.path(d, 'murders.csv')
file.copy(p, 'ha')

library(readr)
d = read_csv('ha')
d2 = read_csv(p)

library(readxl)

library(readr)
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
d = read_csv(url)

download.file(url, 'a')

library(readr)
r = read_csv('times.txt')

s = read_csv('times.txt', col_name = TRUE)

t = read_delim('times.txt', delim = ',')

#
library(readxl)
r = read_xlsx('t.xlsx')

r = read_xlsx('t.xlsx', sheet = 2)

r = read_xlsx('t.xlsx', sheet = '2016')

#
r = read.csv('t.csv')

#
library(readr)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
a = read_csv(url, col_names = FALSE)

#
library(dslabs)
library(readr)
library(dplyr)
p = system.file('extdata', package = 'dslabs')
f = file.path(p, 'fertility-two-countries-example.csv')
od = read_csv(f)
library(tidyr)
nd = od %>% 
pivot_longer(`1960`:`2015`, names_to = 'year', values_to = 'fertility') %>% 
mutate(year = as.integer(year))
library(ggplot2)
nd %>% ggplot(aes(year, fertility, color = country)) + 
  geom_point()

#
library(dslabs)
library(readr)
library(dplyr)
p = system.file('extdata', package = 'dslabs')
f = file.path(p, 'fertility-two-countries-example.csv')
od = read_csv(f)
library(tidyr)
nd = od %>% 
pivot_longer(`1960`:`2015`, names_to = 'year', values_to = 'fertility') %>% 
mutate(year = as.integer(year))
library(ggplot2)
nd %>% ggplot(aes(year, fertility, color = country)) + 
  geom_point()

#
library(dslabs)
p <- system.file("extdata", package = "dslabs")
f <-  file.path(p, "life-expectancy-and-fertility-two-countries-example.csv")
library(readr)
rd <- read_csv(f)
library(dplyr)
#select(rd, 1:5)
d = rd %>% 
pivot_longer(-country) %>% 
separate(name, c('year', 'name'), '_')
#test
d = rd %>% 
pivot_longer(-country) %>% 
separate(name, c('year', 'name1', 'name2'), '_')
#test2
d = rd %>% 
pivot_longer(-country) %>% 
separate(name, c('year', 'name1', 'name2'), '_', fill = 'right')
#test3
library(dslabs)
p <- system.file("extdata", package = "dslabs")
f <-  file.path(p, "life-expectancy-and-fertility-two-countries-example.csv")
library(readr)
rd <- read_csv(f)
library(dplyr)
library(tidyr)
d = rd %>% 
pivot_longer(-country) %>% 
separate(name, c('year', 'name'), '_', extra = 'merge')
d %>% pivot_wider()

#
library(dslabs)
p <- system.file("extdata", package = "dslabs")
f <-  file.path(p, "life-expectancy-and-fertility-two-countries-example.csv")
library(readr)
rd <- read_csv(f)
library(dplyr)
library(tidyr)
d = rd %>% 
pivot_longer(-country) %>% 
separate(name, c('year', 'name1', 'name2'), '_', fill = 'right') %>% 
unite(name, name1, name2) %>% 
pivot_wider() %>% 
rename(fertility = fertility_NA)

#
library(dplyr)
m = matrix(co2, ncol = 12, byrow = TRUE)
c = data.frame(m) %>% 
setNames(1:12) %>% 
mutate(year = as.character(1959:1997))
#test c2 only contains 1959
c2 = c %>% 
filter(year == '1959')

library(tidyr)
c3 = gather(c, month, co2, -year)
library(ggplot2)
c3 %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# 12
library(dslabs)
data(admissions)
library(dplyr)
d <- admissions %>% select(-applicants)
#spread(d, gender, admitted)

library(tidyr)
t = gather(admissions, key, value, admitted:applicants)
t2 = gather(t, column_name, c(key, gender))

t = gather(admissions, key, value, admitted:applicants)
t2 = unite(t, column_name, c(key, gender))

spread(t2, column_name, value)

#17.8.1
library(dplyr)
library(dslabs)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96*se, end = avg + 1.96*se) 

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
posterior_mean
posterior_se
posterior_mean + c(-1.96, 1.96)*posterior_se
1 - pnorm(0, posterior_mean, posterior_se)

#17.8.3 1
set.seed(3)
I = 5
J <- 6
N <- 2000
d <- .021
p <- (d + 1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N))
})

#17.8.3 2
set.seed(3)
I = 5
J <- 6
N <- 2000
d <- .021
p <- (d + 1)/2
h = rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N))
})

#17.8.4 1
library(dplyr)
library(dslabs)
library(stringr)
results_us_election_2016 %>% 
slice_max(order_by = electoral_votes, n = 5)

#17.8.4 2
library(dplyr)
library(dslabs)
library(stringr)
results <- polls_us_election_2016 %>% 
  filter(state != "U.S." & 
  !str_detect(state, "CD") & 
  enddate >= "2016-10-31" &
  (grade %in% c("A+","A","A-","B+") | 
  is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% 
  group_by(state) %>% 
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>% 
  mutate(state = as.character(state))

#17.8.4 3
results %>% arrange(abs(avg))

#17.8.4 4
library(dplyr)
library(dslabs)
library(stringr)
results <- polls_us_election_2016 %>% 
  filter(state != "U.S." & 
  !str_detect(state, "CD") & 
  enddate >= "2016-10-31" &
  (grade %in% c("A+","A","A-","B+") | 
  is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% 
  group_by(state) %>% 
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>% 
  mutate(state = as.character(state))

results = left_join(results, results_us_election_2016, by = 'state')

#17.8.4 5
results_us_election_2016 %>% 
filter(!state %in% results$state) %>% 
pull(state)

#17.8.4 6
library(dplyr)
library(dslabs)
library(stringr)
results <- polls_us_election_2016 %>% 
  filter(state != "U.S." & 
  !str_detect(state, "CD") & 
  enddate >= "2016-10-31" &
  (grade %in% c("A+","A","A-","B+") | 
  is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% 
  group_by(state) %>% 
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>% 
  mutate(state = as.character(state))

results = left_join(results, results_us_election_2016, by = 'state')

results = results %>% 
mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#17.8.4 7
library(dplyr)
library(dslabs)
library(stringr)
results <- polls_us_election_2016 %>% 
  filter(state != "U.S." & 
  !str_detect(state, "CD") & 
  enddate >= "2016-10-31" &
  (grade %in% c("A+","A","A-","B+") | 
  is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% 
  group_by(state) %>% 
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>% 
  mutate(state = as.character(state))

results = left_join(results, results_us_election_2016, by = 'state')

results = results %>% 
mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

mu = 0
tau = 0.02
r = results %>% 
mutate(sigma = sd/sqrt(n), 
  B = sigma^2 / (sigma^2 + tau^2), 
  posterior_mean = B * mu + (1 - B) * avg, 
  posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)))

B = 10000
clinton_EV = replicate(B, {
  r %>% 
  mutate(result = rnorm(length(posterior_mean), posterior_mean, posterior_se), 
  clinton = ifelse(result > 0, electoral_votes, 0)) %>% 
  summarize(clinton = sum(clinton)) %>% 
  pull(clinton) + 7
})

mean(clinton_EV > 269)

#17.8.4 8
library(dplyr)
library(dslabs)
library(stringr)
results <- polls_us_election_2016 %>% 
  filter(state != "U.S." & 
  !str_detect(state, "CD") & 
  enddate >= "2016-10-31" &
  (grade %in% c("A+","A","A-","B+") | 
  is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% 
  group_by(state) %>% 
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>% 
  mutate(state = as.character(state))

results = left_join(results, results_us_election_2016, by = 'state')

results = results %>% 
mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

mu = 0
tau = 0.02
bias_sd = 0.03
r = results %>% 
mutate(sigma = sqrt(sd^2/n + bias_sd^2), 
  B = sigma^2 / (sigma^2 + tau^2), 
  posterior_mean = B * mu + (1 - B) * avg, 
  posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)))

B = 10000
clinton_EV2 = replicate(B, {
  r %>% 
  mutate(result = rnorm(length(posterior_mean), posterior_mean, posterior_se), 
  clinton = ifelse(result > 0, electoral_votes, 0)) %>% 
  summarize(clinton = sum(clinton)) %>% 
  pull(clinton) + 7
})

mean(clinton_EV2 > 269)

#1
library(dplyr)
library(dslabs)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
#  filter(enddate == max(enddate)) %>%
#  ungroup()

c = polls %>% 
mutate(X_hat = mean(spread), 
  se = sd(spread)/sqrt(length(spread)), 
  lower = X_hat - qnorm(0.975) * se, 
  upper = X_hat + qnorm(0.975) * se) %>% 
#select(state, startdate, enddate, pollster, grade, spread, se, lower, upper)
select(spread, X_hat, se, lower, upper)
head(c)

r <- polls %>% 
  mutate(avg = mean(spread), 
  se = sd(spread)/sqrt(length(spread)), 
  start = avg - 1.96*se, 
  end = avg + 1.96*se) %>% 
  select(spread, avg, se, start, end)

head(r)

#1 test
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)

#1 test
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
c = polls %>% 
mutate(x_hat = rawpoll_clinton / (rawpoll_clinton+rawpoll_trump), 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

cis = polls %>% 
mutate(x_hat = rawpoll_clinton / (rawpoll_clinton+rawpoll_trump), 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - 1.96 * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

###teesesesedsrr
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
cis = polls %>% 
mutate(x_hat = rawpoll_clinton / (rawpoll_clinton + rawpoll_trump), 
  se = 2 * sqrt(x_hat * (1-x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, se, lower, upper)

a = data.frame(se=c(0.01088297, 0.02837809), upper=c(0.0413312213, 0.1056345040), spread=c(0.02, 0.05))
a = a %>% mutate(qnorm = (upper-spread)/se)

#3
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
cis = polls %>% 
mutate(x_hat = (spread + 1) / 2, 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p = ci_data %>% 
mutate(hit = actual_spread >= lower & actual_spread <= upper) %>% 
group_by(pollster) %>% 
filter(n()>=5) %>% 
summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>% 
arrange(desc(proportion_hits))
p

#4
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
cis = polls %>% 
mutate(x_hat = (spread + 1) / 2, 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits = ci_data %>% 
mutate(hit = actual_spread >= lower & actual_spread <= upper) %>% 
group_by(state) %>% 
filter(n()>=5) %>% 
summarize(proportion_hits = mean(hit), n = n()) %>% 
arrange(desc(proportion_hits))
p_hits

#5
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
cis = polls %>% 
mutate(x_hat = (spread + 1) / 2, 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits = ci_data %>% 
mutate(hit = actual_spread >= lower & actual_spread <= upper) %>% 
group_by(state) %>% 
filter(n()>=5) %>% 
summarize(proportion_hits = mean(hit), n = n()) %>% 
arrange(desc(proportion_hits))

p_hits$state = reorder(p_hits$state, p_hits$proportion_hits)
library(ggplot2)
p_hits %>% 
ggplot(aes(state, proportion_hits)) + 
geom_bar(stat = 'identity') + 
coord_flip()

#6
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
cis = polls %>% 
mutate(x_hat = (spread + 1) / 2, 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

errors = cis %>% 
mutate(error = spread - actual_spread, 
hit = sign(spread) == sign(actual_spread))

#7
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
cis = polls %>% 
mutate(x_hat = (spread + 1) / 2, 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

errors = cis %>% 
mutate(error = spread - actual_spread, 
hit = sign(spread) == sign(actual_spread))

p_hits = errors %>% 
group_by(state) %>% 
filter(n() >= 5) %>% 
summarize(proportion_hits = mean(hit), n = n())
print(p_hits, n = 100)


#9 test
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
cis = polls %>% 
mutate(x_hat = (spread + 1) / 2, 
  se = 2 * sqrt(x_hat * (1 - x_hat) / samplesize), 
  lower = spread - qnorm(0.975) * se, 
  upper = spread + qnorm(0.975) * se) %>% 
select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

errors = cis %>% 
mutate(error = spread - actual_spread, 
hit = sign(spread) == sign(actual_spread))

library(dslabs)
data(murders)
head(murders)

library(dslabs)
data(polls_us_election_2016)
head(results_us_election_2016)

identical(results_us_election_2016$state, murders$state)

library(dplyr)
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)
head(tab)

library(ggrepel)
tab %>% ggplot(aes(population/10^6, ev, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

tab %>% ggplot(aes(population/10^6, ev, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth(method = "lm", se = FALSE)

tab_1 <- slice(murders, 1:6) %>% select(state, population)
tab_2 <- results_us_election_2016 %>% 
  filter(state%in%c("Alabama", "Alaska", "Arizona", 
                    "California", "Connecticut", "Delaware")) %>% 
  select(state, electoral_votes) %>% rename(ev = electoral_votes)

left_join(tab_1, tab_2, by = 'state')
tab_1 %>% left_join(tab_2, by = 'state')

tab_1 %>% right_join(tab_2, by = 'state')

tab_1 %>% inner_join(tab_2, by = 'state')

tab_1 %>% full_join(tab_2, by = 'state')

tab_1 %>% semi_join(tab_2, by = 'state')

tab_1 %>% anti_join(tab_2, by = 'state')

#bind col
library(dslabs)
data(murders)
data(polls_us_election_2016)
library(dplyr)
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)
t1=tab[,1:2]
t2=tab[,3:4]
t3=tab[,5:8]
nt = bind_cols(t1, t2, t3)

#bind row
library(dslabs)
data(murders)
data(polls_us_election_2016)
library(dplyr)
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)
t1=tab[1:2,]
t2=tab[5:6,]
nt = bind_rows(t1, t2)

#set
library(dslabs)
data(murders)
data(polls_us_election_2016)
library(dplyr)
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)

t1=tab[1:5,]
t2=tab[3:7,]
intersect(t1, t2)

#set2
library(dslabs)
data(murders)
data(polls_us_election_2016)
library(dplyr)
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)

t1=tab[1:5,]
t2=tab[3:7,]
union(t1, t2)

#set3
library(dslabs)
data(murders)
data(polls_us_election_2016)
library(dplyr)
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)

t1=tab[1:5,]
t2=tab[3:7,]
setdiff(t1, t2)

#set4
library(dslabs)
data(murders)
data(polls_us_election_2016)
library(dplyr)
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)

t1=tab[1:5,]
t2=tab[5:1,]
setequal(t1, t2)

#
library(Lahman)
library(dplyr)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

t <- top %>% left_join(Master) %>%
    select(playerID, nameFirst, nameLast, HR)

ts <- Salaries %>% filter(yearID == 2016) %>%
  right_join(t) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

a1 = AwardsPlayers %>% filter(yearID == 2016) %>% right_join(t)

#
library(Lahman)
library(dplyr)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10) 
t <- top %>% left_join(Master) %>%
    select(playerID, nameFirst, nameLast, HR)

a = AwardsPlayers %>% filter(yearID == 2016)
intersect(a$playerID, t$playerID)

#
url = 'https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167'
library(rvest)
h = read_html(url)

library(dplyr)
t = h %>% html_nodes('table')

t = t[[1]] %>% html_table()

t <- t %>% setNames(c("state", "population", "total", "murder_rate")) 
head(t)

#1
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")

html_table(nodes[[21]])

t1 = html_table(nodes[[1]])
t2 = html_table(nodes[[2]])
t3 = html_table(nodes[[3]])
t4 = html_table(nodes[[4]])

#3
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")

t10 = html_table(nodes[[10]])
t19 = html_table(nodes[[19]])

library(dplyr)
t10 = t10 %>% select(-X1) %>% .[-1,] %>% setNames(c("Team", "Payroll", "Average"))
t19 = t19 %>% .[-1,] %>% setNames(c("Team", "Payroll", "Average"))
full_join(t10, t19, by = 'Team')

#4
library(rvest)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)

library(dplyr)
tab = h %>% html_nodes("table")

html_table(tab[[1]])
html_table(tab[[2]])
html_table(tab[[3]])
html_table(tab[[4]])
html_table(tab[[5]], fill = TRUE)

#
library(rvest)
url = 'https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167'
murders_raw <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("state", "population", "total", "murder_rate"))

murders_raw$population[1:3]

as.numeric(murders_raw$population[1:3])

library(readr)
t = parse_number(murders_raw$population[1:3])

mn = murders_raw %>% mutate_at(2:3, parse_number)

#4
library(dplyr)
d = data.frame(Month = 'January', Sales = '$1,560', Profit = '$1,200')
d %>% mutate_at(2:3, parse_number)

d %>% mutate_at(2:3, as.numeric)

d %>% mutate_all(parse_number)

d %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
    mutate_at(2:3, as.numeric)

library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)
sum(is.na(x))

library(dplyr)
reported_heights %>% 
  mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

#
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

#test 1 remove suppressWarnings to see what will happen!
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- as.numeric(x)
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
pt <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

# test 2 only show NA height!
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches)
  ind
}

library(dplyr)
pt <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

#test 3 not include NA
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- inches < smallest | inches > tallest
  ind
}

library(dplyr)
pt <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)
#test 4 to see what not_inches look like
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

a = not_inches(x)

#
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)
inches <- suppressWarnings(as.numeric(x))

#test
h = c('1', 'haha')
x = as.numeric(h)
i <- suppressWarnings(as.numeric(x))
ii = is.na(i) | i < 2

#
library(rvest)
url = 'https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167'
murders_raw <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("state", "population", "total", "murder_rate"))

pattern = ','
library(stringr)
str_detect(murders_raw$total, pattern)

#
library(dslabs)
data(reported_heights)
library(stringr)
str_subset(reported_heights$height, 'cm')

#
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")

str_detect(s, "cm|inches")

#
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

#test
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\d"
str_detect(s, pattern)

#
str_view(s, pattern)
str_view_all(s, pattern)

#
str_view(s, "[56]")

#
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

#
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view_all(s, pattern)

#
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

#
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

# define the problems
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

#
library(stringr)
pattern = "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
#
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
#
str_subset(problems, "inches")
#
str_subset(problems, "''")

#
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

pattern = "^[4-7]'\\d{1,2}$"

problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

#
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

pattern = "^[4-7]\\s*'\\s*\\d{1,2}$"

problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

#
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

pattern = "^[4-7]'\\s\\d{1,2}\"$"
library(stringr)
str_subset(problems, pattern)

#
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

#group
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
#match with group
str_match(s, pattern_with_groups)
#match vs extract
str_extract(s, pattern_with_groups)

#
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

#
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

library(stringr)
str_subset(problems, pattern_with_groups) %>% head()

str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

#improving
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) & 
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  pull(height)

#
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% # convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")# change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]

#test 1
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

#test 2
no <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
no(c(175))
no(c("5'8\""))
no(c(70))
no(c(85))

#test 3
s = c("70", "5 ft", "4'11", "", ".", "Six feet")
pattern = "\\d|ft"
library(stringr)
str_view_all(s, pattern)

#test 3.3
s = c("70", "5 ft", "4'11", "", ".", "Six feet")
pattern = "\\d\\d|ft"
library(stringr)
str_view_all(s, pattern)

#test 8.1
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- 'mo*'
str_detect(animals, pattern)
#test 8.2
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- 'mo?'
str_detect(animals, pattern)

#q 9
schools = c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia",
"U California","California State University")
library(dplyr)
library(stringr)
#9 D
schools %>% 
    str_replace("^Univ\\.?\\s|^U\\.?\\s", "University") %>% 
    str_replace("University ", "University of ")
#9 B
schools = c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia",
"U California","California State University")
library(dplyr)
library(stringr)
schools %>% 
    str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
    str_replace("^University of |^University ", "University of ")

# q13 A
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
    str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
    str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#q13 B
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
    str_replace("\\s+feet|foot|ft\\s+", "'") %>% 
    str_replace("\\s+inches|in|''|\"\\s+", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#q13C
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
    str_replace("\\s*|feet|foot|ft", "'") %>% 
    str_replace("\\s*|inches|in|''|\"", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#q13D
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
    str_replace_all(“\\s”, “”) %>% 
    str_replace("\\s|feet|foot|ft", "'") %>% 
    str_replace("\\s|inches|in|''|\"", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") 
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#
s <- c("5'10", "6'1")
tab <- data.frame(x = s)
library(dplyr)
library(tidyr)
tab %>% separate(x, c("feet", "inches"), sep = "'")

tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
#test
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "left")
#test
tab %>% separate(x, c("feet","inches"), sep = "'")

tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#case 1
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
library(stringr)
str_replace(s, "^([4-7])$", "\\1'0")

#case 2
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
library(stringr)
str_replace(s, "^([4-7])'?$", "\\1'0")

#case 5
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

#trim the end space
str_trim("5 ' 9 ")
#trim the start and end
str_trim(" 5 ' 9 ")
#trim start
str_trim(" 5 ' 9")

#to lower case
s <- c("Five feet eight inches")
str_to_lower(s)

#putting together
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) & 
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  pull(height)

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#
library(dslabs)
data(reported_heights)
x <- as.numeric(reported_heights$height)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

library(dplyr)
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  pull(height)

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

new_heights %>% arrange(height) %>% head(n=7)

#split string
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
head(lines)

library(dplyr)
library(stringr)
x <- str_split(lines, ",") 
x %>% head()

col_names <- x[[1]]
x <- x[-1]

library(purrr)
map(x, function(y) y[1]) %>%
head()

#test
map(x, function(i) {i[1]}) %>%
head()

map(x, 1) %>% head()

#test
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
library(dplyr)
library(stringr)
x <- str_split(lines, ",")
col_names <- x[[1]]
x <- x[-1]
library(purrr)
library(tibble)
dat <- tibble(map_chr(x, 1),  
              map_chr(x, 2),
              map_chr(x, 3),
              map_chr(x, 4),
              map_chr(x, 5)) %>%
              setNames(col_names)

library(readr)
d = tibble(map_chr(x, 1),  
              map_chr(x, 2),
              map_chr(x, 3),
              map_chr(x, 4),
              map_chr(x, 5)) %>%
              mutate_all(parse_guess) %>% 
              setNames(col_names)

#method 2
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
library(dplyr)
library(stringr)
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x <- x[-1,]
colnames(x) = col_names
library(tibble)
library(readr)
x %>% 
as_tibble() %>%
mutate_all(parse_guess)

#recoding
library(dslabs)
data("gapminder")
library(ggplot2)
gapminder %>% 
  filter(region == "Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
# new
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          `Antigua and Barbuda` = "Barbuda",
                          `Dominican Republic` = "DR",
                          `St. Vincent and the Grenadines` = "St. Vincent",
                          `Trinidad and Tobago` = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# review pdf tools, but actually we don't need pdftools
library(dslabs)
data("raw_data_research_funding_rates")
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame() %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% as_tibble()

#q1
a = 'Mandy, Chr and Lau'
library(stringr)
str_split(a, ",|and")
str_split(a, ", | and ")
str_split(a, ",\\s|\\sand\\s")
str_split(a, "\\s?(,|and)\\s?")

#q2 A
schedule = data.frame(day = c('Monday', 'Tuesday'), 
staff = c('Mandy, Chris and Laura', 'Steve, Ruth and Frank'))
library(tidyr)
t <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()
t
#q2 A test
schedule = data.frame(day = c('Monday', 'Tuesday'), 
staff = c('Mandy, Chris and Laura', 'Steve, Ruth and Frank'))
library(tidyr)
t <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) 

#q2 B is wrong
schedule = data.frame(day = c('Monday', 'Tuesday'), 
staff = c('Mandy, Chris and Laura', 'Steve, Ruth and Frank'))
library(tidyr)
t <- separate(schedule, staff, into = c("s1","s2","s3"), sep = “,”) %>% 
  gather(key = s, value = staff, s1:s3)
t

#q2 C
schedule = data.frame(day = c('Monday', 'Tuesday'), 
staff = c('Mandy, Chris and Laura', 'Steve, Ruth and Frank'))
library(tidyr)
t <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest()
t
#q2 C test
schedule = data.frame(day = c('Monday', 'Tuesday'), 
staff = c('Mandy, Chris and Laura', 'Steve, Ruth and Frank'))
library(tidyr)
t <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE))

#q3
library(ggplot2)
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea")) %>%
ggplot(aes(year, life_expectancy, color = country_short)) +
  geom_line()

#q4-7
#q4
library(rvest)
#library(tidyverse)
library(dplyr)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)

library(dplyr)
newName = c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
p = polls[-1,] %>%
setNames(newName)
library(stringr)
p2 = p %>%
filter(str_detect(remain, "%"))

#q5
polls = p2
#q5.1
as.numeric(str_remove(polls$remain, "%"))
#q5.2
as.numeric(polls$remain)/100
#q5.3
library(readr)
parse_number(polls$remain)
#q5.4

#q5.5
as.numeric(str_replace(polls$remain, "%", ""))/100
#q5.6
parse_number(polls$remain)/100

#q6
pt = polls
library(stringr)
str_replace_all(pt$undecided, "N/A", "0")

#q7.1

#q7.2 T
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
ed <- sapply(temp, function(x) x[length(x)])
ed
#q7.3
#q7.4 T
temp <- str_extract_all(polls$dates, "[0-9]+\\s[a-zA-Z]+")
ed <- sapply(temp, function(x) x[length(x)])
ed
#q7.5 T
temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
ed <- sapply(temp, function(x) x[length(x)])
ed
#q7.6 F
temp <- str_extract_all(polls$dates, "\\d{1,2}[a-zA-Z]+")
ed <- sapply(temp, function(x) x[length(x)])
ed
#q7.7 T
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
ed <- sapply(temp, function(x) x[length(x)])
ed

#date and time
library(dslabs)
data("polls_us_election_2016")
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

#lubridate
library(lubridate)
set.seed(2002)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates
tibble(date = dates, 
       month = month(dates),
       day = day(dates),
       year = year(dates))
month(dates, label = TRUE)
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

now() %>% hour()
now() %>% minute()
now() %>% second()
#
library(dslabs)
library(dplyr)
library(ggplot2)
polls_us_election_2016 %>% 
  mutate(week = round_date(startdate, "week")) %>%
  group_by(week) %>%
  summarize(margin = mean(rawpoll_clinton - rawpoll_trump)) %>%
  qplot(week, margin, data = .)
#test
library(dslabs)
library(dplyr)
library(ggplot2)
polls_us_election_2016 %>%
select(startdate, rawpoll_clinton, rawpoll_trump) %>%
mutate(week = round_date(startdate, "week")) %>%
group_by(week) %>%
summarize(n=n(),margin = mean(rawpoll_clinton - rawpoll_trump))

#text mining
library(dslabs)
data("trump_tweets")
#
library(dslabs)
library(lubridate)
library(tidyr)
library(dplyr)
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at) %>% 
  as_tibble()

#test
ct = campaign_tweets %>%
select(source, created_at) %>%
mutate(hour = hour(with_tz(created_at, 'EST')))
#test count
t = ct %>% count(source, hour) %>% group_by(source) %>% mutate(percent = n/sum(n)) %>% ungroup()
#test another way of count
t1 = ct %>% group_by(source, hour) %>% summarize(n=n()) %>% mutate(percent = n/sum(n)) %>% ungroup()

#test delete color='' argument inside labels
library(scales)
#sclaes for using percent_format()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets")

#test str_wrap
a='123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 '
library(stringr)
library(dplyr)
a %>% str_wrap(width=40) %>% cat()
a %>% cat()

#test reorder()
a = data.frame(word=c('ha','ha2','ha3', 'ha3', 'ha', 'ha', 'ha'))
b = a %>% count(word) %>% mutate(word=reorder(word,n))

#
library(dslabs)
library(lubridate)
library(tidyr)
library(dplyr)
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at) %>% 
  as_tibble()

links <- "https://t.co/[A-Za-z\\d]+|&amp;"
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  pivot_wider(names_from = "source", values_from = "n", values_fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

#test
a = tweet_words %>% select(source, created_at, word)

#test 
library(dplyr)
a = data.frame(word = c("'a", "b'b'", "c", "'dd"))
a %>% mutate(word = str_replace(word, "'", ""))

#sentiment
library(dslabs)
library(lubridate)
library(tidyr)
library(dplyr)
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at) %>% 
  as_tibble()

library(tidytext)
library(stringr)
links <- "https://t.co/[A-Za-z\\d]+|&amp;"
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

library(textdata)
nrc <- get_sentiments("nrc")

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% 
  sample_n(5)

#sentiment quant
library(dslabs)
library(lubridate)
library(tidyr)
library(dplyr)
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at) %>% 
  as_tibble()

library(tidytext)
library(stringr)
links <- "https://t.co/[A-Za-z\\d]+|&amp;"
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

library(textdata)
nrc <- get_sentiments("nrc")

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  pivot_wider(names_from = "source", values_from = "n") %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

#test another round
library(dslabs)
library(lubridate)
library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
library(textdata)

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at) %>% 
  as_tibble()

links <- "https://t.co/[A-Za-z\\d]+|&amp;"
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  pivot_wider(names_from = "source", values_from = "n") %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

#test
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

#
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  pivot_wider(names_from = "source", values_from = "n", values_fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

#test
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or > 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#
library(dslabs)
library(lubridate)
options(digits = 3) 
#q3
library(dplyr)
data(brexit_polls)
a = brexit_polls %>%
  mutate(month = month(startdate)) %>%
  group_by(month) %>%
  summarize(n = n())

b = brexit_polls %>% 
  mutate(week = round_date(enddate, "week")) %>%
  group_by(week) %>%
  summarize(n = n())

#q4
c = brexit_polls %>%
  mutate(weekdays = weekdays(enddate)) %>%
  group_by(weekdays) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

#q5
data(movielens)
a = movielens %>% 
  mutate(t = as_datetime(timestamp)) %>%
  mutate(year = year(t)) %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

b = movielens %>% 
  mutate(t = as_datetime(timestamp)) %>%
  mutate(hour = hour(t)) %>%
  group_by(hour) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

#q6
#method1
a = gutenberg_metadata %>%
  filter(title == 'Pride and Prejudice')
#method2
library(stringr)
b = gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

#q7
gutenberg_works(title == "Pride and Prejudice")

#8
words = gutenberg_download(1342) %>% 
  unnest_tokens(word, text)

#9
#  filter(!word %in% stop_words$word ) 

words = words %>%
  filter(!word %in% stop_words$word)
nrow(words)

#10 filter !str_detect(word, "^\\d+$")
words = gutenberg_download(1342) %>% 
  unnest_tokens(word, text)

d = words %>%
  filter(!word %in% stop_words$word & !str_detect(word,"^\\d+$"))
nrow(d)
#37334

e = words %>% filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d$"))
nrow(e)
#37439 wrong

f = words %>% filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d*$"))
nrow(f)

#q8 correct answer
book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)
words <- words %>% anti_join(stop_words)
nrow(words)
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

d = words %>% filter(!str_detect(word,"^\\d+$"))
nrow(d)
#37334 wrong

e = words %>% filter(!str_detect(word,"^\\d"))
nrow(e)
#37331
#let's see who they are!
e1 = words %>% filter(str_detect(word,"^\\d"))

f = words %>% filter(!str_detect(word,"\\d"))
nrow(f)
f1 = words %>% filter(str_detect(word,"\\d"))

#q11
book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)
words <- words %>% anti_join(stop_words)
nrow(words)
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

a = words %>% 
  group_by(word) %>% 
  mutate(n = n()) %>%
  filter(n>100) %>%
  arrange(desc(n))
nrow(a)
#method2
b = words %>%
  count(word) %>%
  filter(n > 100) %>%
  arrange(desc(n))
nrow(b)

#q12
afinn <- get_sentiments("afinn")
a = words %>%
  inner_join(afinn, by = 'word')
nrow(a)
#6064

a %>% filter(value > 0) %>% nrow()
#3413

a %>% filter(value == 4) %>% nrow()
#51

#machine learning
library(dplyr)
library(purrr)
library(ggplot2)
library(caret)
library(dslabs)
data(heights)
h = heights
y = h$sex
x = h$height
set.seed(2007)
# i is the index of a partition of h, length(i) == 10.
i = createDataPartition(y, times = 1, p = 0.5, list = FALSE)
# tr is the train set
tr = h[i, ]
# t is the test set, nrow(t) == length(i)
t = h[-i, ]
# We want to compare the overall accuracy of 2 algos.
# algo 1 (We did not train this simple algo.)
# length(y1) == nrow(t) (== length(i))
y1 = sample(c('Male','Female'), nrow(t), replace = TRUE) %>%
  factor()
mean(y1 == t$sex)
# algo 2
y2 = ifelse(t$height > 62, 'Male', 'Female') %>%
  factor()
mean(y2 == t$sex)
# train
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y2 <- ifelse(tr$height > x, "Male", "Female") %>% 
    factor()
  mean(y2 == tr$sex)
})
max(accuracy)
best_cutoff = cutoff[which.max(accuracy)]
#test
y2 <- ifelse(t$height > best_cutoff, "Male", "Female") %>% 
  factor()
mean(y2 == t$sex)
table(predicted = y2, actual = t$sex)
confusionMatrix(data = y2, reference = t$sex)
#test
cm = confusionMatrix(data = y2, reference = t$sex)

#F1
library(dplyr)
library(purrr)
library(ggplot2)
library(caret)
library(dslabs)
data(heights)
h = heights
y = h$sex
x = h$height
set.seed(2007)
i = createDataPartition(y, times = 1, p = 0.5, list = FALSE)
tr = h[i, ]
t = h[-i, ]
# train
cutoff <- seq(61, 70)
F1 <- map_dbl(cutoff, function(x){
  y2 <- ifelse(tr$height > x, "Male", "Female") %>% 
    factor()
  F_meas(y2, tr$sex)
})
max(F1)
best_cutoff = cutoff[which.max(F1)]
#test
y2 <- ifelse(t$height > best_cutoff, "Male", "Female") %>% 
  factor()
sensitivity(y2, t$sex)
specificity(y2, t$sex)





#data.frame(cutoff=cutoff,accuracy=accuracy) %>% 
#  ggplot(aes(cutoff,accuracy)) + geom_point() + geom_line()



#test to create matrix and list
m = matrix(c(1,2,3,4), nrow = 2, ncol = 2)
#l = list(1,2,3,4)
l = list(m,c(1,2,3,4),i)
i = createDataPartition(y, times = 1, p = 0.5, list = TRUE)
l=list(movienames = c('harr', 'vold'))

i = createDataPartition(y, times = 2, p = 0.5, list = TRUE)

#
