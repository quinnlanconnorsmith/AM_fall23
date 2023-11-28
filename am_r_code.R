library(tidyverse)


d <- select(am_bosmina, lake, date, month, year, time, family, number)
d$lake[d$lake == "MCD"] <- "McD"
d$year <- as.factor(d$year)
d$month <- as.factor(d$month)

ggplot(d, aes(x=number)) +
  geom_bar() +
  facet_grid(month~lake)

ggplot(d, aes(x = month, y = number)) +
  geom_boxplot() +
  facet_grid(lake~year, scales = "free_y") + 
  xlab("Month") +
  ylab("Number of Bosmina")

#summarized dataset
d_sum <- d %>%
  group_by(year, month, lake, time) %>%
  summarize(number = mean(number, na.rm = TRUE))

d_sum_day <- d_sum %>%
  filter(time != "unk")


ggplot(d_sum, aes(x = month, y = number, fill = lake)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, scales = "free_y") + 
  xlab("Month") +
  ylab("Average number of Bosmina")

ggplot(d_sum_day, aes(x = time, y = number, fill = lake)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~year, scales = "free_y") + 
  xlab("ToD") +
  ylab("Average number of Bosmina")

#T-testing for each year
d_17 <- d[d$year == 2017, ]

# Separate data for each lake
mcd_17 <- d_17[d_17$lake == "McD", "number"]
sb_17 <- d_17[d_17$lake == "SB", "number"]

# Perform t-test
t_17 <- t.test(mcd_17, sb_17)

# Print the result
print(t_17)

#2018
d_18 <- d[d$year == 2018, ]

# Separate data for each lake
mcd_18 <- d_18[d_18$lake == "McD", "number"]
sb_18 <- d_18[d_18$lake == "SB", "number"]

# Perform t-test
t_18 <- t.test(mcd_18, sb_18)

# Print the result
print(t_18)

#2019
d_19 <- d[d$year == 2019, ]

# Separate data for each lake
mcd_19 <- d_19[d_19$lake == "McD", "number"]
sb_19 <- d_19[d_19$lake == "SB", "number"]

# Perform t-test
t_19 <- t.test(mcd_19, sb_19)

# Print the result
print(t_19)

#2020
d_20 <- d[d$year == 2020, ]

# Separate data for each lake
mcd_20 <- d_20[d_20$lake == "McD", "number"]
sb_20 <- d_20[d_20$lake == "SB", "number"]

# Perform t-test
t_20 <- t.test(mcd_20, sb_20)

# Print the result
print(t_20)

#2021
d_21 <- d[d$year == 2021, ]

# Separate data for each lake
mcd_21 <- d_21[d_21$lake == "McD", "number"]
sb_21 <- d_21[d_21$lake == "SB", "number"]

# Perform t-test
t_21 <- t.test(mcd_21, sb_21)

# Print the result
print(t_21)

#2022
d_22 <- d[d$year == 2022, ]

# Separate data for each lake
mcd_22 <- d_22[d_22$lake == "McD", "number"]
sb_22 <- d_22[d_22$lake == "SB", "number"]

# Perform t-test
t_22 <- t.test(mcd_22, sb_22)

# Print the result
print(t_22)

#2023
d_23 <- d[d$year == 2023, ]

# Separate data for each lake
mcd_23 <- d_23[d_23$lake == "McD", "number"]
sb_23 <- d_23[d_23$lake == "SB", "number"]

# Perform t-test
t_23 <- t.test(mcd_23, sb_23)

# Print the result
print(t_23)

#T-test results 
print(t_17)
print(t_18)
print(t_19)
print(t_20)
print(t_21)
print(t_22)
print(t_23)

#21 and 23 are not significant 

#2017 - more in mcd
#2018 - more in mcd 
#2019 - more in mcd
#2020 - mroe in mcd 
#2021 - ns, more in sb 
#2022 - more in sb 
#2023 - ns, more in sb 

#T-test day/night 
#T-testing for each year

# Separate data for each lake
d_day <- d[d$time == "Day", "number"]
d_night <- d[d$time == "Night", "number"]

# Perform t-test
t_time <- t.test(d_day, d_night)

# Print the result
print(t_time)

#significantly more zoops at night (lit back this up)

#Goofing with anovas 
d_17$month <- as.factor(d_17$month)

# Perform ANOVA for each year
anova_17 <- lapply(unique(d$year), function(year) {
  d_17 <- d[d$year == 2017, ]
  aov_17 <- aov(number ~ month * lake, data = d_17)
  return(list(year = year, ANOVA = aov_17))
})

# Print the results
print(anova_17)



# Intro 
# Zoop/food web 
# PRedation specifically on bosmina 
#Introduce experiment/questions/hypotheses 

#Methods 
#Fish removal experiment  
#Sample collection 
#Lab analysis 

#Results 
#Describe what you found 

#Discussion 
#2017 as "normal" 
#Flip to bosmina dominance in 2018-2020
#Compensatory response by fish

#Caveats - Carrying capacity / Ice phenology / other stuff 
#Future direction - weird SB shift, physical characteristics of bosmina (body size, antennule length) 

