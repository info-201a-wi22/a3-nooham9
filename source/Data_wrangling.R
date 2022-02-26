library("dplyr")
library("tidyverse")
library("ggplot2")

load_data <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_dataset <- read.csv(load_data)
# View(incarceration_dataset)
# colnames(incarceration_dataset)
 
# proportion of black inmates
# jail race pop/ jail pop
incarceration_rep <- incarceration_dataset %>%
  mutate(black_prop_jail = black_jail_pop / total_jail_pop) %>%
  mutate(black_county_prop = black_pop_15to64 / total_pop_15to64) %>%
  mutate(black_rep_prop = black_prop_jail / black_county_prop) %>%
  drop_na() %>%
  select(year, state, county_name, black_prop_jail, black_county_prop, black_rep_prop, total_pop_15to64, total_jail_pop, black_jail_pop)

# highest jail pop
highest_jail_pop <- incarceration_dataset %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(county_name)

# highest black pop
highest_black_pop <- incarceration_dataset %>%
  filter(black_pop_15to64 == max(black_pop_15to64, na.rm = T)) %>%
  pull(county_name)

# highest black jail pop
highest_black_jail_pop <- incarceration_dataset %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)

# where is over representation the highest
highest_black_overrep <- incarceration_rep %>%
  filter(black_rep_prop == max(black_rep_prop, na.rm = T)) %>%
  pull(county_name)

highest_black_overrep_state <- incarceration_rep %>%
  filter(black_rep_prop == max(black_rep_prop)) %>%
  pull(state)

# percent of counties that have over-representation
# nrow overpop / nrow jail counties
# the proportions should ideally be equal which would divide to one.
black_overrep <- incarceration_rep %>%
  select(black_rep_prop)  %>%
  filter(black_rep_prop > 1)

percent_overrep <- nrow(black_overrep) / nrow(incarceration_rep) *100

# average proportion
ave_black_overrep <- incarceration_rep %>%
  summarise(ave_black_ratio = mean(black_rep_prop)) %>%
  pull(ave_black_ratio)

# Scatterplot comparing two continuous variables
# correlation between racial population in jail and racial population in the county
#X- axis = racial population y - axis = jail population
scatter_plot <- ggplot(incarceration_rep, aes(x = black_county_prop, y = black_prop_jail)) +
  geom_point(color = "dark blue") +
  theme_linedraw() +
  geom_smooth(method=lm, se= F) +
  theme(legend.position = "none") +
  ggtitle("Correlation of Black Jailed Population and Black Total Population By County") +
  xlab("Black Demographic") + ylab("Black Demographic in Jail")

# Line graph over time
# - three things: time on x-axis, number on y-axis, group in lines on the chart
# how has the percent of each racial group' population that is in jail changed over time
# - pick a variable with a couple groups (race, age)

# race jail pop / race pop
race_incarceration_prop <- incarceration_dataset %>%
  mutate(black = black_jail_pop / black_pop_15to64) %>%
  mutate(white = white_jail_pop / white_pop_15to64) %>%
  mutate(native = native_jail_pop / native_pop_15to64) %>%
  mutate(aapi = aapi_jail_pop / aapi_pop_15to64) %>%
  mutate(latinx = latinx_jail_pop / latinx_pop_15to64) %>%
  drop_na()

line_incarceration_prop <- race_incarceration_prop %>%
  select(year, black, white, native, aapi, latinx)

line_incarceration_prop_long <- pivot_longer(line_incarceration_prop, "black":"latinx", names_to = "Racial groups", values_to = "Proportion of people in jail")%>%
  group_by(`Racial groups`, year) %>%
  summarize(`Proportion of people in jail` = mean(`Proportion of people in jail`))

line_graph <- ggplot(line_incarceration_prop_long, aes(x = year, y = `Proportion of people in jail`, color = `Racial groups`)) +
  geom_line() +
  ggtitle("Racial proportions in jail over time")

# Map 
#over representation over states
#difference between of jail racial representation and total population racial representation
# jail race/ jail pop -  race /pop
#install.packages("usmap")
library(usmap)

# modifying incarceration_rep to work for this
map_incarceration_rep <- incarceration_rep %>%
  filter(year == max(year)) %>%
  group_by(state)
# View(map_incarceration_rep)
# States and their over-representation of their black population in jails
# ratio between the black demographic in jails and in those states
map_chart <- plot_usmap(data = map_incarceration_rep, values = "black_rep_prop", color = "red") + 
  scale_fill_continuous(low = "blue", high = "red", name = "Black jailed demographic ratio", label = scales::comma) +
  theme(legend.position = "left") +
  ggtitle("Over-representation of the black population in jails by state")


