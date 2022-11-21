library(tidyverse)
library(dplyr)
library(ggplot2)

incarceration_trends <- read.csv("C:/Users/delan/Documents/info201/data/incarceration_trends.csv")
#View(incarceration_trends)


# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
max_jail_pop <- 
  incarceration_trends %>%
  group_by(year) %>%
  summarize(max_total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
View(max_jail_pop)

max <- max_jail_pop %>%
  filter(max_total_jail_pop == max(max_total_jail_pop)) %>%
  pull(max_total_jail_pop)
print(max)

max_in_2018 <- max_jail_pop %>%
  filter(year == "2018") %>%
  pull(max_total_jail_pop)
print(max_in_2018)

max_in_1970 <- max_jail_pop %>%
  filter(year == "1970") %>%
  pull(max_total_jail_pop)
print(max_in_1970)

dif <- abs(max_in_2018 - max_in_1970)
print(dif)

max_female_jail_pop <-
  incarceration_trends %>%
  group_by(year) %>%
  summarize(max_fem = sum(female_jail_pop, na.rm = TRUE))
View(max_female_jail_pop)

max_f <- max_female_jail_pop %>%
  filter(max_fem == max(max_fem)) %>%
  pull(max_fem)
print(max_f)

max_male_jail_pop <-
  incarceration_trends %>%
  group_by(year) %>%
  summarize(max_male = sum(male_jail_pop, na.rm = TRUE))

max_m <- max_male_jail_pop %>%
  filter(max_male == max(max_male)) %>%
  pull(max_male)
print(max_m)

max_jail_gender_dif <- abs(max_f - max_m)
print(max_jail_gender_dif)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  df <- incarceration_trends %>%
    group_by(year) %>%
    summarize(Total_Jail_Population = sum(total_jail_pop, na.rm = TRUE))
  return(df)
}
#View(get_year_jail_pop())

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  jail_pop_trend_chart <- 
    ggplot(get_year_jail_pop()) +
    geom_col(
      mapping = aes(x = year, 
                    y = Total_Jail_Population)
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Increase of Jail Population in U.S. (1970 - 2018)",
      caption = "The trend of the total jail population in the US from 1970 through 2018"
    )
    
  return(jail_pop_trend_chart)   
} 
#plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_jail_pop_by_states <- function(states){
  df <- incarceration_trends %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(Total_Jail_Population = sum(total_jail_pop, na.rm = TRUE), .groups = "drop")
  return(df)
}
#View(get_jail_pop_by_states(c("WA", "OR", "CA")))

plot_jail_pop_by_states <- function(states) {
  chart <- 
    ggplot(data = get_jail_pop_by_states(states),
           aes(x = year, 
               y = Total_Jail_Population,
               group = state)
           ) +
    geom_line(
      aes(
        linetype = state,
        color = state)
    ) +
    labs(
      title = "The Trend of the Total Jail Population distributed by State (1970 - 2018)",
      caption = "A chart showing the trend of the total jail population by state from 1970 through 2018"
    )
  return(chart)
}
#plot_jail_pop_by_states(c("WA", "TX", "CA"))

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas

jail_pop_by_gender_over_year <- function(){
  df <- incarceration_trends %>%
    group_by(year) %>%
    select(year, female_jail_pop, male_jail_pop)
  return(df)
}
#View(jail_pop_by_gender_over_year())

plot_jail_pop_by_gender_over_year <- function(){
  chart <- 
    ggplot(data = jail_pop_by_gender_over_year(),
           aes(x = male_jail_pop, y = female_jail_pop, group = year)) +
    ylim(0, 20000) +
    geom_point(
      aes(color = year)
    ) +
    labs(
      title = "Jail population trend by Gender (1970 - 2018)",
      caption = "Jail population comparison between female and male through the years 1970-2018"
    )
  return(chart)
}
#plot_jail_pop_by_gender_over_year()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

jail_pop_by_gender_by_state <- function(gender) {
  incarceration_trends$state <- tolower(state.name[match(incarceration_trends$state, state.abb)])
  if(gender == "female"){
    df <- incarceration_trends %>%
      group_by(state) %>%
      summarize(Total_Female_Jail_Pop = sum(female_jail_pop, na.rm = TRUE))
  }else if(gender == "male"){
    df <- incarceration_trends %>%
      group_by(state) %>%
      summarize(Total_Male_Jail_Pop = sum(male_jail_pop, na.rm = TRUE))
  }
  return(df)
}
#View(jail_pop_by_gender_by_state("male"))

state_shape <- function(gender){
  map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(jail_pop_by_gender_by_state(gender), by = "state") # join eviction data
}
#View(state_shape)

# Draw the map setting the `fill` of each state using its eviction rate
jail_pop_distribution_by_gender_by_state <- function(gender){
  if(gender == "female"){
    map <- ggplot(state_shape(gender)) +
      geom_polygon(
        mapping = aes(x = long, y = lat, group = group, fill = Total_Female_Jail_Pop),
        color = "black", # show state outlines
        size = .1        # thinly stroked
      ) +
      coord_map() + # use a map-based coordinate system
      scale_fill_continuous(low = "White", high = "Red", labels = scales::comma) +
      labs(
        fill = "Total_Female_Jail_Population",
        title = "Female Jail Population Distribution Across the US (1970-2018)",
        caption = "The distribution of the female jail population across the United States from 1970 through 2018")
  }else if(gender == "male"){
    map <- ggplot(state_shape(gender)) +
      geom_polygon(
        mapping = aes(x = long, y = lat, group = group, fill = Total_Male_Jail_Pop),
        color = "white", # show state outlines
        size = .1        # thinly stroked
      ) +
      coord_map() + # use a map-based coordinate system
      scale_fill_continuous(low = "Light Blue", high = "Navy", labels = scales::comma) +
      labs(
        fill = "Total_Male_Jail_Population",
        title = "Male Jail Population Distribution Across the US (1970-2018)",
        caption = "The distribution of the male jail population across the United States from 1970 through 2018")
  }
  return(map)
}  
#jail_pop_distribution_by_gender_by_state("male")

## Load data frame ---- 


