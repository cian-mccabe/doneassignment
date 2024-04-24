install.packages("tidyverse")
install.packages("plotly")
library(tidyverse)
library(plotly)
install.packages("maps")
unicef_indicator_22 <- read_csv("unicef_indicator_22.csv")

#map 
obs_value_2023 <-unicef_indicator_22 %>%
  filter(time_period ==2023)
map_world <- map_data("world")
map_obs_value_2023 <- full_join(map_world, obs_value_2023, by = c("region" = "country"))
plot(obs_value_2023)
ggplot(map_obs_value_2023) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  labs(title = "Net Migration Rate 2023") 



#ggplot 1
ggplot(data = unicef_indicator_22)
ggplot(data = gapminder)
ggplot(data = unicef_indicator_22) +
  aes(x = time_period, y = obs_value, color = Continent) +
  geom_line() +
  theme_classic() +
  labs( x = "Time Period",y = "Net Migration Rate Value", title = "Evolution of Net Migration rate per continent.") 

install.packages("plotly")
library(plotly)
p <- ggplot(data = unicef_indicator_22) +
  aes(x = time_period, y = obs_value, color = Continent) +
  geom_line() +
  theme_classic() +
  labs(x = "Time Period", y = "Net Migration Rate Value", title = "Evolution of Net Migration Rate per Continent.", subtitle = "Biggest Outlier in continent per year")
interactive_plot <- ggplotly(p)
interactive_plot


# scatter plot 1
ggplot(filtered_data) +
  aes(time_period, obs_value, color = country) +
  geom_point() +
  labs(
    x = "Net Migration Rate Value",
    y = "Time Period",
    title = "Ukraine and Kuwait and the impact of war on Net Migration Rate."
  ) +
  geom_smooth(method = "lm") +
  theme_classic()

filtered_data <- unicef_indicator_22 %>%
  filter(country %in% c("Ukraine", "Kuwait"))



#bar chart 1
filtered_data2 <- unicef_indicator_22 %>%
  filter(country %in% c("Kuwait", "Oman", "Saudi Arabia", "United Arab Emirates") & 
           time_period ==  1990)
ggplot(filtered_data2) +
  aes(x = country, y = obs_value, fill = country) +  # Set fill to distinguish bars
  geom_bar(stat = "identity") +  # Bar chart with value specified by 'obs_value'
  labs(
    title = "Observation Values in 1990 for Kuwait and selected neighboring countries",
    x = "Country",
    y = "Net Migration Rate"
  ) +
  theme_minimal() +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(obs_value, 1)), vjust = -0.5)


#bar chart 2
filtered_data3 <- unicef_indicator_22 %>%
  filter(country %in% c("Ukraine", "Hungary", "Poland", "Romania", "Slovakia") & 
           time_period == 2022)
ggplot(filtered_data3) +
  aes(x = country, y = obs_value, fill = country) +  # Set fill to distinguish bars
  geom_bar(stat = "identity") +  # Bar chart with value specified by 'obs_value'
  labs(
    title = "Observation Values in 2022 for Ukraine and selected neighboring countries",
    x = "Country",
    y = "Net Migration Rate"
  ) +
  theme_minimal()+
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(obs_value, 1)), vjust = -0.5)
