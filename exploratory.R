library(tidyverse)
library(ggmap)

#Loading the data in from paper ~ downloaded from their resources as a .xlsx, created csv with useful page for quicker reading.
data <- read.csv("data.csv") %>%
  #renaming as column name had a few unrecognised characters in front
  rename(orchard = contains("orchard"))

#Creating an indicator variable to show whether the Orchard appeared in both years or just 1 (0 -> No, 1 -> Yes)  
years <- data %>% 
  select(orchard, year) %>%
  distinct() %>%
  count(orchard) %>%
  rename(both_years = n) %>%
  mutate(both_years = (both_years - 1))


data <- merge(data, years)

#Isolating the lat and long
lat_long <- data %>% 
  select(lat,long,both_years) %>%
  distinct()

register_google(key = "AIzaSyB9Hpt0vTWrALpm0iUyJxW6C2IuHZylpC8")
map <- get_map(location = c(lon = mean(lat_long$lon), lat = mean(lat_long$lat)), zoom = 9,
                      maptype = "satellite", scale = 2)

#Plotting the locations of all the orchards:
plotted_map <- ggmap(map) +
  geom_point(data = lat_long, aes(x = long, y = lat, fill = both_years , alpha = 0.8), size = 5, shape = 21) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE) +
  ggtitle("Orchard Locations") +
  xlab("Longitude") + 
  ylab("Latitude")
plotted_map


# ~~~ Summarising variables ~~~#
#Taking a look at how numbers of wild bees compare with 
abundance_plot <- ggplot(data = data) +
  geom_point(aes(x = orchard, y = apisAb), colour = "orange") +
  geom_point(aes(x = orchard, y = wildAbF), colour = "green") +
  ggtitle("Abundance of honey bees and wild bees vs  temperature") +
  xlab("Orchard") + 
  ylab("Abundance Rating")
abundance_plot


abundance_summary <- data %>% summarise(
  honey_mean = round(mean(apisAb),2),
  wild_mean = round(mean(wildAbF), 2),
  honey_sd = round(sd(apisAb), 2),
  wild_sd = round(sd(wildAbF), 2)
)
abundance_summary 

#Looking at temperature vs bees
temperature_plot <- ggplot(data = data) +
  geom_point(aes(x = temp, y = apisAb), colour = "orange") +
  geom_point(aes(x = temp, y = wildAbF), colour = "green") +
  ggtitle("Abundance of honey bees and wild bees vs temperature") +
  xlab("Temperature") + 
  ylab("Abundance Rating")
temperature_plot

#Looking at the data it seems a quadratic model would be the best fit, shown below
temperature_plot <- temperature_plot +
  stat_smooth(aes(x = temp, y = apisAb), method = "lm", formula = y ~ x + I(x^2), colour = "orange") +
  stat_smooth(aes(x = temp, y = wildAbF), method = "lm", formula = y ~ x + I(x^2), colour = "green") +
  theme_bw()
