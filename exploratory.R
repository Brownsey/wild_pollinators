library(tidyverse)
library(ggmap)
library(leaps)
library(GGally)
library(DiagrammeR)

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
  mutate(both_years = (both_years - 1)) %>%
  mutate(id = row_number())


data <- merge(data, years)

#Isolating the lat and long
lat_long <- data %>% 
  select(lat,long,both_years) %>%
  distinct()

register_google(key = "AIzaSyB9Hpt0vTWrALpm0iUyJxW6C2IuHZylpC8")
map <- get_map(location = c(lon = mean(lat_long$lon), lat = mean(lat_long$lat)), zoom = 9,
                      maptype = "satellite", scale = 2)

#~~~~~Plotting the locations of all the orchards:
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



#Actually shows like nothing lol

log_data <- data %>%
  mutate(apisAb = log(apisAb), wildAbF = log(wildAbF))

temperature_plot_log <-  ggplot(data = log_data) +
  geom_point(aes(x = temp, y = apisAb), colour = "orange") +
  geom_point(aes(x = temp, y = wildAbF), colour = "green") +
  ggtitle("Abundance of honey bees and wild bees vs temperature") +
  xlab("Temperature") + 
  ylab("Abundance Rating")

#Doesn't show too much
pui_plot <-  ggplot(data = data) +
  geom_point(aes(x = eiqB11, y = apisAb), colour = "orange") +
  geom_point(aes(x = eiqB11, y = wildAbF), colour = "green") +
  ggtitle("Abundance of honey bees and wild bees vs Total PUI") +
  xlab("Total PUI") + 
  ylab("Abundance Rating")
pui_plot

#Doesn't show too much
pui_plot <-  ggplot(data = data) +
  geom_point(aes(x = eiqB11, y = apisAb), colour = "orange") +
  geom_point(aes(x = eiqB11, y = wildAbF), colour = "green") +
  ggtitle("Abundance of honey bees and wild bees vs Total PUI") +
  xlab("Total PUI") + 
  ylab("Abundance Rating")
pui_plot


fungicide_plot <-  ggplot(data = data) +
  geom_point(aes(x = eiqB11.fun, y = apisAb), colour = "orange") +
  geom_point(aes(x = eiqB11.fun, y = wildAbF), colour = "green") +
  ggtitle("Abundance of honey bees and wild bees vs Total fungicide") +
  xlab("Total Fungicide") + 
  ylab("Abundance Rating")
fungicide_plot

Insectcide_plot <-  ggplot(data = data) +
  geom_point(aes(x = eiqB11.ins, y = apisAb), colour = "orange") +
  geom_point(aes(x = eiqB11.ins, y = wildAbF), colour = "green") +
  ggtitle("Abundance of honey bees and wild bees vs Total fungicide") +
  xlab("Total Fungicide") + 
  ylab("Abundance Rating")
Insectcide_plot

###Looking at leaps, although not necessarily best approach due to colinearities 
##Just non-varying terms before year and visit number
summary(regsubsets( apisAb ~ eiqB11 + eiqB11.np	+ eiqB11.fun	+ eiqB11.ins +	eiqB11.ins.np +	
                      eiqB11F.pre	+ eiqB11F.blm	+ eiqB11F.pos	+ eiqB11I.pre	+ eiqB11I.blm	+ 
                      eiqB11I.pos	+ eiqB11I.pos.np	+ eiqB11T.blm	+ eiqB11T.pos	+ size	+ hive.acr	+ X2000nat
, data = data, nvmax = 6)) 
#Let's look at a graph of the one with 7 variables:
#eiqB11.ins, eiqB11.ins.np eiqB11F.pos eiqB11I.pre eiqB11I.blm eiqB11I.pos eiqB11I.pos.np
lm_non_vary <- lm(apisAb ~ eiqB11.ins +  eiqB11.ins.np + eiqB11F.pos +
                    eiqB11I.pre +  eiqB11I.blm + eiqB11I.pos + eiqB11I.pos.np, data)
  plot(lm_non_vary)
  summary(lm_non_vary)

  
  
  ###~~~~ With varying variables ~~~ neither of which appear as relevant at all...
summary(regsubsets( apisAb ~ temp+ bloom.index + eiqB11 + eiqB11.np	+ eiqB11.fun	+ eiqB11.ins +	eiqB11.ins.np +	
                       eiqB11F.pre	+ eiqB11F.blm	+ eiqB11F.pos	+ eiqB11I.pre	+ eiqB11I.blm	+ 
                       eiqB11I.pos	+ eiqB11I.pos.np	+ eiqB11T.blm	+ eiqB11T.pos	+ size	+ hive.acr	+ X2000nat
                     , data = data, nvmax = 6))   
  
 ##Both stepwise regressions so the same predictors as the best
hist_resid <- ggplot(data=data, aes(lm_non_vary$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")
 
#Probably need to look into best ways to plot/compare these


##GGally
#Takes like 3 mins to run: so only run if u really wanna see them all and crashed R session Lol
#ggpairs(data = data, columns = 7:34, title = "All Data predictors and outcomes")

ggpairs(data = data, columns = 7:13, title = "Bees Data")

ggpairs(data = data, columns = 13:34, title = "Predictor Data")

###~~~~ Code Post Meeting 1
#Aim is to look at variable selection - really find out about each variable and what it does.

day_data <- data %>%
  filter(day %in% c("1","2")) %>%
  mutate(year_day = ifelse(str_detect(day, "1") & str_detect(year, "3"), '11', 
                           ifelse(str_detect(day, "2") & str_detect(year, "3"), '12',
                           ifelse(str_detect(day, "1") & str_detect(year, "4"), '21',
                            ifelse(str_detect(day, "2") & str_detect(year, "4"), '22', "55"))))) %>%
  mutate(year_day = factor(year_day, labels = c("Year 3 and Day 1","Year 3 and Day 2", "Year 4 and Day 1", "Year 4 and Day 2")))

#checking data as there should be a result for each
tibble(counts = c(count(day_data %>%
        filter(year_day == "Year 3 and Day 1")),
count(day_data %>%
        filter(year_day == "Year 3 and Day 2")),
count(day_data %>%
        filter(year_day == "Year 4 and Day 1")),
count(day_data %>%
        filter(year_day == "Year 4 and Day 2")))) %>%
  view()

honey_by_year_day <- day_data %>%
  ggplot(aes(x = orchard, y = apisAb, colour = year_day)) +
  geom_point() +
  geom_line(aes(group = year_day)) +
  theme_bw() +
  ggtitle("Abundance of Honey Bees by Year and Visit Day") +
  ylab("Honey Bee Abundance") +
  xlab("Orchard")
honey_by_year_day

wild_by_year_day <- day_data %>%
  ggplot(aes(x = orchard, y = wildAbF, colour = year_day)) +
  geom_point() +
  geom_line(aes(group = year_day)) +
  theme_bw() +
  ggtitle("Abundance of Wild Bees by Year and Visit Day") +
  ylab("Wild Bee Abundance") +
  xlab("Orchard")
wild_by_year_day

#Not sure how to label the lines, other software required the actual model to give percentages :/
first_step <- grViz("
digraph dot {

graph [layout = dot,
       rankdir = LR]

node [shape = circle,
      style = filled,
      color = grey,
      label = '']

node [fillcolor = red,
      label = 'Before Bloom']
a

node [fillcolor = green,
      label = 'Apply Fungicide']
b 

node [fillcolor = green,
      label = 'Apply Insecticide']
c 

node [fillcolor = green,
      label = 'Apply Both']
d

node [fillcolor = green,
      label = 'Apply Nothing']
e

node [fillcolor = orange]

edge [color = grey]
a -> {b c d e}
}")
first_step


whole_decision_tree <- grViz("
digraph dot {

graph [layout = dot,
       rankdir = LR]

node [shape = circle,
      style = filled,
      color = grey,
      label = '']

node [fillcolor = red]
a

node [fillcolor = green]
b c d e

node [fillcolor = orange]

edge [color = grey]
a -> {b c d e}
b -> {f g h i j k}
c -> {l m n o p q}
d -> {q s t u v w}
e -> {x y z aa ab ac}
f -> {ad ae af ag}
g -> {ah ai aj ak}
h -> {al am an ao}
i -> {ap aq ar as}
j -> {at au av aw}
k -> {ax ay az ba}
l -> {bb bc bd be}
m -> {bf bg bh bi}
n -> {bj bk bl bm}
o -> {bn bo bp bq}
p -> {bs bt bu bv}
q -> {bw bx by bz}
r -> {ca cb cc cd}
s -> {ce cf ch ci}
t -> {cj ck cl cm}
u -> {cn co cp cq}
v -> {cr cs ct cu}
w -> {cv cw cx cy}
x -> {cz da db dc}
y -> {dd de df dg}
z -> {dh di dj dk}
aa -> {dl dm dn do}
ab -> {dp dq dr ds}
ac -> {ds dt du dv}
}")
whole_decision_tree


#'~~probably won't work but'~~
#Doesn't work but this is a rough idea of what the percentage outcomes give although we need to 
#think of somehow quantifying bees as the "yield" variable
decision_data <- tibble(
  before = c("Apply Fungicide","Apply Insecticide", "Apply Both", "Apply Nothing"),
  during = c("a","b","c","d"),
  bee_yield = c(300,200,300,400)
)
a <- rpart(bee_yield ~ ., data = decision_data, method = 'class')
rpart.plot(a,box.palette = "blue")
