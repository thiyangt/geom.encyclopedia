library(drone)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggalt)
library(ggbeeswarm)
library(ggforce)
library(ggmosaic)

data(worldbankdata)
View(worldbankdata)

worldbankdata <- worldbankdata |> drop_na()
a1 <- ggplot(worldbankdata,
             aes(x=Income, y=Cooking)) + 
  geom_boxplot(col="black", fill="white") +
  theme(legend.position = "none")+
  xlab("") + ylab("") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 


a2wb <- worldbankdata |> filter(Income == "UM")
a2 <- ggplot(a2wb,
             aes(x=Cooking)) + 
  geom_density(, fill="black")+
  theme(legend.position = "none")+
  xlab("") + ylab("")+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 


a3wb <- worldbankdata |> filter(Income == "UM")
a3 <- ggplot(a3wb,
             aes(x=Cooking)) + 
  geom_histogram(col="white", fill="black") +
  theme(legend.position = "none")+
  xlab("") + ylab("") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

a4 <- worldbankdata |>
  filter(Year == 2021) |>
  group_by(Income) |>
  summarise(n = n()) |>
  ggplot(aes(x = Income, y = n)) +   
  geom_col(fill="black")  +
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 


a5 <- worldbankdata |>
  filter(Income == "L") |>
  ggplot(aes(y = Cooking, x=Electricity)) + 
  geom_point() + 
  geom_encircle(s_shape=0.2, expand=0.01,
                fill="Black",alpha=0.4)+
  xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

worldbankdata2021 <- worldbankdata |> filter(Year == 2021)
a6 <- ggplot(worldbankdata2021, aes(x=Income, y=Electricity)) + 
  geom_quasirandom()+
  xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

world = map_data("world")
a7 <- 
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    fill = "white", colour = "black", size=0.5) +
  xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

a8 <- worldbankdata |>
  filter(Country == "Bangladesh" ) |> 
  filter(Year >= 2013 & Year <= 2021) |>
  ggplot(aes(x=Year, y=Electricity)) + 
  geom_point() +
  geom_smooth(col="black") +
  xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

a9 <- worldbankdata |>
  filter(Country == "Bangladesh") |>
  filter(Year >= 2013 & Year <= 2021) |>
  ggplot(aes(x=Year, y=Electricity)) + 
  geom_point() + 
  geom_area(fill="black", alpha=0.5) +
  xlab("") + ylab("") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

(a1|a2|a3)/(a4|a5|a6)/(a7|a8|a9) + theme_bw()
