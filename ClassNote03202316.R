# Gabi Faton
# Class Notes
# 03-2023-16


library(tidyverse)

load("data\RAMLDB v4.495\R Data\DBdata[asmt][v4.495].RData")

head(area)

glimpse(stock)
glimpse(timeseries_values_views)
glimpse(taxonomy)

fish = timeseries_values_views %>%
  left_join(stock) %>%
  left_join(taxonomy) %>%
  select(stockid,stocklong,year,TCbest,tsn,scientificname,commonname,region,FisheryType,taxGroup)

fish = fish %>%
  filter(stockid != "ACADREDGOMGB")

glimpse(fish)
dim(timeseries_values_views)
dim(fish)

glimpse(tsmetrics)
tsmetrics %>% filter(tsshort =="TCbest")


ggplot() + 
  geom_line(aes(x=year, y=TCbest, color=stockid), data = fish) +
  theme(legend.position = "none")

fish %>% filter(TCbest>6000000)

fish %>%
  filter(scientificname =="Gadus morhua") %>%
  distinct(region)

cod = fish %>% 
  filter(scientificname == "Gadus morhua",
         region == "Canada East Coast", 
         !is.na(TCbest)) %>%
  group_by(year) %>%
  summarise(total_catch = sum(TCbest)) 

head(cod)

ggplot(aes(x=year, y=total_catch), data=cod) + 
  geom_line() +
  labs(x= "Year", y= "Total Catch (Metric Tons)", 
       title = "Cod Total Catch in East Canadian Coast")

dat = c(1,3,6,2,3,9,-1)
datmax = cummax(dat) # cummax() returns the cumulative maximum value
datsum = cumsum(dat) # cumsum() returns the cumulative sum value
cbind(dat,datmax,datsum)



codcollapse = cod %>%
  mutate(historical_max_catch = cummax(total_catch),
         collapse = total_catch <= 0.1*historical_max_catch)
head(codcollapse)
tail(codcollapse)

codcollapseyear = codcollapse %>% 
  filter(collapse==TRUE) %>% 
  summarize(year=min(year)) %>% 
  pull(year)

ggplot() +
geom_line(aes(y=total_catch, x=year, color=collapse), data=codcollapse) +
  geom_vline(xintercept = codcollapseyear) + 
  xlab("Total catch (Mt)") + ylab("Year") + ggtitle("East Canada Cod")

collapse = fish %>% 
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
         current_collapse = TCbest < 0.10 * historical_max_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()
head(collapse)
glimpse(collapse)

collapse_yr = collapse %>%
  group_by(stockid, stocklong, region) %>%
  filter(collapsed_yet == TRUE) %>%
  summarize(first_collapse_yr = min(year)) %>%
  ungroup()

glimpse(collapse_yr)
head(collapse_yr)

ggplot() + 
  geom_line(aes(y=total_catch, x=year, color=collapse), data=cod_collapse) +
  geom_vline(xintercept = cod_collapse_year) + # Draws vertical line
  #scale_x_continuous(breaks=c(seq(0,2015,10))) + # Add more breaks on x axis
  xlab("Total catch (Mt)") + ylab("Year") + ggtitle("East Canada Cod")


