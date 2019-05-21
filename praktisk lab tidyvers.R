#Start med laboppgave.
library(tidyverse)
#laste inn data
sykkel<-readr::read_csv("04.csv") #bruker readr pakken for å inportere csv

#mest populære stasjon
start<- sykkel %>% 
  group_by(start_station_name) %>% 
  count() %>% 
  arrange(desc(n)) #festplassen
start

#plot hires and returna per station
sykkel %>% 
  gather(key= variable, value= station, start_station_name, end_station_name) %>% 
  count(variable, station) %>% 
  ggplot(aes(x=station, y= n, fill= variable)) +
  geom_col(position = position_dodge())

# mest populære start og stopp kombinasjon.
sykkel %>% count(start_station_name, end_station_name) %>% arrange(desc(n)) %>% slice(1:2)

#What was the longest/shortest duration of hire? 
sykkel %>% arrange(desc(duration)) #498635
sykkel %>% arrange(duration) #61

#Plot the distribution of hire duration.
sykkel %>% ggplot(aes(x=duration)) + geom_histogram() + xlim(10,3600)

#What is the median duration of hire from each station?
sykkel %>% group_by(start_station_name) %>% summarise(median_duration=median(duration))

#Map this information
map<- sykkel %>% 
  group_by(start_station_name, start_station_latitude, start_station_longitude) %>% 
  summarise(median_duration=median(duration))
map

ggplot(map, aes(x=start_station_longitude, y=start_station_latitude, )) + geom_point(aes(size=median_duration))

#Are there any significant differences in duration between stations.
mod<- lm(duration~start_station_name,data=sykkel)
anova(mod)

#How far does a typical cyclist travel?
sykkel %>% mutate(distans= sqrt()) #avstanden mellom to punkter er pythagoras


#What is the relationship between distance travelled and time taken?

#How fast was the fastest cyclist (for simplicity assume a straight line of travel)