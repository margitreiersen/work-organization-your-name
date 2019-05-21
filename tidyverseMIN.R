library("tidyverse")

#pipes %>% cmd-shift-m ####
data(BCI, package = "vegan")

plot(sort(colSums(BCI), decreasing = TRUE))

BCI %>% colSums() %>% sort(decreasing = TRUE)

# one table functions ####
iris<-as_tibble(iris)
iris
#select
iris %>% select(Sepal.Length, Species)
iris %>% select(-Sepal.Length)
iris %>% select(Sepal.Width :Species) #tar alle kollonene fra første:siste

#rename
iris %>% rename(sepal.length= Sepal.Length) #rename(nytt navn = gammelt navn)

#filter
iris %>% filter(Sepal.Length>5, Petal.Length<2)

#grup_by
iris %>% group_by(Species) #beveger ingenting, men den bare vet at disse hører sammen

#mutat
iris %>% mutate(petal.area=Petal.Length*Petal.Width) #tilfører en ny kollone
iris %>% mutate(Species = toupper(Species)) #alt blir til store bokstaver

#
iris %>% group_by(Species) %>% summarise(mean_petal_length = mean(Petal.Length), sd_petal_length = sd(Petal.Length))

iris %>% group_by(Species) %>% mutate(mean_petal_length=mean(Petal.Length))

#arrenge => systematiser
iris %>% arrange(Petal.Length) #fra minste til størst
iris %>% arrange (desc(Petal.Length)) #fra størst til minst

iris %>% group_by(Species) %>% arrange(Petal.Length) %>% slice(1:3) #vis den minste 3delen av petal.length i gruppene art

iris %>% group_by(Species) %>% nest() #Lager en ny tabble til hver art

iris %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ lm(Sepal.Length ~Sepal.Width, data = .))) %>% 
  mutate(coef=map(mod, broom::tidy)) %>% 
  unnest(coef)

# 
iris %>% 
  rownames_to_column() %>% 
  gather(key= variable, value= measurment, -Species, -rowname) %>% 
  group_by(Species, variable) %>% 
  summarise(mean=mean(measurment))

iris %>% 
  rownames_to_column() %>% 
  gather(key= variable, value= measurment, -Species, -rowname) %>% 
  ggplot(aes(x=variable, y= measurment, fill= Species))+geom_violin()

#two table function ####
#left_join, full_join, semi_join, anti_join

#n table function####
#bind_rows, bind_cols, crossing
