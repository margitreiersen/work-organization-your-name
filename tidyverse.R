library("tidyverse")

####pipes %>% ctrl-shift-m ####
data(BCI, package = "vegan") # load some data
x11()# make a new graphical window (not normally needed with Rstudio) - might need quartz() on a mac

#code without pipes
plot(sort(colSums(BCI), decreasing = TRUE))

#code without pipes but spread over several lines
plot(
  sort(
    colSums(BCI)
    , decreasing = TRUE
    )
  )

#Code with intermediate objects
x1 <- colSums(BCI)
x2 <- sort(x1, decreasing = TRUE)
plot(x2)

#code with intermediate objects reusing object name
x <- colSums(BCI)
x <- sort(x, decreasing = TRUE)
plot(x)

#code with pipe %>% 
BCI %>% 
  colSums(na.rm = TRUE) %>% 
  sort(decreasing = TRUE) %>% 
  plot()

#pipe puts object on the left of the pipe into the first available argument in the function on the right of the pipe

####One table functions ####
#select, filter, mutate, group_by, summarise, slice, count, arrange, nest
iris <- as_tibble(iris) #a tibble is a data.frame with a better print function
iris

#select - select columns
iris %>% select(Sepal.Length, Species)
select(iris, Sepal.Length, Species)

iris %>% select(-Sepal.Width) #negative to remove column
iris %>% select(Sepal.Length:Petal.Length, Species)

#rename - rename columns
iris %>% rename(sepal_length = Sepal.Length, spp = Species)
iris %>% rename(`sepal length` = Sepal.Length) # use back-ticks if you want a space etc
iris %>% rename(`1` = Sepal.Length)

#filter - filter rows with one or more logical statements
iris %>%  filter(Sepal.Length > 5, Petal.Length < 2) %>% 
  select(Species)

#equivalent to 
iris[iris$Sepal.Length > 5 &  iris$Petal.Length < 2, "Species"]

#mutate - change or add a column
iris %>% mutate(petal.area = Petal.Length * Petal.Width)
iris %>% mutate(Species = toupper(Species))

#group_by - how should the data be grouped for summarise/mutate
#summarise - produce one-row summary statistic per group
iris %>% group_by(Species) %>% 
  summarise(mean_petal_length = mean(Petal.Length), sd_petal_length = sd(Petal.Length), n_obs = n())#n() finds number of observations in each group
# iris %>% group_by(Species) %>% 
#   summarise(mean(Petal.Length), sd(Petal.Length))#without useful names

iris %>% group_by(Species) %>% 
  mutate(mean_petal_length = mean(Petal.Length)) %>% 
  ungroup()

#arrange - sort tibble by a column
iris %>% arrange(Petal.Length)
iris %>% arrange(desc(Petal.Length))#descending

#slice - pick rows (from each group) with index
iris %>% group_by(Species) %>% arrange(Petal.Length) %>% slice(1:3)

#count
iris %>% count(Species)

#equivalent to 
iris %>% group_by(Species) %>% summarise(n = n())


#nest - make nested tibble by group
iris %>% group_by(Species) %>% nest()

#nest allows arbitary function on nested data using map()
iris %>% 
  group_by(Species) %>% 
  nest() %>% #comment
  #comment
  mutate(mod = map(data, ~lm(Sepal.Length ~ Sepal.Width, data = .))) %>% #run linear model
  mutate(coef = map(mod, broom::tidy)) %>%# extract model coeficients 
  unnest(coef)#unnest coef 


####reshaping data####
#make tidy data
#gather -> longer, spread -> wider
iris %>% 
  rownames_to_column() %>% 
  gather(key = variable, value = measurement, -Species, -rowname) %>% 
  group_by(Species, variable) %>% 
  summarise(mean = mean(measurement))

iris %>% 
  rownames_to_column() %>% 
  gather(key = variable, value = measurement, -Species, -rowname) %>% 
  ggplot(aes(x = variable, y = measurement, fill = Species)) + geom_violin()

####two table function####
#left_join, full_join, semi_join, anti_join

#new test data - a tribble makes a tibble - convenient for imputting small amounts of data
iris_height <- tribble(~Species, ~height,
                       "setosa", 7,
                       "versicolor", 9,
                       "furcata", 11)#not in iris data

#reduced dataset for ease of viewing
iris2 <- iris %>% group_by(Species) %>% slice(1:2) %>% ungroup()

#left join
iris2 %>% left_join(iris_height)

#right join
iris2 %>% right_join(iris_height)

#full join
iris2 %>% full_join(iris_height)

#inner join
iris2 %>% inner_join(iris_height)

#semi join
iris2 %>% semi_join(iris_height)#finds rows of left dataset that are in right dataset

#anti join
iris2 %>% anti_join(iris_height)#finds rows of left dataset that are NOT in right dataset

#Use `by` argument in *_join for more control

####n table functions####
#bind_rows, bind_cols, crossing
bind_rows(iris2,iris2)
