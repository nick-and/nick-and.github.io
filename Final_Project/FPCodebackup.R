library(tidyverse)
library(lubridate)
library(easystats)
library(png)

options(scipen = 99999)

Sys.time()%>% as.POSIXct()


#city stats
cities <- data.frame(Park = c("Zion","Arches","GrandCanyon","bryce","capitolr","yellowstone",
                              "yosemite"),
                     Distance_from_Major_City.mi = c(1.1,5,6,2.4,4.6,57,14.9),
                     NearestCity = c("Springdale","Moab","Tusayan","BryceCanyonCity","Torrey",
                                     "Jackson","El Portal"),
                     Hotels = c(59,100,20+22,43,4,99+10,133+11),
                     CampUnits = c(299,51,463,200,71,1866,1471))




# Zion
zion <- read_csv("Zion_monthlyvis.csv",skip = 3)
zion <- zion %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "Zion")


arches <- read_csv("Arches_monthlyvis.csv",skip = 3)
arches <- arches %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "Arches")

grand_canyon <- read_csv("GrandCanyon_monthlyvis.csv",skip = 3)
grand_canyon <- grand_canyon %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "GrandCanyon")

bryce <- read_csv("BryceC_monthlyvis.csv",skip = 3)
bryce <- bryce %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "bryce")


capitolr <- read_csv("CapitolReef_monthlyvis.csv",skip = 3)
capitolr <- capitolr %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "capitolr")


yellowstone <- read_csv("Yellowstone_monthlyvis.csv",skip = 3)
yellowstone <- yellowstone %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "yellowstone")


yosemite <- read_csv("Yosemite_monthlyvis.csv",skip = 3)
yosemite <- yosemite %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "yosemite")


z_a <- full_join(zion,arches)

z_a_g <- full_join(z_a,grand_canyon)

zagb <- full_join(z_a_g,bryce)

zagbc <- full_join(zagb,capitolr)

zagbcy <- full_join(zagbc,yellowstone)

zagbcyy <- full_join(zagbcy,yosemite)


year20 <- filter(zagbcyy,Year==2020)

year19 <- filter(zagbcyy,Year==2019)

months <- c("JAN","FEB","MAR",
            "APR","MAY","JUN","JUL",
            "AUG","SEP","OCT",
            "NOV","DEC")
monthss <- c("JAN","FEB","MAR",
             "APR","MAY","JUN","JUL",
             "AUG","SEP","OCT",
             "NOV","DEC")


year20 %>% 
  mutate(Month = factor(Month,levels = months))
temp <- read_csv("IronTempF.csv", skip = 3)


almost_temp <- temp %>% 
  mutate(Monthnumber=str_sub(Date,start=5,end=6) %>% as.numeric()) %>% 
  mutate(Year=str_sub(Date,end=4) %>% as.numeric()) %>%
  mutate(day =01) %>% 
  mutate(date = as.POSIXct(paste0(Year,"-",Monthnumber,"-",day))) %>% 
  mutate(Month = case_when(Monthnumber == 1 ~ "JAN",
                           Monthnumber == 2 ~ "FEB",
                           Monthnumber == 3 ~ "MAR",
                           Monthnumber == 4 ~ "APR",
                           Monthnumber == 5 ~ "MAY",
                           Monthnumber == 6 ~ "JUN",
                           Monthnumber == 7 ~ "JUL",
                           Monthnumber == 8 ~ "AUG",
                           Monthnumber == 9 ~ "SEP",
                           Monthnumber == 10 ~ "OCT",
                           Monthnumber == 11 ~ "NOV",
                           Monthnumber == 12 ~ "DEC"))



vis_temp <- full_join(zagbcyy,almost_temp)

#Graph with Temperature

temp_20 <- filter(vis_temp,Year == 2020)





#Adding yearly visitors
vis_temp <- full_join(zagbcyy,almost_temp)

vis_temp_y <- vis_temp %>% 
  group_by(Year,Park) %>% 
  mutate(YearlyVis = sum(MonthlyVisitor))

zbctemp <- vis_temp_y %>% 
  group_by(Year,Park) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

vis_temp_y %>% 
  filter(Year==c(2019,2020),
         Park=="yosemite") %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor,color=Year))+
  geom_line()
acy <- full_join(vis_temp_y,cities)

mod1 <- glm(data = acy,
               formula = YearlyVis~Hotels+Hikes+Distance_from_Major_City.mi)
summary(mod1)
anova(mod1)
report(mod1)
