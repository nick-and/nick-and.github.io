library(tidyverse)
library(lubridate)
library(png)
options(scipen = 99999)


#pictures
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
  mutate(Park = "Bryce")


capitolr <- read_csv("CapitolReef_monthlyvis.csv",skip = 3)
capitolr <- capitolr %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "CapitolReef")


yellowstone <- read_csv("Yellowstone_monthlyvis.csv",skip = 3)
yellowstone <- yellowstone %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "Yellowstone")


yosemite <- read_csv("Yosemite_monthlyvis.csv",skip = 3)
yosemite <- yosemite %>% 
  select(!Textbox5) %>% 
  pivot_longer(!Year,names_to = "Month",values_to="MonthlyVisitor") %>% 
  mutate(Park = "Yosemite")


z_a <- full_join(zion,arches)

z_a_g <- full_join(z_a,grand_canyon)

zagb <- full_join(z_a_g,bryce)

zagbc <- full_join(zagb,capitolr)

zagbcy <- full_join(zagbc,yellowstone)

zagbcyy <- full_join(zagbcy,yosemite)



img <- readPNG("C:/Users/nicka/Data_Course/baby_yoda_santa.png")

img1 <- readPNG("coyoteg.png")


# Zion


year20 <- filter(zagbcyy,Year==2020)

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


year20 %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))+
  facet_wrap(~Park)



year20yos <- year20 %>% 
  filter(Park == "Yosemite")


year20yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))

year19yos <- zagbcyy %>% 
  filter(Year==2019,Park=="Yosemite")

year19yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))



<div style= "float:right;position: relative;top:20px">
  ```{r, out.width = "600px",echo=FALSE}
knitr::include_graphics("./wp-content/uploads/geoff_nap.jpg")
```

</div>

  
model <- CompleteVis %>% 
  filter(Year>2000)

mod2 <- glm(data = model,
            formula = )
  
  
  
  
  
  
CompleteVis <- zagbcyy %>% 
  group_by(Year,Park) %>% 
  mutate(YearlyVis = sum(MonthlyVisitor))


washington <- read_csv("washingtontemp.csv",skip = 4)

ZiTemp <- washington %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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

zitempchart <- full_join(CompleteVis,ZiTemp)

zitempchartF <- zitempchart %>% 
  filter(Park=="Zion") %>% 
  mutate(YearlyAVGtemp.=mean(Value))

zitempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

zitempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()

washingtonprec <- read_csv("washingtonprec.csv",skip = 4)
ziprec <- washingtonprec %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
ziprecchart <- full_join(CompleteVis,ziprec)

ziprecchartF <- ziprecchart %>% 
  filter(Park=="Zion") %>% 
  mutate(YearlyPrecip.=mean(Value))
ziprecchartF %>% 
  filter(Year>2005) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()








grand <- read_csv("grandtemp.csv",skip = 4)

grandtemp <- grand %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
arctempchart <- full_join(CompleteVis,grandtemp)

arctempchartF <- arctempchart %>% 
  filter(Park=="Arches") %>% 
  mutate(YearlyAVGtemp.=mean(Value))

arctempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

arctempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()

grandprec <- read_csv("grandprec.csv",skip = 4)
arcprec <- grandprec %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
arcprecchart <- full_join(CompleteVis,arcprec)

arcprecchartF <- crprecchart %>% 
  filter(Park=="Arches") %>% 
  mutate(YearlyPrecip.=mean(Value))
arcprecchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()







coconino <- read_csv("coconinotemp.csv",skip = 4)

gctemp <- coconino %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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

gctempchart <- full_join(CompleteVis,gctemp)

gctempchartF <- brytempchart %>% 
  filter(Park=="GrandCanyon") %>% 
  mutate(YearlyAVGtemp.=mean(Value))

gctempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

gctempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()
coconinoprec <- read_csv("coconinoprec.csv",skip = 4)
gcprec <- coconinoprec %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
gcprecchart <- full_join(CompleteVis,gcprec)

gcprecchartF <- gcprecchart %>% 
  filter(Park=="GrandCanyon") %>% 
  mutate(YearlyPrecip.=mean(Value))

gcprecchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()












wayne <- read_csv("waynetemp.csv",skip = 4)
crtemp <- wayne %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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

crtempchart <- full_join(CompleteVis,crtemp)

crtempchartF <- crtempchart %>% 
  filter(Park=="CapitolReef") %>% 
  mutate(YearlyAVGtemp.=mean(Value))

crtempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

crtempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()
waynepr <- read_csv("wayneprec.csv",skip = 4)

crprec <- waynepr %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
crprecchart <- full_join(CompleteVis,crprec)

crprecchartF <- crprecchart %>% 
  filter(Park=="CapitolReef") %>% 
  mutate(YearlyPrecip.=mean(Value))

crprecchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

crprecchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()





teton <- read_csv("tetontemp.csv",skip = 4)
yeltemp <- teton %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
yeltempchart <- full_join(CompleteVis,yeltemp)
yeltempchartF <- yeltempchart %>% 
  filter(Park=="Yellowstone") %>% 
  mutate(YearlyAVGtemp.=mean(Value))

yeltempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

yeltempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()

yeltempchartF %>% 
  filter(Year>1990) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Year,y=YearlyVis))+
  geom_line()

teton.pr <- read_csv("tetonprec.csv",skip = 4)

yelpr <- teton.pr %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
yelprchart <- full_join(CompleteVis,yelpr)

yelprchartF <- yelprchart %>% 
  filter(Park=="Yellowstone") %>% 
  mutate(YearlyPrecip.=sum(Value))

yelprchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

yelprchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()









mariposa <- read_csv("YosTemp.csv",skip = 4)

YosTemp <- mariposa %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
YosTemp %>% 
  filter(Park=="Yosemite") %>% 
  mutate(YearlyAVGtemp.=mean(Value))

YosTempChart <- full_join(CompleteVis,YosTemp)

YosTempChartF <- YosTempChart %>% 
  filter(Park=="Yosemite") %>% 
  mutate(YearlyAVGtemp.=mean(Value))
YosTempChartF %>% 
  ggplot(aes(x=Year,y=YearlyAVGtemp.))+
  geom_point()+
  geom_line()

YosTempChartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

mariposaprec <- read_csv("mariposaprec.csv",skip = 4)
yosprec <- mariposaprec %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
yosprchart <- full_join(CompleteVis,yosprec)

yosprchartF <- yosprchart %>% 
  filter(Park=="Yellowstone") %>% 
  mutate(YearlyPrecip.=sum(Value))

yosprchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()

yosprchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_boxplot()











g <- read_csv("abcztemp.csv",skip=4)


gartemp <- g %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
brytempchart <- full_join(CompleteVis,gartemp)

brytempchartF <- brytempchart %>% 
  filter(Park=="Bryce") %>% 
  mutate(YearlyAVGtemp.=mean(Value))

brytempchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()


brytempchart <- full_join(CompleteVis,gartemp)

brytempchartF <- brytempchart %>% 
  filter(Park=="Bryce") %>% 
  mutate(YearlyAVGtemp.=mean(Value))


garfieldprec <- read_csv("garfieldprec.csv",skip = 4)
garpr <- garfieldprec %>% 
  mutate(Monthnumber = as.numeric(str_sub(Date,start = 5,end = 6))) %>%  
  mutate(Year=as.numeric(str_sub(Date,end=4))) %>%
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
garprchart <- full_join(CompleteVis,garpr)

garprchartF <- garprchart %>% 
  filter(Park=="Bryce") %>% 
  mutate(YearlyPrecip.=mean(Value))

garprchartF %>% 
  filter(Year>2010) %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=Value))+
  geom_boxplot()







zbctemp <- garset %>%
  group_by(Year,Park) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

zbctemp %>% 
  filter(Park=="Bryce") %>% 
  ggplot(x=Month,y=YearlyTemp)+
  geom_boxplot(stat = "identity")
  



















months <- c("January","February","March",
            "April","May","June","July",
            "August","September","October",
            "November","December")
vals <- 1:12

df <- data.frame(months,y=vals)

df$months %>% factor(levels = c("January","February","March",
                                "April","May","June","July",
                                "August","September","October",
                                "November","December"))




#Temperature Data



temp <- read_csv("IronTempF.csv", skip = 3)


almost_temp <- temp %>% 
  mutate(Monthnumber=str_sub(Date,start=5,end=6)) %>% as.numeric() %>% 
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


vis_temp_y %>% 
  filter(Year>1980) %>% 
  group_by(Year) %>% 
  ggplot(aes(x=Year,y=YearlyVis,color=Park))+
  geom_line()


#graph with temperature

zbctemp <- vis_temp_y %>% 
  group_by(Year,Park) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

zbctemp %>% 
  filter(Park == c("Zion","bryce","capitolr")) %>% 
  ggplot(aes(x=Year,y=YearlyTemp))+
  geom_line()


################

#yosemite temp

yostemp <- read_csv("yosemitetemp.csv")

zbcr <- read_csv("ZBCRtemp.csv")

utahparktemp <- zbcr %>% 
  mutate(Year = str_sub(date,start = 1,end=4)) %>% 
  mutate(MeanMonthly = )
  filter(Year > 1978)

utahparktemp %>% 
  group_by(Year) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

ZBCRtemp <- zbcr %>% 
  mutate(Monthnumber=str_sub(Date,start=5,end=6)) %>% as.character() %>% 
  mutate(Year=str_sub(Date,end=4)) %>% as.character()










Tempxyear <- vis_temp_y %>% 
  group_by(Year,Park) %>% 
  summarise(YearlyTemp=mean(Value,na.rm = TRUE))

Tempxyear %>% 
  ggplot(aes(x=Year,y=YearlyTemp,color=Park))+
  geom_line()

glimpse(Tempxyear)



#Distance of each park from closest major city

cities <- data.frame(Park = c("Zion","Arches","GrandCanyon","bryce","capitolr","yellowstone",
                    "yosemite"),
           Distance_from_Major_City.mi = c(1.1,5,6,2.4,4.6,57,14.9),
           NearestCity = c("Springdale","Moab","Tusayan","BryceCanyonCity","Torrey",
                            "Jackson","El Portal"),
           Hotels = c(59,100,20+22,43,4,99+10,133+11),
           Size_acres = c(146597,76679,1218375.5,35835,241900,2219789,759620),
           Hikes = c(103,16,110,32,42,240,276))
ac <- full_join(zagbcyy,cities)

full_join(vis_temp_y,cities)

cities %>% 
  ggplot(aes(x=NearestCity,y=Hotels,fill=Park))+
  geom_bar(stat = "identity")

glimpse(cities)


finalset <- full_join(vis_temp_y,cities)










ftemp <- as.numeric(almost_temp$Year)


visitor_temp <- full_join(almost_temp,zion)


lubridate::month("JAN",format="%M")

months <- month.abb
months <- months %>% str_to_upper()
match(months,month_number)


data.frame(months) %>% 
  mutate(monthnumber=case_when(months == "JAN" ~ 1,
                               months == "FEB" ~ 2,
                               months == "MAR" ~ 3,
                               months == "APR" ~ 4,
                               months == "MAY" ~ 5,
                               months == "JUN" ~ 6,
                               months == "JUL" ~ 7,
                               months == "AUG" ~ 8,
                               months == "SEP" ~ 9,
                               months == "OCT" ~ 10,
                               months == "NOV" ~ 11,
                               months == "DEC" ~ 12))




#combining two plots

year20yos <- year20 %>% 
  filter(Park == "yosemite")


year20yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))

year19yos <- zagbcyy %>% 
  filter(Year==2019,Park=="yosemite")

year19yos %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor))+
  geom_line(aes(group = 1))+
  ylab("Monthly Visitors")+
  theme(axis.text.x = element_text(angle = 90,size = 5))

zagbcyy %>% 
  mutate(Month = factor(Month,levels = months)) %>% 
  filter(Park=="Zion",Year>2018) %>%
  group_by(Year) %>% 
  ggplot(aes(x=Month,y=MonthlyVisitor,color=Year))+
  geom_line(aes(group = 1))





#Help from class

day1 <- 197901

day1 %>% 
  as.character() %>% 
  str_sub(end = 4)

data.frame(day1) %>% 
  mutate(Month=str_sub(day1,start=5,end=6)) %>% 
  mutate(Year=str_sub(day1,end=4)) %>% 
  mutate(day ="01") %>% 
  mutate(date = as.POSIXct(paste0(Year,"-",Month,"-",day)))





