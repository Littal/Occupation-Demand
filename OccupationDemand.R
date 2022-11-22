rm(list=ls())
#setwd("~/...")



# Preparing environment #####

library(tidyverse)
library(readxl)
library(zoo)          # as.yearmon
library(ggthemes)



Sys.setlocale("LC_ALL","English") # to avoid "yearmon" in Hebrew


# Preparing Data #####

#data source: https://data.gov.il/dataset/statisticsjobsperformance/resource/5611e8ec-4688-4635-9582-f6fc778decca

OccDemand <- read_excel("OccupationDemand2015to2020.xlsx", sheet = "1", col_names=TRUE)

OccDemand <- OccDemand %>%
  rename("Occupation" = "Occupations started in april 2015") 

OccDemand$Month <- as.yearmon(OccDemand$Month, "%Y-%m")

OccDemand$District <- OccDemand$District %>%
  recode_factor("מחוז ש\"ת דן" = "Dan",
                "מחוז ש\"ת דרום" = "South", 
                "מחוז ש\"ת צפון" = "North", 
                "מחוז ש\"ת שרון ועמקים" = "Sharon", 
                "מחוז ש\"ת ירושלים" = "Jerusalem") %>%
  fct_relevel(c("Dan", "North", "South", "Sharon", "Jerusalem"))

OccDemand$Occupation <- OccDemand$Occupation %>%
  recode_factor("בעלי מלאכה בתעשייה ובבינוי ובעלי משלח יד דומה" = "Craftsmen in industry and construction",
         "בעלי משלח יד אקדמי" = "Academic professionals", 
         "הנדסאים, טכנאים, סוכנים ובעלי משלח יד נלווה" = "Practical engineers, technicians, agents, and related", 
         "לא הוגדר" = "undefined", 
         "מנהלים" = "Managerial professionals", 
         "מפעילי מתקנים ומכונות, מרכיבי מוצרים וציוד ונהגים" = "Machines operators, products and equipment assembling, and drivers", 
         "עובדי מכירות ושירותים" = "Sales and service workers", 
         "עובדים בלתי מקצועיים" = "Unprofessional workers", 
         "עובדים מקצועיים בחקלאות, בייעור ובדיג" = "Professional workers in agriculture, forestry and fishing", 
         "פקידים כלליים ועובדי משרד" = "General clerks and office workers")

glimpse(OccDemand)


# Exploring Data #####

# Total job demand by districts over months

TotalDistrictMonthly <- OccDemand %>% 
  group_by(District, Month) %>% 
  summarize("TotalOrders"=sum(Orders,na.rm=T), .groups = 'drop')

FigTotalDistrictMonthly <- ggplot(TotalDistrictMonthly,(aes(x=Month,y=TotalOrders,group=District,color=District))) +
  geom_line() + theme_bw() +
  labs(title = "Total Job Demand by Districts (April 2015 - April 2020)", y="Total Job Demand")
FigTotalDistrictMonthly

# Occupation demand by districts over months

UniqOccupation <- unique(OccDemand$Occupation)

for(i in 1:length(UniqOccupation)){
  print(OccDemand %>%
    filter(Occupation == UniqOccupation[i])  %>%
    group_by(District, Month) %>% 
    summarize("TotalOrders"=sum(Orders,na.rm=TRUE), .groups = 'drop') %>%
    ggplot((aes(x=Month,y=TotalOrders,group=District,color=District))) + geom_line() + theme_bw() +
    labs(title = UniqOccupation[i], y="Total Job Demand"))
  }

# Total job demand by Occupation over months

undefined <- which(OccDemand$Occupation=="undefined")

TotalOccupationMonthly <- OccDemand[-undefined,] %>% 
  group_by(Occupation, Month) %>% 
  summarize("TotalOrders"=sum(Orders,na.rm=T), .groups = 'drop')

glimpse(TotalOccupationMonthly)

FigTotalOccupationMonthly <- ggplot(TotalOccupationMonthly,(aes(x=Month,y=TotalOrders,group=Occupation,color=Occupation))) +
  geom_line() + theme_bw() +
  labs(title = "Total Job Demand by Occupations (April 2015 - April 2020)", y="Total Job Demand")
FigTotalOccupationMonthly

# District demand by Occupation over months

UniqDistrict <- unique(OccDemand$District)

for(i in 1:length(UniqDistrict)){
  print(OccDemand[-undefined,] %>%
          filter(District == UniqDistrict[i])  %>%
          group_by(Occupation, Month) %>% 
          summarize("TotalOrders"=sum(Orders,na.rm=TRUE), .groups = 'drop') %>%
          ggplot((aes(x=Month,y=TotalOrders,group=Occupation,color=Occupation))) + geom_line() + theme_bw() +
          labs(title = UniqDistrict[i], y="Total Job Demand"))
}



### Animation

library(gganimate)
library(gifski)
library(magick)


FigTotalDistrictMonthlyAnime <- ggplot(TotalDistrictMonthly,(aes(x=as.Date(Month),y=TotalOrders,group=District,color=District))) +
  geom_line(size = 1) + theme_bw() +
  labs(title = "Total Job Demand by Districts (April 2015 - April 2020)", y="Total Job Demand", x="Months") +
  transition_reveal(as.Date(Month)) 

animate(FigTotalDistrictMonthlyAnime, end_pause = 30, renderer=magick_renderer())


FigTotalDistrictMonthlyAnime2 <- ggplot(TotalDistrictMonthly,(aes(x=as.Date(Month),y=TotalOrders,group=District,color=District))) +
  geom_line(size = 1) + theme_bw() + 
  labs(title = "Total Job Demand by Districts (April 2015 - April 2020)", y="Total Job Demand", x="Months") +
  geom_text(aes(x = as.Date(Month), label = District), hjust = -0.5) +
  #coord_cartesian(clip = 'off') +  
  theme(legend.position="right") +
  transition_reveal(as.Date(Month)) 

animate(FigTotalDistrictMonthlyAnime2, end_pause = 30, renderer=magick_renderer())

#https://aluby.domains.swarthmore.edu/stat041/Labs/Lab11.html
 
