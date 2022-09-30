####Modeling

library(plm)
library("pacman")
p_load(plyr, dplyr, ggplot2, MASS, tidyverse, magrittr, RColorBrewer, readxl,
       readr, rtweet, lubridate, scales, leaflet, academictwitteR, ggmap, quanteda,
       ggmap, jtools, tidytest, googledrive, palmerpenguins, haven)
#wrangling

igo <-readRDS("IGOdata.rds")
igo <- igo%>%
  dplyr::filter(year>=1950) %>%
  dplyr::mutate(AIGO = ifelse(polyarchy < 0.5, 1, 0),
                DIGO = ifelse(polyarchy >-0.5, 1, 0),
                AIGO_polity = ifelse(polity < 6, 1, 0 )) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::arrange(ioname, year) %>%
  dplyr::select(ioname, year, polyarchy, poolconstit, delconstit, everything())%>%
  as.data.frame()

#Model1) Pooled OLS Model
model1 <-lm(poolconstit
             ~ polyarchy + trade + log(gdp_cap) + number + alliances + disparity,
             data = igo)
summary(model1)


#Model2) Within-FE Model only year index
model2 <-plm(poolconstit
                 ~ polyarchy + trade + log(gdp_cap) + number + alliances + disparity,
            data = igo,
            index = c("year"),
            model = "within",
            effect = "individual"
)
summary(model2, vcovBK(model2))


#Model3) Two way FE Model

model3 <-plm(poolconstit
             ~ polyarchy + trade + disparity + log(gdp_cap) + number + alliances,
             data = igo,
             index = c("cow_igocode", "year"),
             model = "within",
             effect = "twoways"
)

summary(model3, vcovBK(model3))


#Model4) First Difference Model

model4 <-plm(poolconstit
             ~ polyarchy + trade + disparity + log(gdp_cap) + number + social + economic
             + alliances -1,
             data = igo,
             index = c("ioname", "year"),
             model = "fd"
)

summary(model4)

#Model5) Random Effects Model

model5 <-plm(poolconstit
             ~ polyarchy + trade + disparity + log(gdp_cap) + number + social + economic
             + alliances ,
             data = igo,
             index = c("ioname", "year"),
             model = "random"
)

summary(model5)
