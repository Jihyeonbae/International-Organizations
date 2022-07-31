####Modeling

library(plm)

#wrangling

igo <-readRDS("IGOdata.rds")
igo <- igo%>%
  dplyr::filter(year>=1950) %>%
  dplyr::mutate(AIGO = ifelse(polyarchy < 0.5, 1, 0),
                DIGO = ifelse(polyarchy >-0.5, 1, 0),
                AIGO_polity = ifelse(polity < 6, 1, 0 )) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::na.omit(polyarchy, poolconstit) %>%
  dplyr::arrange(ioname, year) %>%
  dplyr::select(ioname, year, polyarchy, poolconstit, delconstit, everything())%>%
  as.data.frame()

#Model1) Within-FE Model
model1 <-plm(pooling
                 ~ polyarchy + trade + log(gdp_cap) + number + alliances + disparity,
            data = igo,
            index = c("cow_igocode", "year"),
            model = "within",
            effect = "individual"
)
summary(model1, vcovBK(model1))


#Model2) Two way FE Model

model2 <-plm(pooling
             ~ polyarchy + trade + disparity + log(gdp_cap) + number + alliances,
             data = igo,
             index = c("cow_igocode", "year"),
             model = "within",
             effect = "twoways"
)

summary(model2, vcovBK(model2))

#Model3) First Difference Model

model3 <-plm(poolconstit
             ~ polyarchy + trade + disparity + log(gdp_cap) + number + social + economic
             + alliances -1,
             data = igo,
             index = c("ioname", "year"),
             model = "fd"
)

summary(model3)

#Model4) Random Effects Model

model4 <-plm(poolconstit
             ~ AIGO + trade + disparity + log(gdp_cap) + number + social + economic
             + alliances ,
             data = igo,
             index = c("ioname", "year"),
             model = "random"
)

summary(model4)
