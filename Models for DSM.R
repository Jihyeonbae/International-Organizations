library(plm)
library("pacman")
p_load(plyr, dplyr, ggplot2, MASS, tidyverse, magrittr, RColorBrewer, readxl,
       readr, rtweet, lubridate, scales, leaflet, academictwitteR, ggmap, quanteda,
       ggmap, jtools, tidytest, googledrive, palmerpenguins, haven)

#wrangling
igo <-readRDS("IGOdata.rds")
DSM_data <- igo%>%
  dplyr::filter(year>=1950) %>%
  dplyr::mutate(AIGO = ifelse(polyarchy < 0.5, 1, 0),
                DIGO = ifelse(polyarchy >-0.5, 1, 0),
                AIGO_polity = ifelse(polity < 6, 1, 0 )) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  dplyr::arrange(ioname, year) %>%
  dplyr::select(ioname, year, polyarchy, DS_sum_st, everything())%>%
  as.data.frame()

write.csv(df, file = "IGOdata.csv")

# DS_sum_st ---------------------------------------------------------------

#modeling (1) one way fixed effects (only year)
DSM_1 <-plm(DS_sum_st
            ~ polyarchy_median,
            data = igo,
            index = c("year"),
            model = "within",
            effect = "individual"
)
summary(DSM_1, vcovBK(DSM_1))

DSM_2 <-plm(DS_sum_st
            ~ polyarchy_median + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social,
            data = igo,
            index = c("year"),
            model = "within",
            effect = "individual"
)
summary(DSM_2, vcovBK(DSM_2))

#modeling (3) two-way fixed effects

DSM_3 <-plm(DS_sum_st
            ~ polyarchy_median + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social,
            data = igo,
            index = c("ioname", "year"),
            model = "within",
            effect = "twoways"
)
summary(DSM_3, vcovBK(DSM_3))

# modeling (4) First difference model

DSM_4 <-plm(DS_sum_st
            ~ polyarchy_median + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social -1,
            data = igo,
            index = c("ioname", "year"),
            model = "fd"
)

summary(DSM_4)

#modeling (5) random effects

DSM_5 <-plm(DS_sum_st
            ~ polyarchy_median + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social,
            data = igo,
            index = c("ioname", "year"),
            model = "random")
summary(DSM_5, vcovBK(DSM_5))





# Binary ------------------------------------------------------------------

#modeling (1) one way fixed effects (only year)
AIGO1 <-plm(DS_sum_st
            ~ AIGO,
            data = igo,
            index = c("year"),
            model = "within",
            effect = "individual"
)
summary(AIGO1, vcovBK(AIGO1))

AIGO2 <-plm(DS_sum_st
            ~ AIGO + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social,
            data = igo,
            index = c("year"),
            model = "within",
            effect = "individual"
)
summary(AIGO2, vcovBK(AIGO2))

#modeling (3) two-way fixed effects

DSM_3 <-plm(DS_sum_st
            ~ AIGO + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social,
            data = igo,
            index = c("ioname", "year"),
            model = "within",
            effect = "twoways"
)
summary(DSM_3, vcovBK(DSM_3))

# modeling (4) First difference model

DSM_4 <-plm(DS_sum_st
            ~ AIGO + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social -1,
            data = igo,
            index = c("ioname", "year"),
            model = "fd"
)

summary(DSM_4)

#modeling (5) random effects

DSM_5 <-plm(DS_sum_st
            ~ AIGO + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social,
            data = igo,
            index = c("ioname", "year"),
            model = "random")
summary(DSM_5, vcovBK(DSM_5))

# Interaction Terms ------------------------------------------------------------------

#modeling (1) one way fixed effects (only year)

interaction1 <-plm(DS_sum_st
            ~ factor(AIGO)*polity_homogeneity + gdp_cap + alliances + disparity
             + law + trade + political + social + number,
            data = igo,
            index = c("year"),
            effect = "twoway"
)
summary(interaction1, vcovBK(interaction1))

plot_model(interaction1, type = "pred", terms = c("number [0, 20, 200]", "AIGO"))

#modeling (3) two-way fixed effects

interaction2 <-plm(DS_sum_st
            ~ AIGO*number + gdp_cap + alliances + disparity + polity_homogeneity
            + number + law + trade + political + social,
            data = igo,
            index = c("ioname", "year"),
            model = "within",
            effect = "twoways"
)
summary(interaction2, vcovBK(interaction2))
