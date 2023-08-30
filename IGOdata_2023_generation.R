
# Setup ------------------------------------------------------------------
library("pacman")
p_load(plyr, dplyr, ggplot2, MASS, tidyverse, magrittr, RColorBrewer, readxl,
       readr, rtweet, lubridate, scales, leaflet, academictwitteR, ggmap, quanteda,
       ggmap, jtools, tidytest, googledrive, palmerpenguins, haven)
setwd("/Users/jihyeonbae/Google Drive")
qog<-read_csv("~/Google Drive/My Drive/IGO/Data/QOG/qog_bas_ts_jan19.csv")
vdem<-read_csv("~/Google Drive/My Drive/IGO/Data/VDEM/V-Dem-CY-Full+Others-v9.csv")
POLITY <- read_excel("~/Google Drive/My Drive/IGO/Data/POLITY.xls")
MIA <- read_dta("~/Google Drive/My Drive/IGO/Data/DP_May 2021.dta")
COW <- read_dta("~/Google Drive/My Drive/IGO/Data/COW/IGO_stata/igo_year_format_3.dta")
ccode <- read_dta("~/Google Drive/My Drive/IGO/Data/COW/IGO_stata/state_year_format3.dta")

# country data
qog <-qog %>%
  dplyr::select(cname, year, ccodecow, wdi_gdpcapcon2010, cspf_sfi, iiag_rol, wdi_pop, atop_number, p_polity2, chga_demo, wdi_trade, vdem_polyarchy, vdem_libdem)%>%
  dplyr::rename(cow_ccode=ccodecow)%>%
  dplyr::relocate(cow_ccode, cname, year)

#outer merge of IGO
#first, i need to attach cow country code to Pevehouse data
ccode <- ccode %>%
  dplyr::select(ccode, state, year)%>%
  dplyr::rename(country = state)%>%
  dplyr::rename(cow_ccode = ccode) %>%
  dplyr::relocate(cow_ccode, country, year)


COW <- COW %>%
  dplyr::rename(cow_igocode = ionum)%>%
  dplyr::select(-c(sdate, deaddate, dead, integrated, replaced, longorgname, igocode, version, accuracyofpre1965membershipdates, sourcesandnotes, imputed)) %>%
  dplyr::relocate(cow_igocode, ioname, year, political, social, economic)%>%
  pivot_longer(c(`afghanistan`:`zimbabwe`),
                      names_to="country",
                      values_to="membership")

COW <- COW %>%
  dplyr::filter(membership==1)%>%
  dplyr::left_join(ccode, by=c("country","year"))

igo <- COW %>%
  dplyr::left_join(qog, by=c("cow_ccode", "year"))

#generating herfindahl-hirschman index
igo<-igo %>%
  dplyr::group_by(cow_igocode, year)%>%
  mutate(total_pact=sum(wdi_gdpcapcon2010, na.rm=T),
         share=wdi_gdpcapcon2010/total_pact,
         total_poly=sum(vdem_polyarchy, na.rm=T),
         poly_share=vdem_polyarchy/total_poly)


#summarising state level info to igo level
igo <- igo %>%
  dplyr::group_by(cow_igocode, year) %>%
  dplyr::summarise(polyarchy=mean(vdem_polyarchy, na.rm=T),
                polyarchy_median=median(vdem_polyarchy, na.rm=T),
                libdem=mean(vdem_libdem, na.rm=T),
                libdem_median=median(vdem_libdem, na.rm=T),
                polity=mean(p_polity2, na.rm=T),
                polity_median=median(p_polity2, na.rm=T),
                gdp_cap=mean(wdi_gdpcapcon2010, na.rm=T),
                alliances=mean(atop_number, na.rm=T),
                econ_sd=sd(log(wdi_gdpcapcon2010), na.rm=T),
                polity_sd=sd(p_polity2, na.rm=T),
                polyarchy_sd=sd(vdem_polyarchy, na.rm=T),
                number=n(),
                trade=mean(wdi_trade, na.rm=T),
                percentage=sum(chga_demo, na.rm=T)/number,
                political=mean(political),
                social=mean(social),
                economic=mean(economic),
                hh=sum(share*share, na.rm=T),
                hhi=hh-(1/number),
                hh_poly=sum(poly_share*poly_share, na.rm=T),
                hhi_poly=hh_poly-(1/number))

# merging with MIA data

MIA <- MIA %>%
  dplyr::select(ioname, acronym, ionumber, year, inception, typeI, pooling, delegation, delconstit, poolconstit, DS_sum_st)%>%
  dplyr::rename(cow_igocode = ionumber)

df <- MIA %>%
  dplyr::inner_join(igo, by=c("cow_igocode", "year"))


saveRDS(df, "IGOdata_2023.rds")

