
# Setup ------------------------------------------------------------------
library("pacman")
p_load(plyr, dplyr, ggplot2, MASS, tidyverse, magrittr, RColorBrewer, readxl,
       readr, rtweet, lubridate, scales, leaflet, academictwitteR, ggmap, quanteda,
       ggmap, jtools, tidytest, googledrive, palmerpenguins)

qog<-read_csv("/Volumes/GoogleDrive/My Drive/IGO/Data/QOG/qog_bas_ts_jan19.csv")
vdem<-read_csv("/Volumes/GoogleDrive/My Drive/IGO/Data/VDEM/V-Dem-CY-Full+Others-v9.csv")
POLITY <- read_excel("/Volumes/GoogleDrive/My Drive/IGO/Data/POLITY.xls")
MIA <- read_dta("/Volumes/GoogleDrive/My Drive/IGO/Data/DP_May 2021.dta")
COW <- read_dta("/Volumes/GoogleDrive/My Drive/IGO/Data/COW/IGO_stata/igo_year_format_3.dta")
ccode <- read_dta("/Volumes/GoogleDrive/My Drive/IGO/Data/COW/IGO_stata/state_year_format3.dta")

# country data
qog <-qog %>%
  dplyr::select(cname, year, ccodecow, wdi_gdpcapcon2010, cspf_sfi, iiag_rol, wdi_pop, atop_number, ht_colonial, p_polity2, chga_demo, wdi_trade, ht_regtype1, vdem_libdem, vdem_polyarchy)%>%
  dplyr::rename(cow_ccode=ccodecow)%>%
  dplyr::relocate(cow_ccode, cname, year)

#outer merge of IGO
#first, i need to attach cow country codne to Pevehouse data
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


#summarising state level info to igo level
igo <- igo %>%
  dplyr::group_by(cow_igocode, ioname, year) %>%
  dplyr::summarise(polyarchy=mean(vdem_polyarchy, na.rm=T),
                libdem=mean(vdem_libdem, na.rm=T),
                gdp_cap=mean(wdi_gdpcapcon2010, na.rm=T),
                state_fragility=mean(cspf_sfi, na.rm=T),
                rule_of_law=mean(iiag_rol, na.rm=T),
                population=mean(wdi_pop, na.rm=T),
                alliances=mean(atop_number, na.rm=T),
                disparity=sd(gdp_cap, na.rm=T),
                number=n(),
                trade=mean(wdi_trade, na.rm=T),
                percentage=sum(chga_demo, na.rm=T)/number,
                political=mean(political),
                social=mean(social),
                economic=mean(economic)) 

# merging with MIA data

MIA <- MIA %>%
  dplyr::select(ioname, acronym, ionumber, year, inception, typeI, pooling, delegation, delconstit, poolconstit)%>%
  dplyr::rename(cow_igocode = ionumber)

df <- MIA %>%
  dplyr::right_join(igo, by=c("cow_igocode", "year"))

ggplot(df, aes(x=year, y=libdem))+geom_line()


