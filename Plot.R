
###########plot
igo <-readRDS("IGOdata.rds")
ggplot(data = igo, aes(x=polyarchy, y=poolconstit, by=ioname, color=year)) +
  geom_point()

df1 <- melt(igo,id=c("year", "polyarchy", "poolconstit"))
df1$year <- as.Date(df1$year)
df1<-df1%>%
  dplyr::filter(year>1980)

p <- ggplot(data = df1, aes(x=polyarchy, y=poolconstit))
# group/color by year
p <- p + geom_line(aes(colour=factor(year)))
p <- p + scale_colour_brewer(palette="Set3")
p <- p + facet_wrap(~ year, scales="free", ncol=3)


#panel data plot
ggplot(data = df1, aes(x=polyarchy, y=poolconstit))+
  geom_point()+
  geom_smooth(method="poolconstit~polyarchy")+
  facet_wrap(~year, ncol = 5)
