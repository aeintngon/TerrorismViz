library(plotly)

library(dplyr)
library(ggplo2)
library(networkD3)
library(igraph)


Sys.setenv("plotly_username"="aeint31")
Sys.setenv("plotly_api_key"="64YagDOY29TsffYihgol")

setwd("C:/Users/Aeint Thet Ngon/Documents/Georgetown University/Data Viz/Takehome")

terrorism=read.csv("terrorism.csv")

us <- terrorism %>% select(region_txt, claimmode_txt, iyear)
us <- us[which(us$claimmode_txt!=""),]
us$region_txt <- as.character(us$region_txt)
us$iyear <- as.character(us$iyear)
us$claimmode_txt <- as.character(us$claimmode_txt)
ssa <- transform(us, count = ave(iyear,region_txt, claimmode_txt, FUN = length))

ssa$count <- as.numeric(as.character(ssa$count))
ssa <- ssa[which(ssa$claimmode_txt!="" & ssa$claimmode_txt!="Unknown"),]

ssa <- distinct(ssa)
# ass <- ssa[which(ssa$attacktype1_txt=="Armed Assault"),c(1,3)]
# bomb <- ssa[which(ssa$attacktype1_txt=="Bombing/Explosion"),c(1,3)]
# fac <- ssa[which(ssa$attacktype1_txt=="Facility/Infrastructure Attack"),c(1,3)]
# assi <- ssa[which(ssa$attacktype1_txt=="Assassination"),c(1,3)]
# hij <- ssa[which(ssa$attacktype1_txt=="Hijacking"),c(1,3)]
# unass <- ssa[which(ssa$attacktype1_txt=="Unarmed Assault"),c(1,3)]
# kid <- ssa[which(ssa$attacktype1_txt=="Hostage Taking (Kidnapping)"),c(1,3)]
# host <- ssa[which(ssa$attacktype1_txt=="Hostage Taking (Barricade Incident)"),c(1,3)]
# unknown <- ssa[which(ssa$attacktype1_txt=="Unknown"),c(1,3)]
# 
# library(reshape2)
# subjmeans <- acast(ssa, iyear~attacktype1_txt, sum)
# data <- as.data.frame(subjmeans)
# data$x <- c(1970:1992, 1994:2016)
# p <- plot_ly(data, x = ~x, y = ~`Armed Assault`, name = 'Armed Assault', type = 'scatter',mode = 'lines') %>%
#   add_trace(y = ~Assassination, name = 'Assassination', mode = 'lines') %>%
#   add_trace(y = ~Hijacking, name = 'Hijacking', mode = 'lines')%>%
#   add_trace(y = ~`Bombing/Explosion`, name = 'Bombing/Explosion', mode = 'lines')%>%
#   add_trace(y = ~`Facility/Infrastructure Attack`, name = 'Facility/Infrastructure Attac', mode = 'lines')%>%
#   add_trace(y = ~`Hostage Taking (Barricade Incident)`, name = '`Hostage Taking (Barricade Incident)`', mode = 'lines')%>%
#   add_trace(y = ~`Hostage Taking (Kidnapping)`, name = 'Hostage Taking (Kidnapping)', mode = 'lines')%>%
#   add_trace(y = ~`Unarmed Assault`, name = 'Unarmed Assault', mode = 'lines') %>% layout(xaxis = "Count", yaxis = "Year")
# 
# 
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
# api_create(p, filename = "us_attacks")
# 
# wa <- us %>% select(weaptype1_txt, attacktype1_txt)
# wa$attacktype1_txt <- as.character(wa$attacktype1_txt)
# wa$weaptype1_txt <- as.character(wa$weaptype1_txt)
# wa_t <- transform(wa, count = ave(weaptype1_txt, attacktype1_txt, FUN = length))
# wa_t <- distinct(wa_t)
# wa_t$count <- as.numeric(as.character(wa_t$count))
# wa_t <- wa_t[which(wa_t$attacktype1_txt!="Unknown"),]
# wa_t <- wa_t[which(wa_t$weaptype1_txt!="Unknown"),]
ssa <- distinct(ssa)
write.csv(ssa, "specifity.csv")
ssa <- read.csv("specifity.csv")
ssa$count <- as.numeric(as.character(ssa$count))
ssa <- ssa[which(ssa$claimmode_txt!=""),]

p <- plot_ly(
  x = ssa$region_txt, y = ssa$claimmode_txt,
  z = ssa$count, type = "heatmap") %>% layout(title="Method of Claiming Responsibility")

api_create(p, filename="us_weaponHeat")
