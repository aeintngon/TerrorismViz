
setwd("C:/Users/Aeint Thet Ngon/Documents/Georgetown University/Data Viz/Takehome")

library(ggplot2)
library(dplyr)

terrorism=read.csv("TerrorismData.csv")

g <- ggplot(terrorism, aes(x=reorder(attacktype1_txt,attacktype1_txt,
                                     function(x)-length(x))))
g+geom_bar()+ggtitle("Attack Type Barplot") +
  labs(x="Attack Type",y="Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

g1 <- ggplot(terrorism, aes(x=reorder(targtype1_txt,targtype1_txt,
                                     function(x)-length(x))))
g1+geom_bar()+ggtitle("Target Type Barplot") +
  labs(x="Target Type",y="Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2 <- ggplot(terrorism, aes(x=reorder(weaptype1_txt,weaptype1_txt,
                                      function(x)-length(x))))
g2+geom_bar()+ggtitle("Weapon Type Barplot") +
  labs(x="Weapon Type",y="Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

df<-data.frame(table(terrorism[,c(2,3)]))

df$iyear<-as.character(df$iyear) %>% as.numeric()
df$imonth<-as.character(df$imonth) %>% as.numeric()
ggplot(df, aes(x=iyear, y=imonth,z=Freq))+geom_tile(aes(fill = Freq)) + theme_bw()+ 
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=289, breaks=c(0, 289, 1731), labels=c("min", "289", "max")) +
  ggtitle("Heatmap of the frequency of terrorism over year and month")

temp <- subset(terrorism, iday!=0 & imonth!=0, select=c(iday, imonth))
df<-data.frame(table(temp))

df$iday<-as.character(df$iday) %>% as.numeric()
df$imonth<-as.character(df$imonth) %>% as.numeric()
ggplot(df, aes(x=iday, y=imonth,z=Freq))+geom_tile(aes(fill = Freq)) + theme_bw()+ 
  scale_fill_gradient2(low="blue", high="red", midpoint=500) + theme(legend.position="none")

g <- ggplot(terrorism, aes(x=reorder(propextent,propextent,
                                     function(x)-length(x))))
g+geom_bar()

#creating datetime
terrorism$date=with(terrorism, as.POSIXct(paste(imonth, iday, iyear, sep="-"), format="%m-%d-%Y"))
head(terrorism[,c(2,3,4)])
head(terrorism$date)
     
temp1 <- terrorism %>%
  group_by(date) %>%
  summarize(n=n()) %>% na.omit()

ggplot(temp1, aes(x=date, y=n)) + geom_point(alpha = 1/10) +
  labs(x="Date",y="Count") + ggtitle("Total count of terrorism over time")

write.csv(terrorism, "terrorism.csv")

temp2 <- terrorism %>% group_by(nwound)%>%
  summarize(n=n())
g2 <- ggplot(temp2[!is.na(temp2$nwound),], aes(x=nwound))
g2+geom_bar()+ggtitle("Weapon Type Barplot") +
  labs(x="Weapon Type",y="Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(temp2[!is.na(temp2$nwound),]$n, log="y", type='h', lwd=10, lend=2)

ggplot(temp2[!is.na(temp2$nwound),], aes(x = nwound, y=n)) + geom_histogram(stat="identity") + scale_x_log10()

temp2 <- subset(terrorism, (!is.na(nkill) & nkill > 0))
ggplot(temp2, aes(x = nkill)) + geom_histogram()+ scale_y_log10()+ggtitle("Number of kills") + labs(x="Number killed", y="log of Counts")

                 