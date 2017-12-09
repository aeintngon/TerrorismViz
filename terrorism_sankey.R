setwd("C:/Users/Aeint Thet Ngon/Documents/Georgetown University/Data Viz/Takehome")

library(igraph)
library(dplyr)
library(networkD3)

terrorism=read.csv("TerrorismData.csv")
terrorism$ransomamt=as.numeric(terrorism$ransomamt)
terrorism$ransompaid=as.numeric(terrorism$ransompaid)
terrorism$country_txt=as.character(terrorism$country_txt)
terrorism$gname=as.character(terrorism$gname)
edgeList <- terrorism %>% filter(ransom==1 & iyear==2016) %>% group_by(country_txt, gname) %>% summarize(totalask=sum(ransomamt, na.rm=TRUE))

colnames(edgeList) <- c("SourceName", "TargetName", "Weight")

gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))

nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(gD)$name)

getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
}

(getNodeID("Angola"))

edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName" , "Weight"), 
                        function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                                                TargetID = getNodeID(x$TargetName)))
nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))

betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
rm(betAll, betAll.norm)

dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")
(head(dsAll))

#Create  data frame that contains the Dice similarity between any two vertices
F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
#Place a new column in edgeList with the Dice Sim
(head(edgeList))
edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                        function(x) data.frame(F1(x)))
(head(edgeList))
rm(dsAll, F1, getNodeID, gD)

write.csv(edgeList, "Ransom_edgelist.csv")
write.csv(nodeList, "Ransom_nodelist.csv")

SN <- sankeyNetwork(Links = edgeList, Nodes = nodeList, Source = "SourceID",
              Target = "TargetID", Value = "Weight", NodeID = "nName", unit="US$",
              fontSize = 12, nodeWidth = 40)
SN
networkD3::saveNetwork(SN, "D3_ransomflow.html", selfcontained = TRUE)
