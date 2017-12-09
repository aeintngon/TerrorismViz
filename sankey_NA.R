library(dplyr)
library(networkD3)

library(igraph)
subA <- terrorism[which(terrorism$region_txt=="North America"),]
subA <- subA %>% select(country_txt, weaptype1_txt, attacktype1_txt)
subA$country_txt <- as.character(subA$country_txt)
subA$weaptype1_txt <- as.character(subA$weaptype1_txt)
subA$attacktype1_txt <- as.character(subA$attacktype1_txt)
mat <- transform(subA, count = ave(country_txt, weaptype1_txt,attacktype1_txt, FUN = length))
mat$count <- as.numeric(as.character(mat$count))
mat <- mat1[which(mat1$count>2000),]
mat <- mat[which(mat$attacktype1_txt!="Unknown"),]
df1 <- as.data.frame(cbind(mat$country_txt, mat$attacktype1_txt, mat$count))
df1 <- distinct(df1)
df1$V3 <- as.numeric(as.character(df1$V3))
#df1 <- df1[which(df1$V3>1000),]
colnames(df1) <- c("SourceName", "TargetName", "Weight")
df2<- as.data.frame(cbind(mat$attacktype1_txt, mat$weaptype1_txt, mat$count))
df2 <- distinct(df2)
df2$V3 <- as.numeric(as.character(df2$V3))
colnames(df2) <- c("SourceName", "TargetName", "Weight")
edgeList <- rbind(df1, df2)

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

SN <- sankeyNetwork(Links = edgeList, Nodes = nodeList, Source = "SourceID",
                    Target = "TargetID", Value = "Weight", NodeID = "nName",
                    fontSize = 12, nodeWidth = 40)
SN
networkD3::saveNetwork(SN, "D3_NA_attackandweapon.html", selfcontained = TRUE)

