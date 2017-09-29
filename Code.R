##Zhila Esna Ashari
####################
library("igraph")
zoo <- read.graph("C:/Users/ZhilaEA/Desktop/Assignment1/soc-sign-Slashdot090216.txt", "gml")



data <- read.table("soc-sign-Slashdot090216.txt",header = FALSE)
data_unsigned <- data[,1:2]
xlist <- graph.data.frame(data_unsigned, directed = TRUE)
data1 <- read.table("1.txt",header = FALSE)

negative <- 0
positive <- 0
for (i in 1:545671){
  if (data(i,3)>0){positive <- positive+1}
}


data[4,2]
xlist <- graph.data.frame(data)
ecount(xlist)
vcount(xlist)
plot(xlist)


diameter(xlist, directed = FALSE, unconnected = TRUE)

m <- betweenness(xlist, weights=NULL)
print(maximum<-max(m))
for (i in 1:length(m)){
  if (m[i]==maximum){print(i)}
}

transitivity(zoo,"localaverage" , "directed")
transitivity(zoo,"globalundirected" )


tkplot(xlist, layout=layout1)



data2 <- read.graph("C:/Users/ZhilaEA/Desktop/Assignment1/1.txt", "ncol", directed=TRUE)

tkplot(data2)
data1$V3

adjacency <- matrix(0,545671,545671)
#for (i in 1:545671)


##############################
zoo <- read.graph("C:/Users/ZhilaEA/Desktop/Assignment1/soc-sign-Slashdot0902162.txt", "ncol", directed=TRUE)

plot(degree.distribution(zoo),col="red")
plot(degree.distribution(zoo),log = "x")
plot(degree.distribution(zoo, mode="in"), degree.distribution(zoo, mode="out"))

print(zoo)
vcount(zoo)

plot(cluster.distribution(zoo), col="red")
clusters(zoo, "strong")$no
plot(clusters(zoo, "strong")$membership)
clusters(zoo, "weak")$no

max(degree(zoo, mode="out"))
min(degree(zoo, mode="out"))

graph.strength(zoo)
plot(graph.strength(zoo),col="red")
plot(graph.strength(zoo,mode="in"))
plot(graph.strength(zoo,mode="out"))
min(graph.strength(zoo,mode="out"))
max(graph.strength(zoo,mode="out"))


for (i in 1:length(m$vector)){
  if (m$vector[i]==maximum){print(i)}
}


average.path.length(zoo, directed=TRUE, unconnected=TRUE)
diameter(zoo, directed = TRUE, unconnected = TRUE, weights = NULL)
#graph.adjc

transitivity(zoo,"localaverage" , "directed")
transitivity(zoo,"globalundirected" )

barplot(path.length.hist(zoo)$res)
#############################################################
########centrality
print(maximum<-max(graph.strength(zoo)))
m <- graph.strength(zoo)
for (i in 1:length(m)){
  if (m[i]==maximum){print(i)}
}

m <- eigen_centrality(zoo)
print(maximum<-max(m$vector))
for (i in 1:length(m$vector)){
  if (m$vector[i]==maximum){print(i)}
}


m <- degree(zoo)
print(maximum<-max(m))
for (i in 1:length(m)){
  if (m[i]==maximum){print(i)}
}


m <- page.rank(zoo)
print(maximum<-max(m$vector))
for (i in 1:length(m$vector)){
  if (m$vector[i]==maximum){print(i)}
}


m <- authority.score(zoo)
print(maximum<-max(m$vector))
for (i in 1:length(m$vector)){
  if (m$vector[i]==maximum){print(i)}
}


m <- hub.score(zoo)
print(maximum<-max(m$vector))
for (i in 1:length(m$vector)){
  if (m$vector[i]==maximum){print(i)}
}

m <- betweenness(zoo, weights=NULL)
print(maximum<-max(m))
for (i in 1:length(m)){
  if (m[i]==maximum){print(i)}
}


