library(igraph)
GraphReadClass <- function(name) {
  nc = list(
    name = name,
    nodes = character(),
    edges = character(),
    myGraph = graph.empty(),
    getEdges = function() {
      return(nc$edges)
    },
    getNodes = function() {
      return(nc$nodes)
    },  
    getGraph = function() {
      return(nc$myGraph)
    },
    addVertex = function(vertexId){
      nc$myGraph= nc$myGraph + vertices(c(vertexId))
    },
    addEdge = function(from,to){
      nc$myGraph=add.edges(nc$myGraph,c(from,to))
    }
  )  
  
  nc$readFromFile=function(fileName){
    doc <- xmlTreeParse(fileName)
    top <- xmlRoot(doc)
    
    xmlNodes <- xpathApply(top,"//project/node")
    
    nodeIds<-character()
    for(i in 1:length(xmlNodes)){
      nodeIds<-c(nodeIds,xmlGetAttr(xmlNodes[[i]],"id"))
      #       nc$myGraph= nc$myGraph + vertices(c(xmlGetAttr(xmlNodes[[i]],"id")))
      nc$addVertex(xmlGetAttr(xmlNodes[[i]],"id"))
    }
    
    edges<-character()    
    
    xmlEdges <- xpathApply(top,"//project/edge")
    
    for(i in 1:length(xmlEdges)){
      edges<-c(edges,xmlGetAttr(xmlEdges[[i]],"from"))
      edges<-c(edges,xmlGetAttr(xmlEdges[[i]],"to")) 
      #       nc$myGraph=add.edges(nc$myGraph,c(xmlGetAttr(xmlEdges[[i]],"from"),xmlGetAttr(xmlEdges[[i]],"to")))
      nc$addEdge(xmlGetAttr(xmlEdges[[i]],"from"),xmlGetAttr(xmlEdges[[i]],"to"))
    }
    
    assign("nodes",nodeIds,envir=nc)
    assign("edges",edges,envir=nc)
  }
  
  nc <- list2env(nc)
  class(nc) <- "GraphReadClass"
  return(nc)
}



createEmptyGraph <- function(){
  print("Creating Empty Graph")
  g <- graph.empty()
  return(g)
}

addVertexToGraph <- function(graph,vertexId){
  print("Adding Vertex to graph")
  graph <- graph + vertex(vertexId)
  return(graph)
}


addMultipleEdgesToGraph <- function(graph,edgeSet){
  print("Adding Multiple Edges to graph")
  graph <- graph + 
    edges(edgeSet)
  return(graph)
}
addMultipleVerticesToGraph <- function(graph,nodeSet){
  print("Adding Multiple Vertices to graph")
  graph <- graph + vertices(nodeSet)
  return(graph)
}
displayHierarchicalDecomposition <- function(g,topSet){
  if(is.dag(g)){
    for(i in 1:length(topSet)){
      tmp <- graph.dfs(g,topSet[i],"out",order = TRUE,order.out = TRUE,dist=TRUE)
      tmp$dist
      maxNum <- tmp$dist
      cat(nodeSet[topSet[i]],"\t",maxNum[which.max(maxNum)],"\n")
    }
  }else{
    
    print("Not a Dag")
  }
}
test <- GraphReadClass(name="Jason")
fileName="C:\\Users\\chin_bhagat\\bioSample.xml"
test$readFromFile(fileName)
#test$getNodes()
#test$getEdges()

g <- createEmptyGraph()
nodeSet <- c(test$getNodes())
g <- addMultipleVerticesToGraph(g,nodeSet)
edgeSet <- c(test$getEdges())
g <- addMultipleEdgesToGraph(g,edgeSet)
# get.edgelist(g)
# tmp <- graph.dfs(g,1,"out",order = TRUE,order.out = TRUE,dist=TRUE)
# 
# el <- get.edgelist(g, names=FALSE)
# g2 <- graph(rbind(el[,2],el[,1]))
#tree <- unfold.tree(g,"out", nodeSet)
# boolval <- no.clusters(g, "weak")
# boolval
clust <- clusters(g,"strong")
# clust
c3 <- clust$membership
#c3
h <- hash()
for (i in 1:length(c3)){
  if(!(c3[i] %in% (keys(h)))){
  .set(h, c3[i],list(i))
  }else{
    l1 <- values(h,keys=c3[i])
    l1 <- c(l1, i)
    .set(h, c3[i],l1)
  }
}
c4 <- clust$csize
count <- 0
for(i in 1:length(c4)){
  if(c4[i]>1){
    count <- count+1
  }
}
a1 <- vector("list",count)
j <- 0
for(i in 1:length((keys(h)))){
  l5 <- values(h,i)
  if(length(l5)>1){ 
    j <- j+1
    c1 <- c(l5)
     g2 <- induced.subgraph(g, c1)
     a1[[j]] <- g2
  }
}
get.edgelist(a1[[2]])
for(i in 1:length(a1)){
  a8 <- get.edgelist(a1[[i]])
  print(a8)
  print("===========")
}

# listVal <- decompose.graph(g,"weak")
# # get.vertex.attribute(listVal[1])
# for(i in 1:length(listVal)){
#   print(V(listVal[[i]]))
# }
# topSet <- topological.sort(g)
# displayHierarchicalDecomposition(g,topSet)
