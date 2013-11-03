library(igraph)
ptm <- proc.time()
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
displayHierarchicalDecomposition <- function(g1,topSet){
  if(is.dag(g1)){
    for(i in 1:length(topSet)){
      tmp <- graph.dfs(g1,topSet[i],"out",order = TRUE,order.out = TRUE,dist=TRUE)
      tmp$dist
      maxNum <- tmp$dist
      cat(nodeSet[topSet[i]],"\t",maxNum[which.max(maxNum)],"\n")
    }
  }else{    
    print("Not a Dag")
  }
}
test <- GraphReadClass(name="Jason")
fileName="C:\\Users\\chin_bhagat\\bioSample2.xml"
test$readFromFile(fileName)


g <- createEmptyGraph()
nodeSet <- c(test$getNodes())
g <- addMultipleVerticesToGraph(g,nodeSet)
edgeSet <- c(test$getEdges())
g <- addMultipleEdgesToGraph(g,edgeSet)

#Find all strongly connected components
clust <- clusters(g,"strong")
#numeric array gives cluster id to which each vertex belong. 
#If 2 vertices have same cluster id they belong to same SCC
c3 <- clust$membership

#Hash table (clusterid,vertex list) 
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
#Finding the number of clusters of size greater than 1. These are no of cycles
c4 <- clust$csize
count <- 0
for(i in 1:length(c4)){
  if(c4[i]>1){
    count <- count+1
  }
}
a1 <- vector("list",count)
j <- 0
#creating an array of graphs where each graph is a SCC
for(i in 1:length((keys(h)))){
  l5 <- values(h,i)
  g2 <- graph.empty(n=0, directed=TRUE)
  if(length(l5)>1){ 
    j <- j+1
    c1 <- c(l5)
    g2 <- induced.subgraph(g, c1)
    a1[[j]] <- g2
  }
}
# a2 <- unique(a1)

# get.edgelist(a1[[2]])
# for(i in 1:length(a2)){
#   a8 <- get.edgelist(a2[[i]])
#   print(a8)
#   print("===========")
# }

#Remove edges from main graph which form a cycle by comparing with each SCC graph
listMain <- get.edgelist(g)
for(k in 1:length(a1)){
  vectList = vector()
  listSub <- get.edgelist(a1[[k]])
  
  for(i in 1:((length(listSub))/2)){
    for(j in 1:((length(listMain))/2)){
      
      if((listSub[i] == listMain[j]) && (listSub[i+((length(listSub))/2)] == listMain[j+((length(listMain))/2)])){
        
        vectList <- c(vectList,listMain[j])
        vectList <- c(vectList,listMain[j+((length(listMain))/2)])
        break

      }
    }
  }
  g<- delete.edges(g,E(g, P= c(vectList)))
  
#Collapse all the vertices of a SCC into a single vertex
  for(m in 1:length(V(a1[[k]])$name)){
    for(n in 1:length(c(V(g)))){
      if(V(a1[[k]])$name[[m]] == V(g)$name[[n]])
      {
        V(g)$name[[n]] <- V(a1[[k]])$name[[1]]
      }
      
    }
  }
  
  
}
uniNodes <- unique(c(V(g)$name))
# uniNodes
#create a new graph with reduced edgelist and new vertices
#vertices of a SCC is treated as a single vertex
# g1 <- createEmptyGraph()
# nodeSet1 <- c(uniNodes)
# g1 <- addMultipleVerticesToGraph(g1,nodeSet1)
# vectList1 = vector()
# vectList2 <- get.edgelist(g)
# 
# for(q in 1:((length(vectList2))/2))
# {
#   vectList1 <- c(vectList1,vectList2[q])
#   vectList1 <- c(vectList1,vectList2[q+length(get.edgelist(g))/2])
# }
# edgeSet1 <- vectList1
# # edgeSet1
# g1 <- addMultipleEdgesToGraph(g1,edgeSet1)

# g <- delete.vertices(g, V(g)[8])
# get.edgelist(g1)
# print(V(g1)$name)
# graphVec <- c(V(g)$name)
 graphVec = vector()
#Perform topological sort
 topSet <- topological.sort(g)
# topSet
delVec = vector()
for(i in 1:length(a1)){
  graphVec <- c(V(a1[[i]])$name)
#   print(graphVec[1])
  for(j in 2:length(graphVec)){
    for(k in 1:length(topSet)){
    
        if(nodeSet[topSet[k]] == graphVec[j])
          delVec <- c(delVec,k)
    }
  }
}
# nodeSet[topSet]
# topSet
topSet <- topSet[-c(delVec)]
# delVec <- sort(delVec, decreasing=FALSE)
# countDel <- 0
# for(i in 1: length(delVec)){
# topSet <- topSet[-((delVec[i]-countDel))]
# countDel<-countDel+1
# }
# countDel
# nodeSet[topSet]
displayHierarchicalDecomposition(g,topSet)
