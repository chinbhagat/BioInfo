GraphReadClass <- function(name) {
  nc = list(
    name = name,
    nodes = character(),
    edges = character(),
    getEdges = function() {
      return(nc$edges)
    },
    getNodes = function() {
      return(nc$nodes)
    }    
  )
  
  nc$readFromFile<-function(fileName){
    doc <- xmlTreeParse(fileName)
    top <- xmlRoot(doc)
    
    xmlNodes <- xpathApply(top,"//project/node")
    
    nodeIds<-character()
    for(i in 1:length(xmlNodes)){
      nodeIds<-c(nodeIds,xmlGetAttr(xmlNodes[[i]],"id"))
    }
    
    edges<-character()    
    
    xmlEdges <- xpathApply(top,"//project/edge")
    
    for(i in 1:length(xmlEdges)){
      edges<-c(edges,xmlGetAttr(xmlEdges[[i]],"from"))
      edges<-c(edges,xmlGetAttr(xmlEdges[[i]],"to"))      
    }
    
    assign("nodes",nodeIds,envir=nc)
    assign("edges",edges,envir=nc)
  }
  
  nc <- list2env(nc)
  class(nc) <- "GraphReadClass"
  return(nc)
}

test <- GraphReadClass(name="Jason")
fileName="C:\\Users\\chin_bhagat\\bioSample.xml"
test$readFromFile(fileName)
#test$getNodes()
#test$getEdges()


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

addEdgeToGraph <- function(graph,from,to){
  print("Adding Edge to graph")
  graph <- graph + edge(from,to)
  return(graph)
}
addMultipleEdgesToGraph <- function(graph,edgeSet){
  print("Adding Multiple Edges to graph")
  graph <- graph + edges(edgeSet)
  return(graph)
}
addMultipleVerticesToGraph <- function(graph,nodeSet){
  print("Adding Multiple Vertices to graph")
  graph <- graph + vertices(nodeSet)
  return(graph)
}
g <- createEmptyGraph()
nodeSet <- c(test$getNodes())
g <- addMultipleVerticesToGraph(g,nodeSet)
# for(i in 1:length(nodeSet)){
#   g <- addVertexToGraph(g,nodeSet[i])
# }
edgeSet <- c(test$getEdges())
g <- addMultipleEdgesToGraph(g,edgeSet)
get.edgelist(g)