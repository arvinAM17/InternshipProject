library(igraph)
library(ggplot2)
library(rgdal)
library(igraph)
library(sf)
library(shp2graph)

get_next_hops <- function(graph=NULL){
  next_hops <- list()
  dist <- list()
  paths <- list()
  for (node in V(graph)){
    dist[[node]] <- list()
    next_hops[[node]] <- list()
    paths[[node]] <- list()
    for (nei in V(graph)){
      if (node == nei){
        dist[[node]][[nei]] <- 0
        next_hops[[node]][[nei]] <- nei
      } else if (graph[node, nei] == 0){
        dist[[node]][[nei]] <- Inf
        next_hops[[node]][[nei]] <- 0
      } else {
        dist[[node]][[nei]] <- graph[node, nei]
        next_hops[[node]][[nei]] <- nei
      }
    }
  }
  for (r in V(graph)){
    for (i in V(graph)){
      for (j in V(graph)){
        if (dist[[i]][[j]] > dist[[i]][[r]] + dist[[r]][[j]]){
          dist[[i]][[j]] <- dist[[i]][[r]] + dist[[r]][[j]]
          next_hops[[i]][[j]] <- next_hops[[i]][[r]]
        }
      }
    }
  }
  for (i in V(graph)){
    for (j in V(graph)){
      paths[[i]][[j]] <- get_path_from_hops(graph = graph, next_hops = next_hops, start_node = i, end_node = j)
    }
  }
  return(paths)
}


get_path_from_hops <- function(graph=NULL, next_hops=NULL, start_node, end_node){
  if (next_hops[[start_node]][[end_node]] == 0){
    return(list())
  }
  path <- list()
  path <- append(path, list(start_node))
  u <- start_node
  v <- end_node
  while (u != v){
    u <- next_hops[[u]][[v]]
    path <- append(path, list(u))
  }
  return(path)
}
