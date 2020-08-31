library(igraph)
library(ggplot2)
library(rgdal)
library(igraph)
library(sf)
library(shp2graph)

get_next_hops <- function(graph=NULL){
  next_hops <- list()
  dist <- list()
  for (node in V(graph)){
    dist[[node]] = list()
    next_hops[[node]] = list()
    for (nei in V(graph)){
      if (node == nei){
        dist[[node]][[nei]] <- 0
        next_hops[[node]][[nei]] <- nei
      } else if (graph[node, nei] == 0){
        dist[[node]][[nei]] <- Inf
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
 return(next_hops)
}


get_path_from_hops <- function(graph=NULL, next_hops=NULL, start_node, end_node){
  if (is.null(next_hops[[start_node]][[end_node]])){
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