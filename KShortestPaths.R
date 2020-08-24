library(igraph)
library(ggplot2)
library(rgdal)
library(igraph)
library(sf)
library(shp2graph)

k_best_paths <- function(graph=NULL, start_node = NULL, end_node=NULL, k=3, count_stations=FALSE, check_junctions=FALSE){
  if (count_stations){
    return(k_smallest_paths(graph, start_node, end_node, k, check_junctions))
  }
  paths = list()
  count = list()
  temp_paths = list()
  junctions = list()
  for (node in V(graph)){
    count[[node]] = 0
    if (length(neighbors(graph, node, mode = "all")) > 2){
      junctions <- append(junctions, list(node))
    }
  }
  temp_paths = list(list(c(start_node), 0))
  while (length(temp_paths) > 0 && count[[end_node]] < k){
    min_length = Inf
    current_path = NULL
    temp_temp_paths = list()
    for (path in temp_paths){
      if (path[[2]] < min_length){ 
        if (min_length < Inf){
          temp_temp_paths <- append(temp_temp_paths, list(list(current_path, min_length)))
        }
        current_path <- path[[1]]
        min_length <- path[[2]]
      } else{
        temp_temp_paths <- append(temp_temp_paths, list(list(path[[1]], path[[2]])))
      }
    }
    temp_paths <- temp_temp_paths
    u = current_path[[length(current_path)]]
    count[[u]] <- count[[u]] + 1
    if (u == end_node){
      paths <- append(paths, list(current_path))
    } else if (count[[u]] <= k){
      for (node in neighbors(graph, u)){
        if (node %in% current_path){
          next
        }
        temp_p <- append(current_path, node)
        temp_paths <- append(temp_paths, list(list(temp_p, min_length + graph[u, node])))
      }
    }
  }
  
  return(paths)
}


k_smallest_paths <- function(graph, start_node, end_node, k, check_junctions){
  paths = list()
  temp_paths = list()
  junctions = list()
  for (node in V(graph)){
    if (length(neighbors(graph, node, mode = "all")) > 2){
      junctions <- append(junctions, list(node))
    }
  }
  temp_paths = list(list(c(start_node), 0, 0))
  while (length(temp_paths) > 0 && length(paths) < k){
    temp_temp_paths = list()
    for (path in temp_paths){
      last_node <- path[[1]][[length(path[[1]])]]
      for (nei in neighbors(graph, last_node)){
        if (nei %in% path[[1]]){
          next
        }
        new_path <- list(c(path[[1]], nei), path[[2]] + graph[last_node, nei], path[[3]] + 1)
        temp_temp_paths <- append(temp_temp_paths, list(new_path))
      }
    }
    temp_paths <- temp_temp_paths
    temp_temp_paths <- list()
    temp_indices <- c()
    for (path in temp_paths){
      last_node <- path[[1]][[length(path[[1]])]]
      if (last_node == end_node){
        temp_temp_paths <- append(temp_temp_paths, list(path))
        temp_indices <- c(temp_indices, path[[2]])
      }
    }
    if (length(temp_indices) < 1){
      next
    }
    temp_temp_paths <- temp_temp_paths[order(temp_indices)]
    if (length(temp_temp_paths) + length(paths) > k){
      temp_len <- length(paths)
      for (i in 1:k - temp_len){
        paths <- append(paths, list(temp_temp_paths[[i]][[1]]))
      }
    } else {
      for (path in temp_temp_paths){
        paths <- append(paths, list(path[[1]]))
      }
    }
  }
  return(paths)  
}

adjm <- matrix(sample(0:5, 400, replace=TRUE,
                      prob=c(0.8,0.08,0.03,0.03,0.03,0.03)), nc=20)
g2 <- graph_from_adjacency_matrix(adjm, weighted=TRUE)
answer <- k_best_paths(g2, start_node = 1, end_node = 9, k = 5)
print(answer)
