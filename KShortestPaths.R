library(igraph)

k_best_paths <- function(graph=NULL, start_node = NULL, end_node=NULL, k=3){
  paths = list()
  count = list()
  temp_paths = list()
  for (node in V(graph)){
    count = append(count, list(str(node)=0))
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
        max_length <- path[[2]]
      } else{
        temp_temp_paths <- append(temp_temp_paths, list(list(path[[1]], path[[2]])))
      }
    }
    temp_paths <- temp_temp_paths
    u = current_path[[length(current_path)]]
    count[[str(u)]] <- count[[str(u)]] + 1
    if (u == end_node){
      paths <- append(paths, list(current_path))
    } else if (count[[str(u)]] <= k){
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

