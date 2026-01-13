chuliu <- function (G, root){
  Gh <- G
  Gh[,root] <- NA
  nGh <- 1 - Gh
  diag(nGh) <- 0
  #minrows <- apply(t(nGh), 1, function(x){which.max(x)[1]})
  minrows <- max.col(t(nGh))
  Th <- matrix(0,nrow(Gh),ncol(Gh))
  for (i in 1:ncol(Th)){
    Th[minrows[i],i] <- Gh[minrows[i],i]
  }
  Gh[is.na(Gh)] <- 0
  Ghg <- graph_from_adjacency_matrix(Gh,weighted = T,diag = F)
  Thg <- graph_from_adjacency_matrix(Th,weighted = T,diag = F)
  stages <- list(list(nodes = 1:vcount(Ghg), arcs = cbind(get.edgelist(Ghg),E(Ghg)$weight), super.node = NULL, matches = cbind(1:vcount(Ghg),1:vcount(Ghg))))
  trees <- list(Thg)
  while (!is.dag(Thg)){
    message(paste("Stage",length(stages)))
    Thgzc <- searchZeroCycle(1:vcount(Thg),cbind(get.edgelist(Thg),E(Thg)$weight))
    cc <- compactCycle(1:vcount(Ghg),cbind(get.edgelist(Ghg),E(Ghg)$weight),Thgzc$cycle.nodes)
    stages[[length(stages) + 1]] <- cc
    Ghg <- graph_from_edgelist(cc$arcs[,1:2, drop = F])
    Ghg <- set.edge.attribute(Ghg, "weight", value = cc$arcs[,3, drop = F])
    Thg <- incoming(Ghg)
    trees[[length(trees) + 1]] <- Thg
  }
  thisstage <- stages[[length(stages)]]
  thistree <- trees[[length(trees)]]
  thisadj <- matrix(0,length(thisstage$nodes),length(thisstage$nodes))
  thisadj[get.edgelist(thistree)] <- E(thistree)$weight
  while (length(stages) > 1){
    message(paste("Stage",length(stages) - 1))
    laststage <- stages[[length(stages) - 1]]
    lasttree <- trees[[length(trees) - 1]]
    lastadj <- matrix(0,length(laststage$nodes),length(laststage$nodes))
    lastadj[get.edgelist(lasttree)] <- E(lasttree)$weight
    lastgadj <- matrix(0,length(laststage$nodes),length(laststage$nodes))
    lastgadj[laststage$arcs[,1:2, drop = F]] <- laststage$arcs[,3, drop = F]
    newadj <- matrix(0,length(laststage$nodes),length(laststage$nodes))
    incycle <- thisstage$matches[thisstage$matches[,2, drop = F] == thisstage$super.node,1]
    receptor <- thisstage$matches[thisstage$matches[,1, drop = F] %in% incycle,2][1]
    for (i in 1:nrow(newadj)){
      for (j in 1:ncol(newadj)){
        if (i %in% incycle & j %in% incycle){
          if (j != receptor){
            newadj[i,j] <- lastadj[i,j]
          }
        } else if (((i %in% incycle) == F) & ((j %in% incycle) == F)) {
          newadj[i,j] <- thisadj[thisstage$matches[i,2],thisstage$matches[j,2]]
        } else if (((i %in% incycle) == F) & ((j %in% incycle) == T)) {
          if (j == receptor){
            newadj[i,j] <- thisadj[thisstage$matches[i,2],thisstage$matches[j,2]]
          }
        } else if (((i %in% incycle) == T) & ((j %in% incycle) == F) & (thisadj[thisstage$matches[i,2],thisstage$matches[j,2]] > 0)) {
          if (lastgadj[i,j] == min(lastgadj[incycle,j])) {
            newadj[i,j] <- lastgadj[i,j]
          }
        }
      }
    }
    thisadj <- newadj
    thisstage <- laststage
    thistree <- graph_from_adjacency_matrix(newadj,weighted = T,diag = F)
    stages[[length(stages)]] <- NULL
    trees[[length(trees)]] <- NULL
  }
  # plot(thistree,vertex.label.cex = 1, vertex.size = 0, edge.arrow.size = 0.5)
  thistree
}

incoming <- function(g){
  gadj <- as.matrix(get.adjacency(g,attr = "weight"))
  diag(gadj) <- 1
  minrows <- max.col(t(1-gadj))
  tadj <- matrix(0,nrow(gadj),ncol(gadj))
  for (i in 1:ncol(tadj)){
    tadj[minrows[i],i] <- gadj[minrows[i],i]
  }
  Th <- graph_from_adjacency_matrix(tadj,weighted = T,diag = F)
  Th
}