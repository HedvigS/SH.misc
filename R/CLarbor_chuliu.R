#' Find the minimum spanning arborescence of a weighted graph, given a root node
#'
#' Applies the Chu-Liu/Edmonds algorithm, as described in Georgiadis (2003), to
#' find the minimum spanning arborescence of a weighted graph, given a root
#' node.
#'
#' This seems to work more reliably than the `getMinimumArborescence()` function
#' from the `optrees` package, which anyway is no longer available from CRAN.
#'
#' @param G An `igraph` weighted graph.
#' @param root An integer giving the index of the root node.
#'
#' @returns An `igraph` tree giving the minimum spanning arborescence of `G`
#'   with the root at index `root`.
#' @references Georgiadis, L. (2003). Arborescence optimization problems
#'   solvable by Edmonds’ algorithm. Theoretical Computer Science, 301(1-3),
#'   427-437.
#' @import igraph
#' @export
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

# The following functions are copied from the latest version of the `optrees`
# package, which is no longer available in CRAN.

searchZeroCycle <- function(nodes, arcs) {
  
  # Previous
  arcs <- cbind(arcs, 0)  # add column to mark arcs checked
  uncheck.nodes <- nodes  # vector with uncheked nodes
  stack.nodes <- c()  # vector to store nodes of the actual walk
  cycle.nodes <- c()  # vector to store nodes of the cycle
  
  # Initialize
  arc <- arcs[1,]  # start with first arc
  arcs[1, 4] <- 1  # mark arc as checked
  
  # Start with first node of the arc, remove from unchecked and add to walk
  actual.node <- arc[1]
  uncheck.nodes <- uncheck.nodes[-which(uncheck.nodes == actual.node)]
  stack.nodes <- c(stack.nodes, actual.node)
  # Continue with second node of the arc, remove from unchecked and add to walk
  actual.node <- arc[2]
  uncheck.nodes <- uncheck.nodes[-which(uncheck.nodes == actual.node)]
  stack.nodes <- c(stack.nodes, actual.node)
  
  # Iterate until found a cycle and there are unchecked arcs
  while (length(cycle.nodes) == 0 && any(arcs[, 4] != 1)) {
    
    # Check uncheked arcs leaving actual node
    i <- which(arcs[, 1] == actual.node & arcs[, 4] == 0)
    leaving.arcs <- matrix(arcs[i, ], ncol = 4)  # arcs leaving actual node
    # WARNING! The arc may have been checked but reach a valid node
    
    if (nrow(leaving.arcs) == 0) {
      # If no arcs leaving go back in stack
      stack.nodes <- stack.nodes[-length(stack.nodes)]  # remove node
      
      # Review stack
      if (length(stack.nodes) == 0) {
        # If stack is empty find another uncheked arc
        uncheck.arcs <- arcs[which(arcs[, 4] == 0),]
        arc <- matrix(uncheck.arcs[1, ], ncol = 4)  # get first unchecked arc
        arcs[which(arcs[, 1] == arc[1] & arcs[, 2] == arc[2]), 4] <- 1  # mark
        
        # Get first node of the arc, remove from unchecked and add to walk
        actual.node <- arc[1]
        uncheck.nodes <- uncheck.nodes[-which(uncheck.nodes == actual.node)]
        stack.nodes <- c(stack.nodes, actual.node)
        # Get second node of the arc, remove from unchecked and add to walk
        actual.node <- arc[2]
        uncheck.nodes <- uncheck.nodes[-which(uncheck.nodes == actual.node)]
        stack.nodes <- c(stack.nodes, actual.node)
        
      } else {
        # There are nodes in stack so check precedent
        actual.node <- stack.nodes[length(stack.nodes)]
      }
      
    } else {
      # If arcs leaving get one and check cycle
      arc <- matrix(leaving.arcs[1, ], ncol = 4)  # get first arc
      arcs[which(arcs[, 1] == arc[1] & arcs[, 2] == arc[2]), 4] <- 1  # mark
      
      # Check cycle
      if (arc[2] %in% stack.nodes) {
        # If reaching node is in stack there is a cycle
        from <- which(stack.nodes == arc[2])  # stack index where cycle begins
        to <- which(stack.nodes == arc[1])  # stack index where cycle ends
        # Save cycle
        cycle.nodes <- c(cycle.nodes, stack.nodes[from:to], stack.nodes[from])
        actual.node <- arc[1]; actual.node  # keep actual node
        
      } else {  
        # If reaching node isn't in stack no cycle
        actual.node <- arc[2]  # reaching node is the new node to check
        # Remove node from unchecked and add to walk
        uncheck.nodes <- uncheck.nodes[-which(uncheck.nodes == actual.node)]
        stack.nodes <- c(stack.nodes, actual.node)
        
      }
    }
  }
  
  # Final check
  if (length(cycle.nodes) == 0) {
    # If cycle.nodes is empty there is no cycle
    print("No cycle!")
    cycle <- FALSE
    return(cycle)
    
  } else {
    # There is a cycle
    cycle <- TRUE
    cycle.arcs <- matrix(ncol = 4)[-1, ]  # matrix to store arcs of the cycle
    for (i in 1:(length(cycle.nodes)-1)) {
      # Check all nodes in the cycle to add arcs
      i <- which(arcs[, 1] == cycle.nodes[i] & arcs[,2] == cycle.nodes[i+1])
      cycle.arcs <- rbind(cycle.arcs, arcs[i, ])
    }
    
    cycle.arcs <- cycle.arcs[, -4]  # remove last column
    
    return(list("cycle" = cycle, "cycle.nodes" = cycle.nodes,
                "cycle.arcs" = cycle.arcs))
    
  }
}

compactCycle <- function(nodes, arcs, cycle) {
  
  # Previous
  Cmat <- ArcList2Cmat(nodes, arcs)  # work with cost matrix
  
  # Initialize
  super.node <- min(cycle)  # minimun node of the cycle as super.node
  iCosts <- Cmat[super.node, -cycle[2:(length(cycle)-1)]]  # new node file
  jCosts <- Cmat[-cycle[2:(length(cycle)-1)], super.node]  # new node column
  
  # Matches between nodes
  match.nodes <- matrix(nodes, nrow = length(nodes), ncol = 2)
  # First column previous list of nodes, second column new list of nodes
  match.nodes[match(cycle[-length(cycle)], match.nodes[,1]), 2] <- super.node
  # Update nodes with larger label than super.node
  new.nodes <- which(match.nodes[, 2] > super.node)
  match.nodes[new.nodes, 2] <- c((super.node+1):(super.node+length(new.nodes)))
  
  # Fill new file with minimun arcs from removed nodes
  # Costs from leaving arcs
  cost.arcs <- Cmat[cycle[1:(length(cycle)-1)], -cycle[2:(length(cycle)-1)]]
  # Add to fileCost the minimum cost arcs
  for (j in 1:length(iCosts)) {
    # Iterate by columns
    iCosts[j] <- min(cost.arcs[, j])  # save minimum cost arc
  }
  
  # Fill new column with minimum arcs from removed nodes
  # Costs from reaching arcs
  cost.arcs <- Cmat[-cycle[2:(length(cycle)-1)], cycle[1:(length(cycle)-1)]]
  # Add to colCost the minimum cost arcs
  for (i in 1:length(jCosts)) {
    # Iterate by files
    jCosts[i] <- min(cost.arcs[i, ])  # save minimum cost arc
  }
  
  # New vector of nodes
  nodes <- c(1:(length(nodes) - (length(cycle) - 2)))
  # Compact nodes of the cycle in super.node
  Cmat <- Cmat[-cycle[2:(length(cycle)-1)],-cycle[2:(length(cycle)-1)]]
  # Add costs calculated before
  Cmat[super.node, ] <- iCosts
  Cmat[, super.node] <- jCosts
  
  # Return list of arcs
  arcs <- Cmat2ArcList(nodes, Cmat, directed = TRUE)
  
  return(list("nodes"= nodes, "arcs" = arcs, "super.node" = super.node,
              "matches" = match.nodes))
}

ArcList2Cmat <- function(nodes, arcs, directed = TRUE) {
  
  # Initialize
  n <- length(nodes)  # number of nodes
  Cmat <- matrix(nrow = n, ncol = n)  # building cost matrix
  Cmat[, ] <- Inf  # start with all Inf
  
  for (i in 1:nrow(Cmat)) {
    Cmat[i, i] <- NA  # NA loops
  }
  
  for (i in 1:nrow(arcs)) {
    Cmat[arcs[i, 1], arcs[i, 2]] <- arcs[i, 3]  # get cost from list of arcs
  }
  
  if (!directed) {
    # If the arcs of the graph are directed, duplicate arcs
    for (i in 1:nrow(arcs)) {
      Cmat[arcs[i, 2], arcs[i, 1]] <- arcs[i, 3]  # get cost from list of arcs
    }
  }
  
  # Return the cost matrix of the graph
  return(Cmat)
  
}

Cmat2ArcList <- function(nodes, Cmat, directed = TRUE) {
  
  # Initialize
  arcs <- c()  # list of arcs
  
  if (directed) {
    # If the arcs of the graph are directed
    for (i in seq(along = nodes)) {  # check every row
      for (j in seq(along = nodes)) {  # and every column
        if (!is.na(Cmat[i, j]) && Cmat[i, j] != Inf) {
          # Save the arc if exists (no NA nor Inf)
          arcs <- rbind(arcs, c(i, j, Cmat[i, j]))
        }
      }
    }
    
    # Column names if directed graph
    colnames(arcs) <- c("head", "tail", "weight")
    
  } else {
    # If the arcs of the graph are not directed
    for (i in seq(along = nodes)) {  # check every row
      for (j in i:length(nodes)) {  # and every column starting from node i
        if (!is.na(Cmat[i, j]) && Cmat[i, j] != Inf) {
          # Save the arc if exists (no NA nor Inf)
          arcs <- rbind(arcs, c(i, j, Cmat[i, j]))
        }
      }
    }
    
    # Column names if undirected graph
    colnames(arcs) <- c("ept1", "ept2", "weight")
  }
  
  # Return matrix with list of arcs
  return(arcs)
  
}