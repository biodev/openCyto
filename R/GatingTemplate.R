is.string <- function(x) {
  return( is.character(x) && length(x) == 1 )
}

## Methods for Generating a Gating Template
GatingTemplate <- function() {
  g <- graphNEL(nodes = "root", edgemode = "directed")
  g <- as(g, "gatingTemplate")
  nodeDataDefaults(g, "pop") <- ""
  edgeDataDefaults(g, "gtMethod") <- ""
  edgeDataDefaults(g, "ppMethod") <- ""
  edgeDataDefaults(g, "isReference") <- FALSE
  
  # add default root
  nodeData(g, "root", "pop") <- new("gtPopulation", id = "root", name = "root", alias = "root")
  
  return(g)
}

node.path <- function(gt, parent, node) {
  
  ## Escape quickly if parent == 'root'
  if (parent == "root") {
    return( paste0("/", node) )
  }
  
  ## Make 'parent' act more like a regular expression if it isn't one already
  parent_regex <- parent
  n <- nchar(parent_regex)
  if (!substring(parent_regex, 1, 1) == "/") parent_regex <- paste0("/", parent_regex)
  if (!substring(parent_regex, n, n) == "$") parent_regex <- paste0(parent_regex, "$")
  parent_regex <- gsub("(?<!\\\\)\\+", "\\\\+", parent_regex, perl=TRUE)
  
  ## Try to match 'parent_regex' to the available nodes
  path <- grep( parent_regex, nodes(gt), perl=TRUE, value=TRUE )
  
  
  if (length(path) == 0) {
    stop("Couldn't match parent path '", parent, "' to any node in the GatingTemplate!")
  }
  
  if (length(path) > 1) {
    stop("Matched parent path to multiple paths:\n",
      paste("  ", path, collapse="\n")
    )
  }
  
  path <- sub("^root", "", path)
  
  return( paste(path, node, sep="/") )
}

## Add a gate to a gatingTemplate
addGate <- function(gt, parent, node, gtMethod, gtPopulation=NULL, ppMethod=NULL) {
  
  newNodePath <- node.path(gt, parent, node)
  
  gt <- addNode(newNodePath, gt)
  
  ## Add an edge between the parent and the new node
  gt <- addEdge(parent, newNodePath, gt)
  
  if (is.list(gtMethod)) gtMethod <- unlist(gtMethod)
  
  ## Be nice: allow user to pass a named string as a gtMethod
  ## Done in the case of 'simple' arguments
  if (!is.null(names(gtMethod)) && is.character(gtMethod)) {
    gtMethod <- new("gtMethod", 
      name=names(gtMethod), 
      dims=unname(gtMethod),
      args=list(),
      collapse=FALSE,
      groupBy=""
    )
  }
  
  ## Add the gating method
  if (!inherits(gtMethod, "gtMethod")) {
    stop("The 'gtMethod' object passed must be of class 'gtMethod'. See ",
      "?\"gtMethod-class\" for more details.")
  }
  
  edgeData(gt, parent, newNodePath, "gtMethod") <- gtMethod
  edgeData(gt, parent, newNodePath, "isReference") <- FALSE
  
  ## Add the preprocessing method if available
  if (!is.null(ppMethod)) {
    edgeData(gt, parent, newNodePath, "ppMethod") <- ppMethod
  }
  
  ## Add the population information
  if (is.null(gtPopulation)) {
    gtPopulation <- new("gtPopulation",
      id=newNodePath,
      name=node,
      alias=node
    )
  }
  
  if (is.string(gtPopulation)) {
    gtPopulation <- new("gtPopulation",
      id=newNodePath,
      name=node,
      alias=gtPopulation
    )
  }
  
  nodeData(gt, newNodePath, "pop") <- gtPopulation
  return(gt)
}

.addQuadGate <- function(gt, parent, x) {
  
  gtMethod <- x$gtMethod
  gtPopulation <- x$gtPopulation
  ppMethod <- x$ppMethod
  
  ## Node should be the name of the channel, with either a '+' or a '-'
  ## appended
  if (is.string(gtPopulation)) {
    nodePlus <- paste0(gtPopulation, "+")
    nodeMinus <- paste0(gtPopulation, "-")
  } else {
    nodePlus <- paste0(gtPopulation@name, "+")
    nodeMinus <- paste0(gtPopulation@name, "-")
  }
  
  ## Make sure the 'positive' argument gets properly get in gtMethod
  ## this means we have to construct it explicitly
  if (is.string(gtMethod)) {
    gtMethod <- new("gtMethod", name=gtMethod)
  }
  
  gtMethodPlus <- gtMethodMinus <- gtMethod
  gtMethodPlus@dims <- nodePlus
  gtMethodMinus@dims <- nodeMinus
  
  ## Add gates
  gt <- addGate(gt, parent, nodePlus, gtMethodPlus, gtPopulation, ppMethod)
  gt <- addGate(gt, parent, nodeMinus, getMethodMinus, gtPopulation, ppMethod)
  
}

## Add a quad gate
## 'x' should be a list containing a gtMethod, gtPopulation, and ppMethod
## similarily for 'y'
addQuadGate <- function(gt, parent, x, y) {
  
  gt <- .addQuadGate(gt, parent, x)
  gt <- .addQuadGate(gt, parent, y)
  
}