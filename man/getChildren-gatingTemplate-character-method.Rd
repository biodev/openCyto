% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gatingTemplate-methods.R
\docType{methods}
\name{getChildren,gatingTemplate,character-method}
\alias{getChildren,gatingTemplate,character-method}
\title{get children nodes}
\usage{
\S4method{getChildren}{gatingTemplate,character}(obj, y)
}
\arguments{
\item{obj}{\code{gatingTemplate}}

\item{y}{\code{character} parent node path}
}
\description{
get children nodes
}
\examples{
\dontrun{
gt <- gatingTemplate(system.file("extdata/gating_template/tcell.csv",package = "openCyto"))

getNodes(gt, "/nonDebris")
getChildren(gt, "/nonDebris") 
}
}
