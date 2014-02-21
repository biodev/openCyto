gt <- GatingTemplate()
plot(gt)
gtMethod <- new("gtMethod",
  name="mindensity",
  dims="Live",
  args=list(),
  collapse=FALSE,
  groupBy=""
)
gt <- addGate(gt, "root", "Live+", c("Live"="mindensity"))
plot(gt)

gt <- gatingTemplate(system.file("extdata/template_tcell.csv",package = "openCyto"))
