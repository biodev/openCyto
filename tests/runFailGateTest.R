detach("package:openCyto")
install.packages("~/openCyto_1.9.3_R_x86_64-pc-linux-gnu.tar.gz",repos = NULL)

library(openCyto)

panel1Files <- list.files("~/data/relabeled/panel1/",full.names = TRUE)
panel1Fs <- list.files("~/data/relabeled/panel1/")
#get our test data
testData <- lapply(panel1Files, function(x){read.FCS(x)})
names(testData) <- panel1Fs
testData <- as(testData, "flowSet")
source(paste0(baseDir,"/scripts/QC.R"))
testData <- transformFS(testData)


#set min options
options(openCyto = list(gating=list(minEvents=100)))

#load pipeline File
baseDir <- "~/openCytoAML/"
source(paste0(baseDir,"/scripts/initOpenCyto.R"))


pipeFile <- paste0(baseDir,"/pipelines/testPipeline.csv")

#pipeFile <- paste0(baseDir,"/pipelines/panel1-pipeline1-expanded.csv")
#pipeFile <- paste0(baseDir,"/pipelines/panel1-pipeline1-initial.csv")

gt <- gatingTemplate(pipeFile)

gs <- GatingSet(testData)
gating(gt, gs, mc.cores=10, parallel_type="multicore")
getPopStats(gs)

