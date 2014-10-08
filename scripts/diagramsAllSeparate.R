#!/usr/bin/Rscript
# Args: filename, name
RESULTS_PATH_MALE = "../experimentalResults/uzyte/resMale/"
RESULTS_PATH_DUZE = "../experimentalResults/uzyte/resSaturn8/"
myPaste <- function(a,b) {
  paste(a,b,sep="")
}
filenames <- list(
               list(myPaste(RESULTS_PATH_MALE, "polbooks.res"), "Polbooks network, Independent Cascade Model"),
               list(myPaste(RESULTS_PATH_MALE, "polbooks_lt.res"), "Polbooks network, Linear Threshold Model"),
               list(myPaste(RESULTS_PATH_DUZE, "hep-th.res"), "Hep-th netwrok, Independent Cascade Model"),
               list(myPaste(RESULTS_PATH_DUZE, "hep-th_lt.res"), "Hep-th network, Linear Threshold Model")
               )
filenamesLog <- list(
               list(myPaste(RESULTS_PATH_MALE, "dolphins_times.res"), "Dolphins network, running time"),
               list(myPaste(RESULTS_PATH_DUZE, "amazon_times.res"), "Amazon network, running time")
              )
dolphinesT = filenamesLog[[1]]
amazonT = filenamesLog[[2]]

cc <-function(a, ...) {
  paste(a, ..., sep="")
}

source("plotDiagram.R")
#par(mfrow=c(3,2))
for (file in filenames) {
  print(paste("Generating plot...", file[[1]]))
  pdf(cc(file, ".pdf"), width=8, height=8)
  data <- read.table(file[[1]])
  plotDiagram(file[[2]], data)
  dev.off()
}

print(paste("Generating plot...", dolphinesT[[1]]))
pdf(cc(dolphinesT[[1]], ".pdf"), width=8, height=8)
data <- read.table(dolphinesT[[1]])
plotDiagram(dolphinesT[[2]], data, T, 20)

print(paste("Generating plot...", amazonT[[1]]))
pdf(cc(amazonT[[1]], ".pdf"), width=8, height=8)
data <- read.table(amazonT[[1]])
plotDiagram(amazonT[[2]], data, T)





