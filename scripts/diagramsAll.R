# Args: filename, name
RESULTS_PATH_MALE = "../resMale/"
RESULTS_PATH_DUZE = "../resSaturn8/"
myPaste <- function(a,b) {
  paste(a,b,sep="")
}
filenames <- list(
               list(myPaste(RESULTS_PATH_MALE, "polbooks.res"), "Polbooks IC"),
               list(myPaste(RESULTS_PATH_MALE, "polbooks_lt.res"), "Polbooks LT"),
               list(myPaste(RESULTS_PATH_DUZE, "hep-th.res"), "Hep-th IC"),
               list(myPaste(RESULTS_PATH_DUZE, "hep-th_lt.res"), "Hep-th LT")
               )
filenamesLog <- list(
               list(myPaste(RESULTS_PATH_MALE, "dolphins_times.res"), "Dolphins running time"),
               list(myPaste(RESULTS_PATH_DUZE, "amazon_times.res"), "Amazon running time")
              )
dolphinesT = filenamesLog[[1]]
amazonT = filenamesLog[[2]]


source("plotDiagram.R")
pdf("../allPlots.pdf", onefile=TRUE, width=8, height=12)
par(mfrow=c(3,2))
for (file in filenames) {
  data <- read.table(file[[1]])
  plotDiagram(file[[2]], data)
}

data <- read.table(dolphinesT[[1]])
plotDiagram(dolphinesT[[2]], data, T, 20)

data <- read.table(amazonT[[1]])
plotDiagram(amazonT[[2]], data, T)
dev.off()





