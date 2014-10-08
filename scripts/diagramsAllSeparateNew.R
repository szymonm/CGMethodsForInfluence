#!/usr/bin/Rscript
# Args: filename, name
RESULTS_PATH_MALE = "../experimentalResults/uzyte/resMale/"
RESULTS_PATH_DUZE = "../experimentalResults/uzyte/resSaturn8/"
myPaste <- function(a,b) {
  paste(a,b,sep="")
}

polbooks = list(myPaste(RESULTS_PATH_MALE, "polbooks.res"), 
                "Polbooks network, Independent Cascade Model")
dolphins = list(myPaste(RESULTS_PATH_MALE, "dolphins.res"),
                "Dolphins network, Independent Cascade Model")
polbooks.lt = list(myPaste(RESULTS_PATH_MALE, "polbooks_lt.res"), 
                   "Polbooks network, Linear Threshold Model")
hepth = list(myPaste(RESULTS_PATH_DUZE, "hep-th.res"), 
             "Hep-th citation netwrok, Independent Cascade Model")
hepth.lt = list(myPaste(RESULTS_PATH_DUZE, "hep-th_lt.res"), 
                "Hep-th citation network, Linear Threshold Model")

dolphinesT = list(myPaste(RESULTS_PATH_MALE, "dolphins_times.res"), 
                  "Dolphins network, running time")
amazonT = list(myPaste(RESULTS_PATH_DUZE, "amazon_times.res"), 
               "Amazon network, running time")

cc <-function(a, ...) {
  paste(a, ..., sep="")
}

source("plotDiagram.R")

algLabelsMale = c("Greedy LDAG", expression(bold("LDAG-BF")), expression(bold("LDAG-SV")),
                  "CELF++","Fringe Game",   
                  expression(bold("SVD")), 
                  "DDH", "RANDOM")
rowOrderMale = c(1,2,3,4,7,5,9,10,8)
print(paste("Generating plot...", polbooks[[1]]))
pdf(cc(polbooks, ".pdf"), width=8, height=8)
data <- read.table(polbooks[[1]])
plotDiagram(polbooks[[2]], data, algLabelsMale, rowOrderMale)
x = dev.off()

print(paste("Generating plot...", dolphins[[1]]))
pdf(cc(dolphins, ".pdf"), width=8, height=8)
data <- read.table(dolphins[[1]])
plotDiagram(dolphins[[2]], data, algLabelsMale, rowOrderMale)
x = dev.off()

print(paste("Generating plot...", polbooks.lt[[1]]))
pdf(cc(polbooks.lt, ".pdf"), width=8, height=8)
data <- read.table(polbooks.lt[[1]])
plotDiagram(polbooks.lt[[2]], data, algLabelsMale, rowOrderMale)
x = dev.off()

print(paste("Generating plot...", hepth[[1]]))
pdf(cc(hepth, ".pdf"), width=8, height=8)
data <- read.table(hepth[[1]])
algLabelsHep = algLabelsMale[c(1,2,3,5,6,7,8)]
orderHep =  c(1,2,3,4,6,7,5)
plotDiagram(hepth[[2]], data, algLabelsHep, orderHep)
x = dev.off()

print(paste("Generating plot...", hepth.lt[[1]]))
pdf(cc(hepth.lt, ".pdf"), width=8, height=8)
data <- read.table(hepth.lt[[1]])
plotDiagram(hepth.lt[[2]], data, algLabelsHep, orderHep)
x = dev.off()

print(paste("Generating plot...", dolphinesT[[1]]))
pdf(cc(dolphinesT[[1]], ".pdf"), width=8, height=8)
data <- read.table(dolphinesT[[1]])
plotDiagram(dolphinesT[[2]], data, algLabelsMale, rowOrderMale,T, 20)
x = dev.off()

print(paste("Generating plot...", amazonT[[1]]))
pdf(cc(amazonT[[1]], ".pdf"), width=8, height=8)
data <- read.table(amazonT[[1]])
algLabelsAmz = algLabelsMale[c(1,5,6,7,8)]
orderAmz = c(1,2,3,5,6,4)
plotDiagram(amazonT[[2]], data, algLabelsAmz, orderAmz, T)
x = dev.off()





