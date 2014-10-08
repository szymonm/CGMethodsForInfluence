# Args: filename, name
args <- commandArgs(trailingOnly=T)
filename <- args[1]
print(paste("Reading from:", filename))

data <- read.table(args[1])

source("plotDiagram.R")

pdf(paste(filename,".pdf", sep=""))
plotDiagram(args[2], data)
dev.off()

