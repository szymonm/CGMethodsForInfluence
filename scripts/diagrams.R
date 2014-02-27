# Args: filename, name
args <- commandArgs(trailingOnly=T)
filename <- args[1]
print(paste("Reading from:", filename))

data <- read.table(args[1])

plotDiagram <- function(name, table) {
  png(paste(filename,".png", sep=""))
  rows <- dim(table)[1]
  plot(as.numeric(table[1,-1]), as.numeric(table[2, -1]), 
       main=name, type="l", lty=2, col=2, 
       xlab="initial seed size", ylab="influence on network")
  for (i in 3:rows) {
    lines(as.numeric(table[1,-1]), as.numeric(table[i,-1]), lty=i, col=i)
  }
  legend(data[1,2] + 1, max(table[2, -1]), data[-1,1], lty=2:rows, col=2:rows,
         cex=0.8)
  dev.off()
}

plotDiagram(args[2], data)

