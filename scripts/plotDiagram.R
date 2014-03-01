plotDiagram <- function(name, table, log=F, legPosY=0) {
  if (legPosY == 0) {
    legPosY = max(table[,-1])
  }
  rows <- dim(table)[1]
  if (log) {
    f <- function(x) {log(x)}
    plot(as.numeric(table[1,-1]), f(as.numeric(table[2, -1])),
         main=name, type="l", lty=2, col=2, 
         xlab="initial seed size", ylab="running time (log scale)",
         xlim=c(min(table[1,-1]), max(table[1, -1])),
         ylim=f(c(0.2, max(table[, -1]))))
    for (i in 3:rows) {
      lines(as.numeric(table[1,-1]), f(as.numeric(table[i,-1])), lty=i, col=i)
    }
    legend(data[1,2], f(legPosY), data[-1,1], lty=2:rows, col=2:rows,
           cex=0.8)
  } else {
    plot(as.numeric(table[1,-1]), as.numeric(table[2, -1]), 
         main=name, type="l", lty=2, col=2, 
         xlab="initial seed size", ylab="influence on network",
         ylim=c(0, max(table[, -1])))
    for (i in 3:rows) {
      lines(as.numeric(table[1,-1]), as.numeric(table[i,-1]), lty=i, col=i)
    }
    legend(data[1,2] + 1, max(table[, -1]), data[-1,1], lty=2:rows, col=2:rows,
           cex=0.8)
  }
}
