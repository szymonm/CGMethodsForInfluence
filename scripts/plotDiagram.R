plotDiagram <- function(name, table, algLabels, rowOrder, log=F, legPosY=0) {
  if (legPosY == 0) {
    legPosY = max(table[,-1])
  }
  pointTypes = 0:10
  colorUsed = c("yellow", "red", "green", "blue", "darkcyan", "darkmagenta", "goldenrod", "black",
                "grey40", "darkgreen")
  rows <- dim(table)[1]
  if (log) {
    table = table[rowOrder, ]
    print(table)
    f <- function(x) {log(x + 1.001)}
    plot(as.numeric(table[1,-1]), f(as.numeric(table[2, -1])),
         main=name, type="o", lty=2, col=colorUsed[2], 
         xlab="initial seed size", ylab="running time (log scale)",
         xlim=c(min(table[1,-1]), max(table[1, -1])), pch=pointTypes[2],
         ylim=f(c(0.2, max(table[, -1]))))
    for (i in 3:rows) {
      lines(as.numeric(table[1,-1]), f(as.numeric(table[i,-1])), type="o", 
            pch=pointTypes[1:rows][i], lty=i, col=colorUsed[1:rows][i])
    }
    legend(table[10,2], f(legPosY), algLabels, pch=pointTypes[2:rows], 
           lty=2:rows, col=colorUsed[2:rows], cex=0.8)
  } else {
    table = table[rowOrder, ]
    print(table)
    plot(as.numeric(table[1,-1]), as.numeric(table[2, -1]), 
         main=name, type="o", lty=2, col=colorUsed[2], 
         xlab="initial seed size", ylab="influence on network", pch=pointTypes[2],
         ylim=c(min(table[-1,4]), max(table[, -1])))
    for (i in 3:rows) {
         lines(as.numeric(table[1,-1]), as.numeric(table[i,-1]), lty=i, 
               col=colorUsed[1:rows][i], pch=pointTypes[1:rows][i], type="o")
    }
    legend(table[10,2] + 1, max(table[, -1]), algLabels, lty=2:rows,
           cex=0.8, col=colorUsed[2:rows], pch=pointTypes[2:rows])
  }
}
