

library(qtbase)
library(qtutils)

img  <-  Qt$QImage("small.jpg")

## Rest of code assumes 8-bit grayscale, so check:
img$format() == Qt$QImage$Format_Indexed8
img$isGrayscale()

ipixmap <- Qt$QPixmap$fromImage(img)

## qscene

scene <- Qt$QGraphicsScene()
scene$addPixmap(ipixmap)

view <- Qt$QGraphicsView(scene)


## img$invertPixels()
## ipixmap$convertFromImage(img)

qimage2matrix <- function(img)
{
    x <- matrix(0L, img$width(), img$height())
    for (i in seq_len(nrow(x)))
        for (j in seq_len(ncol(x)))
        {
            x[i, j] <- img$pixelIndex(i-1L, j-1L)
        }
    x / 255
}

imat <- qimage2matrix(img) # slow

image(imat, ylim = c(1, 0))


identifyGaps <- function(x)
{
    ## background white, so find stretches of high average color, and
    ## and compute midpoint of runs

    i <- x > 0.75
    w <- which(i)
    changePoints <- w[1 + which(diff(w) != 1)]
    i <- as.numeric(i)
    for (k in changePoints) i[k:length(i)] <- i[k:length(i)] + 1L
    x <- x[w]
    i <- i[w]
    tapply(x, list(i), which.max) + c(1L, changePoints) - 1L
}

plot(colMeans(imat), type = "l")
abline(v = identifyGaps(colMeans(imat)))


lineBreaks <- identifyGaps(colMeans(imat))
for (y in lineBreaks) scene$addLine(0, y, scene$width()-1, y, Qt$QPen(qcolor("red")))







