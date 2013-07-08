library(lattice)
library(bocra)

img <- Qt$QImage("/home/deepayan/Dropbox/OCR_samples/gupibagha/page01-processed.png")

## img  <-  read.qimage("golpo-015.jpg")
ipixmap <- Qt$QPixmap$fromImage(img)

## qscene

scene <- Qt$QGraphicsScene()
scene$addPixmap(ipixmap)

(view <- Qt$QGraphicsView(scene))

view$scale(2, 2)
view$scale(1/2, 1/2)
view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)

imat <- qimage2matrix(img)

## image(imat, ylim = c(1, 0))

imat01 <- ifelse(imat > 0.5, 1, 0)

## plot(colMeans(imat01), type = "l")
## abline(v = identifyGaps(colMeans(imat), 0.75))

scene$clear()
scene$addPixmap(ipixmap)

breaks <- identifyWords(imat01, line.thres = 0.80, word.thres = 0.99)

for (y in breaks$lineBreaks)
    scene$addLine(0, y, scene$width()-1, y, Qt$QPen(qcolor("red")))

for (i in seq_along(breaks$wordBreaks))
{
    for (x in breaks$wordBreaks[[i]])
        scene$addLine(x, breaks$lineBreaks[i], x, breaks$lineBreaks[i+1],
                      Qt$QPen(qcolor("red")))
}

for (i in seq_along(breaks$wordBreaks))
{
    wb <- breaks$wordBreaks[[i]]
    if (length(wb) > 1)
    {
        for (j in seq_len(length(wb) - 1))
        {
            wrows <- wb[j]:wb[j + 1]
            wcols <- with(breaks, lineBreaks[i]:lineBreaks[i + 1])
            w <- 1 - t(imat01[wrows, wcols])
            mpos <- locateMatra(w)
            dpos <-
                deleteMatraPosition(w, matraPos = mpos,
                                    startPoint = 0, endPoint = 5,
                                    reEnterPoint = 15)
            foo <- removeMatra(w, matraPos = mpos, locations = which(dpos),
                               verPixDel = 5)
            imat01[wrows, wcols] <- 1 - t(foo)
        }
    }
}

pdf("segmented-example.pdf", width = 10, height = 7)
levelplot(t(imat01), useRaster = TRUE, col.regions = grey.colors)
dev.off()









## saveWords(imat, breaks, outdir = "saved-words")

## load("saved-words/w_0026_0006.rda") # loads w

## d <- dimage(w)
