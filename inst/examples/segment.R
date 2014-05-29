library(lattice)
library(qtutils)
library(bocra)

img <- Qt$QImage("/home/deepayan/Dropbox/OCR_samples/gupibagha/page01-processed.png")

## img  <-  read.qimage("golpo-015.jpg")
ipixmap <- Qt$QPixmap$fromImage(img)


## qscene

scene <- Qt$QGraphicsScene()
scene$addPixmap(ipixmap)

(view <- Qt$QGraphicsView(scene))

## view$scale(2, 2)
## view$scale(1/2, 1/2)
view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)

imat <- qimage2matrix(img)

## image(imat, ylim = c(1, 0))

imat01 <- ifelse(imat > 0.5, 1, 0)

## plot(colMeans(imat01), type = "l")
## abline(v = identifyGaps(colMeans(imat), 0.75))

if (FALSE)
{
    ipixmap <- Qt$QPixmap$fromImage(matrix2qimage(colors = imat01))
}


scene$clear()
scene$addPixmap(ipixmap)

breaks <- identifyWords(imat01, line.thres = 0.80, word.thres = 0.99)

## topOfHeadline1 <- function(x)
## {
##     ## x matrix, 1 means ink, 0 background.  want to find 1-pixels
##     ## which have 0-pixels to the north
##     r <- row(x)
##     c <- col(x)
##     w <- which(x == 0)
##     ## x is stored in column-major order, so north is previous entry
##     ## (assuming none in w are boundary pixesls)
##     e <- w[x[w-1] == 1]
##     list(x = c[e], y = -r[e])
## }


topOfHeadline2 <- function(x, prior = NA_integer_, threshold = 5)
{
    ## x matrix, 1 means ink, 0 background.  want to find norhmost
    ## 1-pixel in each column
    keep <- colSums(x) != 0
    if (!any(keep)) return (NA_integer_)
    ## list(x = seq_len(ncol(x))[keep],
    ##      y = -apply(x, 2, which.max)[keep])
    ans <- median(apply(x, 2, which.max)[keep])
    if (!is.na(prior) &&
            (abs(prior - ans) > 5 || ncol(x)/nrow(x) < 1.0))
        ans <- prior
    ans
}

## identify per-word headline location prior to segmentation. Retain for later use

locateHeadline <- function(x, breaks)
{
    line.locs <- rep(NA_integer_, length(breaks$lineBreaks) - 1L)
    word.locs <- vector(mode = "list", length = length(line.locs))
    for (i in seq_along(breaks$wordBreaks))
    {
        wb <- breaks$wordBreaks[[i]]
        wcols <- with(breaks, lineBreaks[i]:lineBreaks[i + 1])
        if (length(wb) > 1)
        {
            line.locs[i] <- topOfHeadline2(1 - t(imat01[, wcols]))
            word.locs[[i]] <- rep(NA_integer_, length(wb) - 1L)
            for (j in seq_len(length(wb) - 1))
            {
                wrows <- wb[j]:wb[j + 1]
                w <- 1 - t(imat01[wrows, wcols])
                ## mpos <- locateMatra(w)
                word.locs[[i]][j] <- topOfHeadline2(w, prior = line.locs[i])
            }
        }
    }
    list(lineLocs = line.locs, wordLocs = word.locs)
}

headlines <- locateHeadline(imat01, breaks)


## for (y in breaks$lineBreaks)
##     scene$addLine(0, y, scene$width()-1, y, Qt$QPen(qcolor("red")))

for (i in seq_along(breaks$wordBreaks))
{
    y <- breaks$lineBreaks[i]
    scene$addLine(0, y, scene$width()-1, y, Qt$QPen(qcolor("red")))
    for (j in seq_along(headlines$wordLocs[[i]]))
    {
        x1 <- breaks$wordBreaks[[i]][j]
        x2 <- breaks$wordBreaks[[i]][j+1]
        h <- y + headlines$wordLocs[[i]][j] - 1
        scene$addLine(x1, breaks$lineBreaks[i], x1, breaks$lineBreaks[i+1],
                      Qt$QPen(qcolor("red")))
        scene$addLine(x1, h, x2, h,
                      Qt$QPen(qcolor("blue")))
    }
}

for (i in seq_along(breaks$wordBreaks))
{
    wb <- breaks$wordBreaks[[i]]
    wcols <- with(breaks, lineBreaks[i]:lineBreaks[i + 1])
    if (length(wb) > 1)
    {
        for (j in seq_len(length(wb) - 1))
        {
            wrows <- wb[j]:wb[j + 1]
            w <- 1 - t(imat01[wrows, wcols])
            ## mpos <- locateMatra(w)
            mpos <- headlines$wordLocs[[i]][j]
            dpos <-
                deleteMatraPosition(w, matraPos = mpos,
                                    startPoint = 0, endPoint = 5,
                                    reEnterPoint = 20)
            foo <- removeMatra(w, matraPos = mpos, locations = which(dpos),
                               verPixDel = 5)
            imat01[wrows, wcols] <- 1 - t(foo)
        }
    }
}

ipixmap <- Qt$QPixmap$fromImage(matrix2qimage(colors = imat01))
scene$clear()
scene$addPixmap(ipixmap)

for (i in seq_along(breaks$wordBreaks))
{
    y <- breaks$lineBreaks[i]
    hl <- y + headlines$lineLocs[i] - 1L
    scene$addLine(0, y, scene$width()-1, y, Qt$QPen(qcolor("red")))
    scene$addLine(0, hl, scene$width()-1, hl, Qt$QPen(qcolor("green")))
    for (j in seq_along(headlines$wordLocs[[i]]))
    {
        x1 <- breaks$wordBreaks[[i]][j]
        x2 <- breaks$wordBreaks[[i]][j+1]
        h <- y + headlines$wordLocs[[i]][j] - 1
        scene$addLine(x1, breaks$lineBreaks[i], x1, breaks$lineBreaks[i+1],
                      Qt$QPen(qcolor("red")))
        scene$addLine(x1, h, x2, h,
                      Qt$QPen(qcolor("blue")))
    }
}


getWord <- function(x = imat01, breaks, line, word)
{
    i <- line; j <- word
    wb <- breaks$wordBreaks[[line]]
    wrows <- wb[j]:wb[j + 1]
    wcols <- with(breaks, lineBreaks[i]:lineBreaks[i + 1])
    1 - t(x[wrows, wcols])
}

levelplot(findComponents(getWord(imat01, breaks, 2, 3)))


## component.list.matrix <- function(w)
## {
##     c <- findComponents(w)
##     n <- max(c, na.rm = TRUE)
##     lapply(seq_len(n),
##            function(i) {
##                tmp <- (c == i) # logical
##                rtmp <- range(row(tmp)[tmp])
##                ctmp <- range(col(tmp)[tmp])
##                ## 0 + tmp[rtmp[1]:rtmp[2], ctmp[1]:ctmp[2], drop = FALSE]
##                0 + tmp[rtmp[1]:rtmp[2], , drop = FALSE]
##            })
## }

component.list.tuple <- function(w, headline, min.pixel = 5, ...)
{
    ## return as (x,y) positions of ink pixels
    c <- findComponents(w)
    n <- max(c, na.rm = TRUE)
    lapply(seq_len(n),
           function(i) {
               tmp <- (c == i) # logical
               if (sum(tmp) >= min.pixel)
               {
                   xmid <- round(median(col(tmp)[tmp]))
                   md <- list(xmid = xmid, ...) # meta-data to locate in original image
                   structure(list(x = col(tmp)[tmp] - xmid,
                                  y = row(tmp)[tmp] - headline),
                             metadata = md)
               }
               else NULL
           })
}



## cl <- component.list(getWord(image01, breaks, 2, 3))

cl <- list()

for (line in seq_along(breaks$wordBreaks))
{
    for (word in seq_len(length(breaks$wordBreaks[[line]]) - 1))
    {
        print(c(line, word))
        cl <- c(cl, component.list.tuple(getWord(imat01, breaks, line, word),
                                         headline = headlines$wordLocs[[line]][word],
                                         line = line, word = word))
    }
}

cl <- cl[!sapply(cl, is.null)]


setdiffDistance <- function(w1, w2)
{
    ## avg of number of points remaining in w1\w2 / size of {w1}
    ## (asymmetric, will average later)
    ## crude calculation
    s1 <- with(w1, sort(paste(x, y, sep=".")))
    s2 <- with(w2, sort(paste(x, y, sep=".")))
    length(setdiff(s1, s2)) / length(s1)
    s <- unique(c(s1, s2))
    (length(s) - length(s2)) / length(s1)
}



setdiffDistance2 <- function(w1, w2)
{
    ## convert to matrix before comparing.  Indices can be negative, so first:
    xrng <- range(w1$x, w2$x)
    yrng <- range(w1$y, w2$y)
    nx <- diff(xrng) + 1
    ny <- diff(yrng) + 1
    m1 <- numeric(nx * ny)
    m2 <- numeric(nx * ny)
    m1[ 1 + w1$y - yrng[1] + ny * (w1$x - xrng[1]) ] <- 1
    m2[ 1 + w2$y - yrng[1] + ny * (w2$x - xrng[1]) ] <- 1
    m <- pmax(m1, m2)
    d1 <- (sum(m) - sum(m2)) / sum(m1)
    d2 <- (sum(m) - sum(m1)) / sum(m2)
    0.5 * (d1 + d2)
}



dmat <- matrix(0, length(cl), length(cl))

for (i in seq_along(cl))
    for (j in seq_len(i-1))
{
    if (j == 1) print(i)
    dmat[i, j] <- setdiffDistance2(cl[[i]], cl[[j]])
}

save(dmat, file = "dmat.rda")

dmat.orig <- dmat

dmat[upper.tri(dmat)] <- t(dmat)[upper.tri(dmat)]

h <- hclust(as.dist(dmat))

levelplot(dmat[h$order, h$order])

ocl <- cl[h$order]

for (i in 1:length(ocl))
{
    d <- ocl[[i]]
    ## print(range(d$y))
    plot(xyplot(-y ~ x, d, grid = TRUE, aspect = "iso", pch = 16,
                xlim = c(-30, 30), ylim = c(-50, 20)))
    ## Sys.sleep(0.1)
}

odmat <- dmat[h$order, h$order]
successiveDistances <- diag(odmat[-nrow(odmat), -1])


names(ocl) <- as.character(seq_along(ocl))
docl <- do.call(make.groups, lapply(ocl, function(x) do.call(data.frame, x)))
rownames(docl) <- NULL

p <- xyplot(-y ~ x | which, data = docl, pch = ".", strip = FALSE,
            xlim = c(-30, 30), ylim = c(-50, 20), layout = c(10, 10))

pdf("clustered.pdf")
plot(p)
dev.off()

pdf("segmented-example.pdf", width = 10, height = 7)
levelplot(t(imat01), useRaster = TRUE, col.regions = grey.colors)
dev.off()




d <- ocl[[sample(length(ocl), 1)]];
## plot(xyplot(-y ~ x, d, grid = TRUE, aspect = "iso", pch = 16,
##             xlim = c(-30, 30), ylim = c(-50, 20)))
md <- attr(d, "metadata")
w <- t(getWord(imat01, breaks, md$line, md$word))
levelplot(w, ylim = c(ncol(w), 1))
trellis.focus(highlight = FALSE)
panel.points(d$x + md$xmid, d$y + headlines$wordLocs[[md$line]][md$word])
trellis.unfocus()

## NEXT: need an interface to select templates.  Maybe fill in the
## list of standard characters one by one, and then ask user to label
## unrecognized ones.

## marks may need special treatment (chandrabindu, bisarga)

locateWord <- function(x, y)
{
    ## x, y: click location
    line <- findInterval(y, breaks$lineBreaks)
    word <- findInterval(x, breaks$wordBreaks[[line]])
    w <- t(getWord(imat01, breaks, line, word))
    plot(levelplot(w, ylim = c(ncol(w), 1)))
}


## To identify words using mouseclicks, need new class inheriting from QGraphicsScene

qsetClass("SceneWithEvents", Qt$QGraphicsScene)

qsetMethod("mouseDoubleClickEvent", SceneWithEvents,
           function(event) {
               xy <- as.numeric(event$scenePos())
               locateWord(xy[1], xy[2])
           }, access = "protected")

escene <- SceneWithEvents()
escene$addPixmap(ipixmap)
(eview <- Qt$QGraphicsView(escene))




## saveWords(imat, breaks, outdir = "saved-words")

## load("saved-words/w_0026_0006.rda") # loads w

## d <- dimage(w)
