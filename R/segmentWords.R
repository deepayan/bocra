
## FIXME: add minimum required consecutive pixels to be above
## threshold, and use center rather than max.

identifyGaps <- function(x, threshold = 0.75)
{
    ## background white, so find stretches of high average color, and
    ## and compute midpoint of runs

    i <- x > threshold
    w <- which(i)
    changePoints <- w[1 + which(diff(w) != 1)]
    i <- as.numeric(i)
    for (k in changePoints) i[k:length(i)] <- i[k:length(i)] + 1L
    x <- x[w]
    i <- i[w]
    as.vector(tapply(x, list(i), which.max) + c(1L, changePoints) - 1L)
}


identifyWords <- function(x, line.thres = 0.75, word.thres = 0.9) # x is [0,1] matrix
{
    lineBreaks <- identifyGaps(colMeans(x), threshold = line.thres)
    ilines <- lapply(seq_len(length(lineBreaks) - 1),
                     function(i) seq(lineBreaks[i], lineBreaks[i+1]))
    getWordBreaks <- function(iline)
    {
        y <- x[, iline, drop = FALSE]
        wordBreaks <- identifyGaps(rowMeans(y), threshold = word.thres)
        wordBreaks
    }
    list(lineBreaks = lineBreaks,
         wordBreaks = lapply(ilines, getWordBreaks))
}

saveWords <- function(x, breaks = identifyWords(x),
                      outdir = stop("Specify folder name"),
                      max.aspect = 1)
{
    ## words are usually short and wide.  Since our current word
    ## detection is error prone, we remove some false words by
    ## filtering on aspect ratio.
    saveWord <- function(w, line, word) # w is a sub-matrix of x
    {
        if (ncol(w) / nrow(w) < max.aspect)
        {
            save(w, file = file.path(outdir, sprintf("w_%04d_%04d.rda", line, word)))
        }
    }
    for (i in seq_along(breaks$wordBreaks))
    {
        wb <- breaks$wordBreaks[[i]]
        if (length(wb) > 1)
        {
            for (j in seq_len(length(wb)-1))
            {
                w <- x[wb[j]:wb[j+1] , with(breaks, lineBreaks[i]:lineBreaks[i+1])]
                saveWord(w, i, j)
            }
        }
    }
}

