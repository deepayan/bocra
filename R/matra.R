

## locateMatra(x): locate the position of the matra on the basis of
## calculation of maximum row-sum

locateMatra <- function(x)
{
    rs <- rowSums(x) 
    floor(median(which(rs == max(rs)))) ## Note -1 has been removed
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x transformed data containing the 0-1 pixel grey value matrix
##' @param matraPos Position of matra
##' @param startPoint Minimum number of pixels within which we have to
##' exit black to consider this position as part of the matra.
##' Usually should be 0 or possibly a small negative integer.
##' @param endPoint The maximum number of pixels within which we have
##' to exit black to consider this position as part of the matra.
##' Should be expected thickness of the matra.
##' @param reEnterPoint Number of pixels within which we should not
##' re-encounter a black pixel if this is part of the matra.  This
##' should be slighly less than the font height (from matra downwards).
##' @return Locations where matra should be deleted?
##' @author Kaustav Nandy
deleteMatraPosition <-
    function(x, matraPos = locateMatra(x),
             startPoint = -1, endPoint = 3, reEnterPoint = 25)
{
    fm <- .Call("segment", x, nrow(x), ncol(x), matraPos, 1, 2, 3)
    sg <-
        with(fm, (leaveBlack >= matraPos + startPoint &
                  leaveBlack <= matraPos + endPoint &
                  (reEnterBlack == -1 | reEnterBlack >= matraPos + reEnterPoint)))
    ## tm <- .Call("detectDeletePortion", as.integer(sg), length(sg), 0)
    ## sg * tm
    sg
}


##' substitute appropriate black pixel by white one to segment the letters
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param matraPos 
##' @param horPixDel number of pixels in horizontal direction to revert into white pixel
##' @param verPixDel number of pixels in vertical direction to revert into white pixel
##' @return 
##' @author Kaustav Nandy
removeMatra <-
    function(x, matraPos = locateMatra(x),
             locations = deleteMatraPosition(x),
             horPixDel = 0, # ignored
             verPixDel = 2)
{
    locations <- locations[locations > 0]
    from <- max(1, matraPos - verPixDel)
    to <- min(nrow(x), matraPos + verPixDel)
    x[from:to, locations] <- 0
    x
}


identifyComponents <- function(x) 
{
    ans <- .Call("componentsRosenfeld", ncol(x), nrow(x))
    ans[ans == 0] <- NA
    dim(ans) <- dim(x)
    ans
}

