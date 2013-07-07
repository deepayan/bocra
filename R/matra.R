

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
##' @param startPoint minimum requirement of staying in black pixel
##' @param endPoint maximum tolerance limit of staying in black pixel
##' @param reEnterPoint number of pixels required to re-enter in black pixel
##' @return Locations where matra should be deleted?
##' @author Kaustav Nandy
deleteMatraPosition <-
    function(x, matraPos = locateMatra(x),
             startPoint = 0, endPoint = 3, reEnterPoint = 25)
{
    fm <- .Call("segment", x, nrow(x), ncol(x), matraPos, 1, 2, 3)
    tmp1 <- which(fm$leaveBlack >= matraPos + startPoint &
                  fm$leaveBlack <= matraPos + endPoint)
    tmp2 <- which(fm$reEnterBlack == -1 | fm$reEnterBlack >= reEnterPoint)
    sg <- intersect(tmp1, tmp2)
    tm <- .Call("detectDeletePortion", as.integer(sg), length(sg), 0)
    sg * tm
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
    for (i in locations)
    {
        x[ (matraPos - verPixDel) : (matraPos + verPixDel), i ] <- 0
    }
    x
}

