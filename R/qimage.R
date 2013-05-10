
## Tools to deal with images using Qt$QImage

read.qimage <- function(file)
{
    Qt$QImage(file)
}

qimage2matrix <- function(img)
{
    ## This creates a double matrix of entries in [0,1], converting to
    ## grayscale if necessary.
    .Call("qt_qimage2matrix", img)
}



