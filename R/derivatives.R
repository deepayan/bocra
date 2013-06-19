

dimage <- 
    function(x, bw = 3, maxdist = ceiling(bw))
{
    ans <- 
        .Call("diffCalBW",
              as.double(x),
              nrow(x), ncol(x),
              as.double(bw),
              as.integer(maxdist))
    dim(ans[[1]]) <- dim(ans[[2]]) <- dim(x)
}
