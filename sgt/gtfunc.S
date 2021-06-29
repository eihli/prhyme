    nrzest<-function(r, nr)
    {
    d <- c(1, diff(r))
    dr <- c(0.5 * (d[-1] + d[ - length(d)  ]), d[length(d)])
    return(nr/dr)
    }

    rstest<-function(r, coef)
    {
        return(r * (1 + 1r)^(1 + coef[2]))
    }
