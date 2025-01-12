evdistq0.pp <-
function (y, qfunc, para, npoints = 101, plim, xlim = c(-2, 5), 
    ylim, type, xlab = expression("Reduced variate,  " * -log(-log(italic(F)))), 
    ylab = "Quantile", rp.axis = TRUE, ...) 
{
    if (!missing(plim)) 
        xlim <- c(-log(-log(plim)))
    missing.ylim <- missing(ylim)
    if (missing.ylim) 
        ylim <- c(0, 1)
    if (missing(y)) {
        xx <- 0
        yy <- 0
        type <- "n"
    }
    else {
        yy <- sort(y[!is.na(y)])
        lyy <- length(yy)
        xx <- -log(-log(((1:lyy) - 0.44)/(lyy + 0.12)))
        if (missing.ylim) 
            ylim <- c(min(0, yy), max(yy))
        if (missing(plim)) 
            xlim <- c(min(xlim[1], xx[1]), max(xlim[2], xx[lyy]))
        if (missing(type)) 
            type <- "p"
    }
    if (!missing(qfunc) && missing.ylim) {
        xval <- range(xlim)
        pval <- exp(-exp(-xval))
        yval <- if (missing(para)) 
            qfunc(pval)
        else if (is.list(para)) 
            do.call(qfunc, c(list(pval), para))
        else qfunc(pval, para)
        ylim <- range(c(ylim, yval))
    }
    #plot(xx, yy, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, 
    #    type = type, ...)
    #if (!missing(qfunc)) 
    #    evdistq(qfunc, para, npoints = npoints, ...)
    #if (rp.axis) {
    #    parusr <- par("usr")
    #    rp.lab <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000, 10000, 
    #        1e+05, 1e+06, 1e+07, 1e+08)
    #    rp.tic <- -log(-log(1 - 1/rp.lab))
    #    crit <- (rp.tic >= parusr[1] & rp.tic <= parusr[2])
    #    rp.tic <- rp.tic[crit]
    #    rp.lab <- rp.lab[crit]
    #    rp.ypos <- parusr[3] + (parusr[4] - parusr[3]) * 0.05
    #    axis(side = 3, at = rp.tic, labels = rp.lab, pos = rp.ypos)
    #    text((min(rp.tic) + max(rp.tic)) * 0.5, rp.ypos + par("cxy")[2] * 
    #        par("mgp")[1], "Return period", adj = c(0.5, 0))
    #}
    return(list(x=xx, y=yy))
}
