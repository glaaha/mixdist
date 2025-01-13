#' @export
evdistqR <- function (qfunc, para, npoints = 101, ...)
{
#####
# Version von evdistq für reverse Vt wie GEV(-X) ...
#####
    parusr <- par("usr")
    xval <- seq(from = parusr[1], to = parusr[2], length = npoints)
    pval <- exp(-exp(-xval))
    yval <- if (missing(para))
        qfunc(pval)
    else if (is.list(para))
        do.call(qfunc, c(list(pval), para))
    else qfunc(1-pval, para)		# for (-X)
    lines(xval, (-1)*yval, ...)	# for (-X)
}

