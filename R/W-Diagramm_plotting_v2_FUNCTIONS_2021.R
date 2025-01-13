#' @export
########################################################################
## Fct: Return period scale plotten (mit scaling Faktor fak)
return.scale <- function(x=AM, ...)
{
	T<-c(2,5,10,20,100); f<-1/T; f
	RV <- -log(-log(f)); RV
	fak <- max(AM)/150
	pts <- list(x = c(min(RV),max(RV)) , y = c(2,2)*fak)
	lines(pts)
	for(i in 1:6){
	pts <- list(x = rep(RV[i],2) , y = c(2,5)*fak)
	lines(pts)
	}
	text(x=RV,y=rep(5.0,5)*fak, labels=T, pos=3, cex=1)
	text(x=mean(min(RV),max(RV)), y=20*fak, labels="J\U00E4hrlichkeit T (a)", pos=4, cex=1)

}

#######################################################################
# Fct: Frequency axis plotten
freq.axis <- function(x=AM, ...)
{
	RV2 <- seq(-2, 5); RV2
	f2 <- exp(-exp(-RV2)); f2<- round(f2,3); f2
	axis(side=3, at = RV2[1], labels = f2[1])
	axis(side=3, at = RV2[-1], labels = round(f2[-1],3))

	mtext("P(x)", side=3, line=3, outer=FALSE, at = mean(par("usr")[1:2]), cex=1, font=1)
	# mit Grid
	for(tx in seq(-2,5)) abline(v=tx, lty="dotted")
	ti <- axTicks(side=2)
	for(ty in seq(min(ti), max(ti), by=(ti[2]-ti[1])/2)) abline(h=ty, lty="dotted")
}

#######################################################################
# Fct: Empir. Vt. mit Weibull plotting positions zeichnen
# Dazu muss man be evplot(...type="n") setzen, um empty plot zu zeichnen
evpointsGL <- function (y, ...)
{
    yval <- sort(y[!is.na(y)])
    n <- length(yval)
    xval <- -log(-log((1:n)/(n + 1))) # Weibull plotting positions
    points(xval, yval, ...)
}
