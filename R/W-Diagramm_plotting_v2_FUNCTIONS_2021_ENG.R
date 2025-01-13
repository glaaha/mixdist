
########################################################################
## Fct: Return period scale plotten (mit scaling Faktor fak)

#' @export
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

#' @export
return.scale.ENG <- function(x=AM, ...)
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
  text(x=mean(min(RV),max(RV)), y=20*fak, labels="Return period T (a)", pos=4, cex=1)

}

#' @export
return.scale.ENG.1 <- function(x=AM, ...)
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
  text(x=RV,y=rep(5.0,5)*fak, labels=T, pos=3, cex=0.8)
  text(x=mean(min(RV),max(RV)), y=20*fak, labels="Return period T (a)", pos=4, cex=1)

}

#' @export
return.scale.ENG.ZOOM <- function(x=AM, ...)
{
  T<-c(2,5,10,20,100); f<-1/T; f
  RV <- -log(-log(f)); RV
  ymin <- min(AM)/2
  ymin <- min(AM)-((median(AM)-min(AM))/2)
  #ymin <- par("usr")[3]
  #fak <- median(AM)/150
  fak <- median(AM)/400
  #fak <- (median(AM)-ymin)/150
  pts <- list(x = c(min(RV),max(RV)) , y = c(2,2)*fak+ymin)
  pts <- list(x = c(min(RV),max(RV)) , y = c(0,0)*fak+ymin)
  #pts <- list(x = c(min(RV),max(RV)) , y = c(min(AM),min(AM)))
  lines(pts)
  for(i in 1:6){
    #pts <- list(x = rep(RV[i],2) , y = c(2,5)*fak)
    pts <- list(x = rep(RV[i],2) , y = c(0,3)*fak+ymin)
    #pts <- list(x = rep(RV[i],2) , y = c(min(AM),min(AM)+3*fak))
    lines(pts)
  }
  text(x=RV,y=rep(5.0,5)*fak+ymin, labels=T, pos=3, cex=1)
  text(x=mean(RV), y=22*fak+ymin, labels="Return period T (a)",adj=0.5,  cex=1) # pos=2,
}

#' @export
return.scale.ENG.ZOOM.1 <- function(x=AM, ...)
{
  T<-c(2,5,10,20,100); f<-1/T; f
  RV <- -log(-log(f)); RV
  ymin <- min(AM)/2
  ymin <- min(AM)-((median(AM)-min(AM))/2)
  #ymin <- par("usr")[3]
  #fak <- median(AM)/150
  fak <- median(AM)/400
  #fak <- (median(AM)-ymin)/150
  pts <- list(x = c(min(RV),max(RV)) , y = c(2,2)*fak+ymin)
  pts <- list(x = c(min(RV),max(RV)) , y = c(0,0)*fak+ymin)
  #pts <- list(x = c(min(RV),max(RV)) , y = c(min(AM),min(AM)))
  lines(pts)
  for(i in 1:6){
    #pts <- list(x = rep(RV[i],2) , y = c(2,5)*fak)
    pts <- list(x = rep(RV[i],2) , y = c(0,3)*fak+ymin)
    #pts <- list(x = rep(RV[i],2) , y = c(min(AM),min(AM)+3*fak))
    lines(pts)
  }
  text(x=RV,y=rep(5.0,5)*fak+ymin, labels=T, pos=3, cex=0.8)
  text(x=mean(RV), y=22*fak+ymin, labels="Return period T (a)",adj=0.5,  cex=1) # pos=2,
}
#######################################################################
# Fct: Frequency axis plotten
#' @export
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

#' @export
freq.axis.ZOOM <- function(x=AM, grid=FALSE, ...)
{
  RV2 <- seq(-2, exp(-exp(0)), by=0.5); RV2
  f2 <- exp(-exp(-RV2)); f2<- round(f2,3); f2
  axis(side=3, at = RV2[1], labels = f2[1])
  axis(side=3, at = RV2[-1], labels = round(f2[-1],3))

  mtext("P(x)", side=3, line=3, outer=FALSE, at = mean(par("usr")[1:2]), cex=1, font=1)
  # mit Grid
  if (grid==TRUE) {
  for(tx in seq(-2,0,by=0.5)) abline(v=tx, lty="dotted")
  ti <- axTicks(side=2)
  for(ty in seq(min(ti), max(ti), by=(ti[2]-ti[1])/2)) abline(h=ty, lty="dotted")
  }
}

#######################################################################
# Fct: Empir. Vt. mit Weibull plotting positions zeichnen
# Dazu muss man be evplot(...type="n") setzen, um empty plot zu zeichnen

#' @export
evpointsGL <- function (y, ...)
{
    yval <- sort(y[!is.na(y)])
    n <- length(yval)
    xval <- -log(-log((1:n)/(n + 1))) # Weibull plotting positions
    points(xval, yval, ...)
}
