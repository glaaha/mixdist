
pobs_mixed_FUN <- function(AM=AM, AMs=AMs, AMw=AMw, plot=FALSE, plotC=FALSE, ...) {
  p.s <- vector(length=length(AM))
  p.w <- vector(length=length(AM))

  for (i in 1:length(AM)) {
    AM.i <- AM[i]
    pAMs <- pobs(AMs)
    pAMw <- pobs(AMw)
    #p.imax.s <- max(pAMs[sAMs<=AM[i]])
    #p.imin.s <- min(pAMs[sAMs>=AM[i]])
    if (AM[i] >= min(AMs) & AM[i] <= max(AMs))   p.s[i] <- mean(c(max(pAMs[AMs<=AM[i]]), min(pAMs[AMs>=AM[i]])))
    if (AM[i] > max(AMs))   p.s[i] <- mean(c(max(pAMs, 1))) # Frage ob 1 oder max(pAMs) oder was?)
    
    if (AM[i] >= min(AMw)) p.w[i] <- mean(c(max(pAMw[AMw<=AM[i]]), min(pAMw[AMw>=AM[i]])))
    if (AM[i] > max(AMw))   p.w[i] <- mean(c(max(pAMw, 1))) # Frage ob 1 oder max(pAMw) oder was?)
  }
  
  p.mix <- 1-((1-p.w) * (1-p.s)) 	# Like DWA 551, but Inverse probas multiplied
  
  # Versuch mit empir. Copula (AND-estimator)
  X.obs <- as.matrix(cbind(pobs(AMs), pobs(AMw)))
  u.obs <- as.matrix(cbind(pobs(AM), pobs(AM)))
  .pMvde <- C.n(u.obs, X.obs, smoothing = "b")
 # p.mix.C  <- 1-  (1-p.s - p.w + .pMvde*p.s*p.w)
 ## p.mix.C  <- 1-  (1-p.s - p.w + .pMvde)
  
  #FxyC  <- Fx + Fy - x.pMvd*Fx*Fy  # ACHTUNG - letzter Term !!!
  
  p.mix.C <- p.s + p.w - .pMvde*p.s*p.w
  
  ## NEU - 2022-07-20
  #q_mat <- as.matrix(cbind(x, y))
  #.F.n <- F.n(x=q_mat, X=X.obs)
  
  X.obs <- as.matrix(cbind((AMs), (AMw))) # original observations for computing the empirical cdf !
  x.obs <- as.matrix(cbind((AM), (AM))) # pobs [0,1] of magnitude q used as evaluation points (=q_mat)
  
  .F.n <- F.n(x=x.obs, X=X.obs)
  p.mix.C <- p.s + p.w - .F.n
  
  
  # Plotting: 
  if (plot==TRUE) {
    p.mix1 <- -log(-log(p.mix))
    #points(p.mix1, AM, col="chocolate", pch="+")
    points(p.mix1, AM, ...)
  }
  if (plotC==TRUE) {
    p.mix.c1 <- -log(-log(p.mix.C))
    points(p.mix.c1, AM, ...)
  }
  
  return(list(AM=AM, p.mix=p.mix, p.mix.C=p.mix.C))
}

# Anwendung:
# pobs_mixed_FUN(AM=AM, AMs=AMs, AMw=AMw, plotC=TRUE)
