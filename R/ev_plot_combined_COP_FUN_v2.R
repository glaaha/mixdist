#' @export
#################################################################################
##
## FUNCTION for plotting:
## (war erst in Bild13_Copula_mischvt...R Skript, aber jetzt als separates Skript file)
##
#ev_plot_combined<- function() {
ev_plot_combined_COP <- function(AM_list) {
  # Note that hyearstart=4, seas2start=11 is set in the code, when function cdfwei_MixedC() is called

  colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cBB9 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#888888")

  # NEW_2025: Takes AM_list with AM, AMs, AMw
  AM <- AM_list$AM
  AMs <- AM_list$AMs
  AMw <- AM_list$AMw

  # -) no seasons
  evpointsGL(AM, col=eval(cBB9[9]))
  #.y <- quawei(seq(0.0001,0.9999,0.0001), para = x.pelwei)
  .y <- quawei(seq(0.0001,0.9999,0.0001), para = pelwei(samlmu(AM)))
  #evpointsGL(.y, col="black", type="l" )
  evpointsGL(.y, col=cBB9[9], type="l" )


  # -) summer
  #points(evdistq0.pp(AMs), col="red") # empir distr
  points(evdistq0.pp(AMs), col=cBB9[7]) # empir distr
  #.ys <- quawei(seq(0.0001,0.9999,0.0001), para = x.pelwei.s)
  .ys <- quawei(seq(0.0001,0.9999,0.0001), para = pelwei(samlmu(AMs)))
  #evpointsGL(.ys, col="red", type="l" )
  evpointsGL(.ys, col=cBB9[7], type="l" )

  # -) winter
  points(evdistq0.pp(AMw), col=cBB9[6]) # empir distr
  #x.pelwei.w<- pelwei(lmom, bound=0); x.pelwei.w
  #.yw <- quawei(seq(0.0001,0.9999,0.0001), para = x.pelwei.w)
  .yw <- quawei(seq(0.0001,0.9999,0.0001), para = pelwei(samlmu(AMw)))
  #evpointsGL(.yw, col="blue", type="l" )
  evpointsGL(.yw, col=cBB9[6], type="l" )


  ### Mixed distribution
  .ymin <- par("usr")[3] ; .ymax <- par("usr")[4]
  .y1 <- seq(round(.ymin, 1), round(.ymax, 1), by=0.01) # feine Diskretisierung für y.lim (i.e. plot area)
  #y1 <- seq(0, 1, by=0.01)

  # NEU (Funktion nutzen !!!:
  #cdfwei_Mixed(x=.y1, y=.y1, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), plot=TRUE, col=cBB9[4], lty="dashed", lwd=3)
  cdfwei_Mixed(x=.y1, y=.y1, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), plot=TRUE, col=cBB9[4])


  # Add empirical prob of mixed distribution
  #pobs_mixed_FUN(AM, AMs, AMw, plot=TRUE, col=eval(cBB9[4]), pch=20)
  pobs_mixed_FUN(AM, AMs, AMw, plot=TRUE, col=eval(cBB9[4]))


  ### NEU: COPULA
  # NEU (Funktion nutzen !!!:
  #cdfwei_MixedC(x=.y1, y=.y1, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=TRUE, col=cBB9[8], lty=3, lwd=3)
  cdfwei_MixedC(x=.y1, y=.y1, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=TRUE,
                    hyearstart=4, seas2start=11, col=cBB9[8], lty="dashed", lwd=2)
  #do.call(what='cdfwei_MixedC', args=c(x=.y1, y=.y1, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=TRUE,
  #                                           hyearstart=4, seas2start=11, col=cBB9[8]))

  # Add empirical "Copula" prob of mixed distribution
  #/!\
  pobs_mixed_FUN(AM, AMs, AMw, plot=FALSE, plotC=TRUE, col=eval(cBB9[8]), pch=20)  # Empirical copula Estimator passt nicht gut - ungenau bei kleinem Sample?


  # legende
  #legend(x="topleft", inset=0.01, bty="n", legend=c(" Winter ", " Summer ", " Mixed ", " Annual "),
  #       col = c("blue", "red", "green", "black"), lty=1, pt.bg = 'white', bg="white")
  legend(x="topleft", inset=0.01, bty="n", legend=c(" Winter ", " Summer ", " Mixed ", " Annual "),
         col = cBB9[c(6,7,4,9)], lty=1, pt.bg = 'white', bg="white")

}
