#library(lfstat)
# require(copula)
# require(EnvStats) ## Nicht als dependent laden...
# require(FAdist)
# require(xtable)

# load("MixedCopPackage.RData")

# x11(width=10, height=10)

.path1 <- "./inst/extdata/HzbBis2010/"
#plot.path <- "C:/Users/Gregor/Seafile/Paper_Mixed low flow distributions/Paper1_Revision/Plots"
plot.path <- "./Plot/"
#pdf(file = file.path(plot.path, "Paper1_Rev1_FigX2.pdf"), width=10, height=10)


# Austria (a,c,d)
.pg <-205229 # Ebensee @ Langbathbach - Zone 4 (Flyschzone)
.fi <- paste(.path1, "QMittelTag", .pg, ".dat", sep="")
readLines(.fi, n = 5)
x0 <- readlfdata(.fi, type="HZB",  hyearstart = 4, baseflow =FALSE)
x2 <- x0[x0$year>=1950,]
x1 <- x2[-(1:90),]
xa <- x1

.pg <- 200287 # Schönenbach @ Subersach - Zone E (Vorarlberg, Alpenvorland)
.fi <- paste(.path1, "QMittelTag", .pg, ".dat", sep="")
readLines(.fi, n = 5)
x0 <- readlfdata(.fi, type="HZB",  hyearstart = 4, baseflow =FALSE)
x2 <- x0[x0$year>=1950,]
x1 <- x2[-(1:90),]
xc <- x1

.pg <- 211193 # St. Peter @ Vordernberger Bach - Zone A (High alps)
.fi <- paste(.path1, "QMittelTag", .pg, ".dat", sep="")
readLines(.fi, n = 5)
x0 <- readlfdata(.fi, type="HZB",  hyearstart = 4, baseflow =FALSE)
x2 <- x0[x0$year>=1950,]
x1 <- x2[-(1:90),]
xd <- x1

#b) Bayern
.pg <- 18381500 # Weg / Isen  ## good!
.path <- "./inst/extdata/Bayern_Daten_GL/"
#.path <- "C:/Users/laaha_admin/Dropbox/Work/DWA_Hennef/Magdeburg2016/R_skripts_GL/"
.fi <- paste(.path, .pg, ".dat", sep="")
readLines(.fi, n = 1)
x0 <- readlfdata(.fi, type="LFU",  hyearstart = 4, baseflow =FALSE)
x2 <- x0[x0$year>=1950,]
x1 <- x2[-(1:90),]
xb <- x1

# seasAM_FUN <- function(x1){
# ## Separate into sum/win season
# x1s <- x1[ x1$month >= 4 & x1$month <= 10 , ]
# x1w <- x1[ x1$month < 4  | x1$month > 10 , ]
#
# AM0 <- MAM(x1,n=7, yearly = TRUE)
# AM <- AM0$MAn
# #lmom <- samlmu(AM)
# #x.pelwei <- pelwei(lmom); x.pelwei
#
# AM0s <- MAM(x1s,n=7, yearly = TRUE)
# AMs <- AM0s$MAn
# #lmoms <- samlmu(AMs)
# #x.pelwei.s <- pelwei(lmoms); x.pelwei.s
#
# AM0w <- MAM(x1w,n=7, yearly = TRUE)
# AMw <- AM0w$MAn
# #lmomw <- samlmu(AMw)
# #x.pelwei.w <- pelwei(lmomw); x.pelwei.w
# return(list('AM'=AM, 'AMs'=AMs, 'AMw'=AMw))
# }


#######################
# Manual plotting (not needed any more, as included in plotting functions below)

# Plot
#width <- 12
#x11(width = width, height = width)
#pdf(file="C:/Users/Gregor/Seafile/Paper_Mixed low flow distributions/R_Analysen/Bayern/Plot_Paper1_final/Fig1.pdf",
#    width = width, height = width)
#
#par(mfrow=c(2,2))
#par(mgp=c(2,0.7,0))

# #a,b,c,d)
# x1 <- xa
# #x1 <- xb
# #x1 <- xc
# x1 <- xd
#
# ## Separate into sum/win season
# x1s <- x1[ x1$month >= 4 & x1$month <= 10 , ]
# x1w <- x1[ x1$month < 4  | x1$month > 10 , ]
#
# AM0 <- MAM(x1,n=7, yearly = TRUE)
# AM <- AM0$MAn
# lmom <- samlmu(AM)
# x.pelwei <- pelwei(lmom); x.pelwei
#
# AM0s <- MAM(x1s,n=7, yearly = TRUE)
# AMs <- AM0s$MAn
# lmoms <- samlmu(AMs)
# x.pelwei.s <- pelwei(lmoms); x.pelwei.s
#
# AM0w <- MAM(x1w,n=7, yearly = TRUE)
# AMw <- AM0w$MAn
# lmomw <- samlmu(AMw)
# x.pelwei.w <- pelwei(lmomw); x.pelwei.w


#Plot layout German (DWA) -> ENGLISH
#evplot(AM, xlab = "-ln(-ln(P(x)))", ylab = "Annual low flow (m³/s)", rp.axis = FALSE, type="n")
#return.scale.ENG()
#freq.axis()
#ev_plot_combined()

#dev.off()

#######################

# Obsolete: return.scale ampassen
# #return.scale.ENG.1<- return.scale.ENG
# fix("return.scale.ENG.1") # set cex=0.8 (Zeile 12)
# #return.scale.ENG.ZOOM.1<- return.scale.ENG.ZOOM
# fix("return.scale.ENG.ZOOM.1") # set cex=0.8 (Zeile 21)


###################
## Combined plot
## Fritz SI ASJ
## Figure_MixedDist
###################

#width <- 12
#width <- 10
width <- 9.5
x11(width = width, height = width/2)

# For saving as pdf, use alternatively:
##pdf(file="C:/Users/Gregor/Seafile/Meine Bibliothek/Institut/Laaha/ASJ-SI Fritz Leisch/Figure_MixedDist.pdf", width = width, height = width/2)
#pdf(file = file.path(plot.path, "Figure_MixedDist.pdf", width = width, height = width/2)

# General plot Layout: 2 horizontal panels
par(mfrow=c(1,2))
par(oma=c(0.5,0.5,2,0.01), mar=c(4,4,0.1,0.1))

# Zoomed evplot for Panel a
#b) -> a)
par(mgp=c(2.2,1,0))
# Prepare AM series
#x1 <- xa
AM_list <- seasAM_FUN(xa)
AM <- AM_list$AM
# empty plot
evplot(AM, xlab = "-ln(-ln(P(x)))", ylab = "Annual low flow (m³/s)", rp.axis = FALSE, type="n", plim=c(0.001, 0.5), ylim=c(min(AM)-((median(AM)-min(AM))/2),median(AM)))
return.scale.ENG.ZOOM.1()
# draw the plot
ev_plot_combined(AM_list)     # Mixed distribution approach
# ev_plot_combined_COP(xa) # Mixed copula estimator
mtext("a)", 3, adj=0, line = 0.5)

# Complete evplot for Panel b
#d)
par(mar=c(4,3,0.1,1.1))
# Prepare AM series
AM_list <- seasAM_FUN(xd)
AM <- AM_list$AM
# empty plot
evplot(AM, xlab = "-ln(-ln(P(x)))", ylab = "", rp.axis = FALSE, type="n")
return.scale.ENG.1()
# draw the plot
ev_plot_combined(AM_list)     # Mixed distribution approach
# ev_plot_combined_COP(xd) # Mixed copula estimator
mtext("b)", 3, adj=0, line = 0.5)

# And finally close the plot (when plotting a pdf)
dev.off()


