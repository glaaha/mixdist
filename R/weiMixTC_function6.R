#' @export
weiMixT <- function(x1, sortx=FALSE, T=c(100,50, 20))
{

SI <- seasindex(x1)
SR <- seasratio(x1, breakdays=c("01/04", "01/11"))
SR <- as.numeric(SR)

x1s <- x1[ x1$month >= 4 & x1$month <= 10 , ]
x1w <- x1[ x1$month < 4  | x1$month > 10 , ]

AM0 <- MAM(x1,n=7, yearly = TRUE)
if (sortx==TRUE) {AM0 <- AM0[order(AM0$MAn),]}
AM <- AM0$MAn
lmom <- samlmu(AM)
x.pelwei <- pelwei(lmom); x.pelwei
x.p <- cdfwei(AM, para = x.pelwei)

AM0s <- MAM(x1s,n=7, yearly = TRUE)
if (sortx==TRUE) {AM0s <- AM0s[order(AM0s$MAn),]}
AMs <- AM0s$MAn
lmoms <- samlmu(AMs)
x.pelwei.s <- pelwei(lmoms); x.pelwei.s
#x.ps <- cdfwei(AMs, para = x.pelwei.s) # FALSCH ?!
x.ps <- cdfwei(AM, para = x.pelwei.s)

AM0w <- MAM(x1w,n=7, yearly = TRUE)
if (sortx==TRUE) {AM0w <- AM0w[order(AM0w$MAn),]}
AMw <- AM0w$MAn
lmomw <- samlmu(AMw)
x.pelwei.w <- pelwei(lmomw); x.pelwei.w
#x.pw <- cdfwei(AMw, para = x.pelwei.w) # FALSCH ?!
x.pw <- cdfwei(AM, para = x.pelwei.w)

#
AM0sw <- merge(AM0s, AM0w, by = "hyear", all = TRUE)
cor_spearman <- cor.test(AM0sw$MAn.x, AM0sw$MAn.y, method = "spearman")

mixture_rate <- sum(AMs < AMw)/length(AM)

.pobs <- (rank(AM))/(length(AM) + 1) # Weibull prob estimators -> Same as pobs(AM) from package copula!

# Empirical mixed prob (of a heterogeneous distribution)
.pobs.mix <- pobs_mixed_FUN(AM=AM, AMs=AMs, AMw=AMw, plot=FALSE) # $AM, $p.mix and $p.mix.c are returned


# Predicted mixed prob (model)
x.pred.mix <- cdfwei_Mixed(x=AM, y=AM, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), plot=FALSE)

# Predicted mixed prob with copula (model)
x.pred.mixC <- cdfwei_MixedC(x=AM, y=AM, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=FALSE)


# /!\ NEU hier sample + predictions f?r sample werte zusammenstellen!

#sample <- cbind(obs=obs, pA=x.p, TA=1/x.p, pMix=x.pred.mix, TMix=1/x.pred.mix, "pMixC"=p.mix.c, "TMixC"=1/p.mix.c)
#obs <- data.frame(cbind(hyear=AM0$hyear, x=AM, pobs=.pobs, pobs.mix=.pobs.mix$p.mix, pobs.mix=.pobs.mixC$p.mix.c))
sample <- data.frame(hyear=AM0$hyear, x=AM, pobs=.pobs, pobs.mix=.pobs.mix$p.mix, pA=x.p, TA=1/x.p, pMix=x.pred.mix, TMix=1/x.pred.mix, "pMixC"=x.pred.mixC, "TMixC"=1/x.pred.mixC)

#####################
# Analyse: Change in return period when using Mixed distribution
# Frage: Wie ?ndert sich RP von Annual wei distribution wenn ich mit Mixed Distribuion auswerte?
# -) Annual Series (no seasons, common probability distribution)

.f <- 1/T # Hier Probability 1/T
.y <- quawei(.f, para = x.pelwei) # Hier Quantilwerte von T => als Eingang in andere Prob-Berechnungen
.p <- cdfwei(.y, para = x.pelwei)
pred <- data.frame(yo=.y, po=.p, To=1/.p)

# -) summer
.ys <- quawei(.f, para = x.pelwei.s)
.ps <- cdfwei(.y, para = x.pelwei.s)
pred.s <- data.frame(y_of_To=.ys, p=.ps, T=1/.ps)

# -) winter
.yw <- quawei(.f, para = x.pelwei.w)
.pw <- cdfwei(.y, para = x.pelwei.w)
pred.w <- data.frame(y_of_To=.yw, p=.pw, T=1/.pw)

### Mixed distr. NEU2 /!\ #########
.pmix <- cdfwei_Mixed(x=.y, y=.y, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), plot=FALSE, pch="x")

# Ermittle Quantile der Mischverteilung mit p=.f (z.b. p=1/20=0.2)
# Dazu feine Diskretisierung
t.y1 <- par("usr")[3] ; t.y2 <- par("usr")[4] ### wie ohne plot berechnen???
t.y.qua <- seq(round(t.y1, 1), round(t.y2, 1), by=0.01)
t.pmix <- cdfwei_Mixed(x=t.y.qua, y=t.y.qua, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), plot=FALSE, pch="x")
# Nun Suche an der y-Stelle der Verteilung wo x=.pmix=.f ist
# ... d.h, gr??er Wert mit p <= .f
f.qmix <- vector()
for (i in 1:length(.f)){
  f.qmix[i] <- max(t.y.qua[t.pmix <= .f[i]]) ### better vectorize ...
}

pred.mix <- data.frame(y_of_To=f.qmix, p=.pmix, T=1/.pmix)



### Copula estimator ####
.pmixC <- cdfwei_MixedC(x=.y, y=.y, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=FALSE)

# Ermittle Quantile der Mischverteilung mit p=.f (z.b. p=1/20=0.2)
# Dazu feine Diskretisierung
t.y1 <- par("usr")[3] ; t.y2 <- par("usr")[4] ### wie ohne plot berechnen???
t.y.qua <- seq(round(t.y1, 1), round(t.y2, 1), by=0.01)
t.pmixC <- cdfwei_MixedC(x=t.y.qua, y=t.y.qua, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=FALSE, pch="x")
# Nun Suche an der y-Stelle der Verteilung wo x=.pmix=.f ist
# ... d.h, gr??er Wert mit p <= .f
f.qmixC <- vector()
for (i in 1:length(.f)){
  f.qmixC[i] <- max(t.y.qua[t.pmixC <= .f[i]]) ### better vectorize ...
}

pred.mixC <- data.frame(y_of_To=f.qmixC, p=.pmixC, T=1/.pmixC)

To_events <- data.frame("To"=1/.p, "po"=.p, "Tmix"=1/.pmix , "pmix"=.pmix , "TmixC"=1/.pmixC , "pmixC"=.pmixC , "Ts"=1/.ps, "ps"=.ps, "Tw"=1/.pw, "pw"=.pw)


# Return
#return(list("sample"=sample, "pred"=pred, "pred.s"=pred.s, "pred.w"=pred.w, "pred.mix"=pred.mix, "pred.mixC"=pred.mixC, "SI"=SI, "SR"=SR))
return(list("sample"=sample, "To_events"=To_events, "SI"=SI, "SR"=SR, "Rho"=cor_spearman$estimate[[1]], "Rho_p-value"=cor_spearman$p.value, "mixture_rate"=mixture_rate))
}

# Anwendung

# weiMixT(x1)

# REM: Plotting the Copula probability
# p.mix.c1 <- -log(-log(weiMixT(x1)$sample$pMixC))
# points(p.mix.c1, AM, col="black", pch="+")
