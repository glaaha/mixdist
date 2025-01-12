
cdfwei_MixedC <- function (x, y, para.x, para.y, plot=FALSE, x1, CopFamily="gumbelCopula", hyearstart=4, seas2start=11, ...) {
#cdfwei_MixedC <- function (x, y, para.x, para.y, plot=FALSE, ...) {
#
  if (length(para.x) != 3) 
    stop("parameter vector has wrong length")
  if (any(is.na(para.x))) 
    stop("missing values in parameter vector")
  if (para.x[2] <= 0 || para.x[3] <= 0) 
    stop("distribution parameters of Fx invalid")
  #
  if (length(para.y) != 3) 
    stop("parameter vector has wrong length")
  if (any(is.na(para.y))) 
    stop("missing values in parameter vector")
  if (para.y[2] <= 0 || para.y[3] <= 0) 
    stop("distribution parameters of Fx invalid")
  if (CopFamily != "gumbelCopula" & CopFamily !="indepCopula") 
    stop("CopFamily must be 'gunbelCopula' or indepCopula'")
  #
  if (hyearstart < 1 || hyearstart > 12) 
    stop("hyearstart must be between 1 and 12 (default 4)")
  if (seas2start < 1 || seas2start > 12) 
    stop("start of second season 'seas2start' must be between 1 and 12 (default 11)")
  if (seas2start <= hyearstart) 
    stop("start of second season 'seas2start' must be greater than 'hyearstart'")
  #
  # Copula estimator
  #x1s <- x1[ x1$month >= 4 & x1$month <= 10 , ]   # Aus x1, also den Stationsdaten !!
  #x1w <- x1[ x1$month < 4  | x1$month > 10 , ]
  x1s <- x1[ x1$month >= hyearstart & x1$month < seas2start, ]    # Update 21.07.2022 (f?r EU, aber nach Ergebnisanalysen...)
  x1w <- x1[ x1$month < hyearstart  | x1$month >= seas2start, ]     # REM: Winter immer 1. Nov !
  AMs <- MAM(x1s,n=7, yearly = TRUE)$MAn
  AMw <- MAM(x1w,n=7, yearly = TRUE)$MAn
  ##/X1 <- pobs(as.matrix(cbind(AMs, AMw)))
  u1 <- pobs(as.matrix(cbind(AMs, AMw)))
  
  ##/fit_copula <- fitCopula(gumbelCopula(dim=2), data=X1, method = c("ml"))
  fit_copula <- fitCopula(gumbelCopula(dim=2), data=u1, method = c("ml"))
  gum_copula  <- gumbelCopula(param = coef(fit_copula))
  #gum_copula  <- gumbelCopula(param = 1)
  Fs.list <- list(shape=para.x[3],scale=para.x[2],thres=para.x[1]) # 1st marginal - summer
  Fw.list <- list(shape=para.y[3],scale=para.y[2],thres=para.y[1]) # 2nd marginal - winter
  paramMargins = list(Fs.list, Fw.list)
  ##/u <- as.matrix(cbind(pobs(x), pobs(y)))
  q_mat <- as.matrix(cbind(x, y)) ##/!\ ACHTUNG: keine pobs !!!
  my_mvdc <- mvdc(gum_copula, margins = c("weibull3","weibull3"), paramMargins = paramMargins)
  ##/x.pMvd <- pMvdc(u, my_mvdc)
  x_pMvdc <- pMvdc(x=q_mat, my_mvdc)
  
  
  # /!\ NEU ... various copula families
  if (CopFamily=="indepCopula") {
  #fit_copula <- fitCopula(gumbelCopula(dim=2), data=X1, method = c("ml"))
  #gum_copula  <- gumbelCopula(param = coef(fit_copula))
  gum_copula <- indepCopula(dim=2)
  Fs.list <- list(shape=para.x[3],scale=para.x[2],thres=para.x[1]) # 1st marginal - summer
  Fw.list <- list(shape=para.y[3],scale=para.y[2],thres=para.y[1]) # 2nd marginal - winter
  paramMargins = list(Fs.list, Fw.list)
  #u <- as.matrix(cbind(pobs(x), pobs(y)))
  my_mvdc <- mvdc(gum_copula, margins = c("weibull3","weibull3"), paramMargins = paramMargins)
  x_pMvdc <- pMvdc(x=q_mat, my_mvdc)
  }
  
  Fx <- ifelse(x <= para.x[1], 0,   1 - exp(-((x - para.x[1])/para.x[2])^para.x[3]))
  Fy <- ifelse(y <= para.y[1], 0,   1 - exp(-((x - para.y[1])/para.y[2])^para.y[3]))
  #FxyC  <- Fx + Fy - x_pMvdc*Fx*Fy  # ACHTUNG - letzter Term !!!
  FxyC_1  <- Fx + Fy - x_pMvdc*Fx*Fy  # ACHTUNG - letzter Term !!!
  FxyC_2  <- Fx + Fy - x_pMvdc  # Lt. Literatur - AND Verkn?pfung
  #FxyC_3  <- x_pMvdc
  FxyC_4  <- 1- (1 - Fx - Fy + x_pMvdc)  # Lt. Literatur - AND Verkn?pfung
  FxyC_5  <- Fx + Fy - Fx*Fy  # Formula for independence - geht bei .pg = 205229 (und entspricht Fmix Formel!)
  
  
  FxyC <- FxyC_2
  
  # Plotting: 
  if (plot==TRUE) {
    if (any(x != y)) 
      stop("plot not meaningful for x != y")
    FxyC1 <- -log(-log(FxyC))
    #points(FxyC1, x, ...)
    lines(FxyC1, x, ...)
  }
  return(FxyC)
  #return(round(cbind(Fx, Fy, Fmix=FxyC_5, FxyC_4, "Fx.Fy"=Fx*Fy, "x_pMvdc.Fx.Fy"=x_pMvdc*Fx*Fy, "diff"=Fx*Fy-x_pMvdc*Fx*Fy, x_pMvdc, FxyC_1, FxyC_2), 3))
  #return(round(cbind(Fx, Fy, Fmix=FxyC_5, FxyC_2, diff=FxyC_5-FxyC_2), 3))
  
}

# Anwendung
# cdfwei_MixedC(x=sort(AM), y=sort(AM), para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=TRUE, CopFamily="indepCopula")
# cdfwei_MixedC(x=sort(AM), y=sort(AM), para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=TRUE)
# cdfwei_MixedC(x=sort(AM), y=sort(AM), para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=TRUE, hyearstart = 1)

#cdfwei_MixedC(x=AM, y=AM, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1, plot=TRUE)
#cdfwei_MixedC(x=AM, y=AM, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), x1=x1)


# Anmerkung
# Macht Berechnung ?berhaupt Sinn wenn x =! y ???





