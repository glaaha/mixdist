
cdfwei_Mixed <- function (x, y, para.x, para.y, plot=FALSE, ...) {
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
  #
  Fx <- ifelse(x <= para.x[1], 0,   1 - exp(-((x - para.x[1])/para.x[2])^para.x[3]))
  Fy <- ifelse(y <= para.y[1], 0,   1 - exp(-((x - para.y[1])/para.y[2])^para.y[3]))
  Fxy <- 1-((1-Fx) * (1-Fy))
  # Plotting:
  if (plot==TRUE) {
    if (any(x != y))
      stop("plot not meaningful for x != y")
    Fxy1 <- -log(-log(Fxy))
    lines(Fxy1, x, ...)
  }
  return(Fxy)
}

# Anwendung
#cdfwei_Mixed(x=AM, y=AM, para.x = pelwei(samlmu(AMs)), para.y = pelwei(samlmu(AMw)), plot=TRUE)


# Anmerkung
# Macht Berechnung überhaupt Sinn wenn x =! y ???
