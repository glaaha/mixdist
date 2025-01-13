#' @export
mixture_rate_FUN <- function(x1) {

  x1s <- x1[ x1$month >= 4 & x1$month <= 10 , ]
  x1w <- x1[ x1$month < 4  | x1$month > 10 , ]

  #AM0 <- MAM(x1,n=7, yearly = TRUE)
  #if (sortx==TRUE) {AM0 <- AM0[order(AM0$MAn),]}
  #AM <- AM0$

  AM0s <- MAM(x1s,n=7, yearly = TRUE)
  #if (sortx==TRUE) {AM0s <- AM0s[order(AM0s$MAn),]}
  AMs <- AM0s$MAn

  AM0w <- MAM(x1w,n=7, yearly = TRUE)
  #if (sortx==TRUE) {AM0w <- AM0w[order(AM0w$MAn),]}
  AMw <- AM0w$MAn

  AM0sw <- merge(AM0s, AM0w, by = "hyear", all = TRUE)
  mixture_rate <- sum(AM0sw$MAn.x < AM0sw$MAn.y)/nrow(AM0sw)

}


# Anwendung:

#head(tt.abs2)
#mixture_rate_LUT <- data.frame("HZBNR"=tt.abs2$HZBNR, "mixture.rate"=NA)

# .pgi=23
# for (.pgi in 1:length(names(stations))) {
#   #for (.pgi in set_1) {
#   cat(paste(.pgi, " "))
#   #if (is.element(.pgi, exclude)) next
#   .pg.name <- names(stations[.pgi])
#   x0 <- stations[[.pgi]]
#   x2 <- x0[x0$year>=1950,]
#   x1 <- x2[-(1:90),]
#
#   mixture_rate_LUT[.pgi,2] <- mixture_rate_FUN(x1)
# }


