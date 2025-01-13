seasAM_FUN <- function(x1){
  ## Separate into sum/win season
  x1s <- x1[ x1$month >= 4 & x1$month <= 10 , ]
  x1w <- x1[ x1$month < 4  | x1$month > 10 , ]

  AM0 <- MAM(x1,n=7, yearly = TRUE)
  AM <- AM0$MAn
  #lmom <- samlmu(AM)
  #x.pelwei <- pelwei(lmom); x.pelwei

  AM0s <- MAM(x1s,n=7, yearly = TRUE)
  AMs <- AM0s$MAn
  #lmoms <- samlmu(AMs)
  #x.pelwei.s <- pelwei(lmoms); x.pelwei.s

  AM0w <- MAM(x1w,n=7, yearly = TRUE)
  AMw <- AM0w$MAn
  #lmomw <- samlmu(AMw)
  #x.pelwei.w <- pelwei(lmomw); x.pelwei.w
  return(list('AM'=AM, 'AMs'=AMs, 'AMw'=AMw))
}
