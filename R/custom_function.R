#' Geometric Mean Summary
#' @param x vector of values
#' @param coverage_fraction coverage fraction
#' @return data.frame
#' @export

geometric_mean <- function(x, coverage_fraction) {
  data = x * coverage_fraction
  data = data[!is.na(data)]
  exp(mean(log(data[data>0])))
}

#' Geometric Mean Summary
#' @param x vector of values
#' @param coverage_fraction coverage fraction
#' @return data.frame
#' @export

circular_mean <- function (x, coverage_fraction) {
  
  degrad = pi / 180 
  
  sinr <- sum(sin(x * degrad), na.rm = TRUE)
  
  cosr <- sum(cos(x * degrad), na.rm = TRUE)
  
  val = atan2(sinr, cosr) * (1 / degrad)
  
  ifelse(val < 0, 180 + (val + 180), val)
}


#' Geometric Mean Summary
#' @param x vector of values
#' @param coverage_fraction coverage fraction
#' @param bins number of bins to compute (default 10)
#' @return data.frame
#' @export

binned_json = function(x = NULL, coverage_fraction = NULL, bins = 10){
  x1 = x*coverage_fraction
  tmp = as.data.frame(table(cut(pmax(0,x1), breaks = bins)))
  tmp$v = as.numeric(gsub("]", "", sub('.*,\\s*', '', tmp$Var1)))
  tmp$frequency = tmp$Freq / sum(tmp$Freq)
  as.character(toJSON(tmp[,c("v", "frequency")]))
}