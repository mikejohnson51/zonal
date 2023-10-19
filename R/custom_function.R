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

circular_mean <- function (values, coverage_fraction) {
  
  degrad = pi / 180 
  
  sinr <- sum(sin(values * degrad), na.rm = TRUE)
  
  cosr <- sum(cos(values * degrad), na.rm = TRUE)
  
  val = atan2(sinr, cosr) * (1 / degrad)
  
  ifelse(val < 0, 180 + (val + 180), val)
}


#' Distribution Summary
#' @param x vector of values
#' @param coverage_fraction coverage fraction
#' @param breaks either a numeric vector of two or more unique cut points or a single number 
#' (greater than or equal to 2) giving the number of intervals into which x is to be cut. (default = 10)
#' @return data.frame
#' @export

distribution = function(values, coverage_fraction, breaks = 10){
  
  x1 = values*coverage_fraction
  
  tmp = as.data.frame(table(cut(pmax(0,x1), 
                                  breaks = breaks)))
    
  tmp$v = as.numeric(gsub("]", "", sub('.*,\\s*', '', tmp$Var1)))
  
  tmp$frequency = tmp$Freq / sum(tmp$Freq)

  as.character(toJSON(tmp[,c("v", "frequency")]))
    
}


