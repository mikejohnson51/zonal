#' Geometric Mean Summary
#' @param values vector of values
#' @param coverage_fraction coverage fraction
#' @return data.frame
#' @export

geometric_mean <- function(x, coverage_fraction) {
  data = x * coverage_fraction
  data = data[!is.na(data)]
  exp(mean(log(data[data>0])))
}

#' Geometric Mean Summary
#' @param values vector of values
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
#' @param values vector of values
#' @param coverage_fraction coverage fraction
#' @param breaks either a numeric vector of two or more unique cut points or a single number 
#' (greater than or equal to 2) giving the number of intervals into which x is to be cut. (default = 10)
#' @param constrain should breaks (with length > 1) be limited by the max of values?
#' @return data.frame
#' @export

distribution = function(values, coverage_fraction, breaks = 10, constrain = FALSE){

  x1 = values*coverage_fraction

  if(constrain | length(breaks) > 1){
    breaks = breaks[which( breaks > max(values))]
  }
  
  tmp = as.data.frame(table(cut(x1, breaks = breaks)))
    
  tmp$v = as.numeric(gsub("]", "", sub('.*,\\s*', '', tmp$Var1)))
  
  tmp$frequency = tmp$Freq / sum(tmp$Freq)

  as.character(toJSON(tmp[,c("v", "frequency")]))
    
}


#' Equal Area Distribution
#' @param values vector of values
#' @param coverage_fraction coverage fraction
#' @param groups number of intervals to create
#' @return data.frame
#' @export

equal_population_distribution = function(values, coverage_fraction, groups = 4){
  
  x1 = values*coverage_fraction

  tmp = as.data.frame(table(chop_equally(x1, groups = groups)))
  
  tmp$v = as.numeric(gsub(")", "", gsub("]", "", sub('.*,\\s*', '', tmp$Var1))))
  
  tmp$frequency = tmp$Freq / sum(tmp$Freq)
  
  as.character(toJSON(tmp[,c("v", "frequency")]))
  
}


