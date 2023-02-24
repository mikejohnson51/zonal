geometric_mean <- function(x, coverage_fraction) {
  # https://www.wwdmag.com/channel/casestudies/handling-zeros-geometric-mean-calculation
  # replace negative numbers with 0
  prod(pmax(x, 0) + 1^coverage_fraction)^(1/sum(coverage_fraction)) - 1 
}

circular_mean <- function (x, coverage_fraction) {
  
  degrad = pi / 180 
  
  sinr <- sum(sin(x * degrad), na.rm = TRUE)
  
  cosr <- sum(cos(x * degrad), na.rm = TRUE)
  
  val = atan2(sinr, cosr) * (1 / degrad)
  
  ifelse(val < 0, 180 + (val + 180), val)
}

binned_json = function(x = NULL, coverage_fraction = NULL, bins = 10){
  x1 = x*coverage_fraction
  tmp = as.data.frame(table(cut(pmax(0,x1), breaks = bins)))
  tmp$v = as.numeric(gsub("]", "", sub('.*,\\s*', '', tmp$Var1)))
  tmp$frequency = tmp$Freq / sum(tmp$Freq)
  as.character(toJSON(tmp[,c("v", "frequency")]))
}