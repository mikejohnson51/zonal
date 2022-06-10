#' Weight Function Lookup
#' @return data.frame
#' @export
#' @importFrom collapse .FAST_STAT_FUN

weight_functions = function(){
  data.frame(collapse = .FAST_STAT_FUN, base = substring(.FAST_STAT_FUN, 2))
}

#' exactextractr Function Lookup
#' @return  data.frame
#' @export
#' @importFrom collapse .FAST_STAT_FUN

ee_functions = function(){
  c("min", "max", "count", "sum", 
    "mean", "median", "quantile", "mode", 
    "majority", "minority", "variety", "variance", 
    "stdev", "coefficient_of_variation", "weighted_mean", 
    'weighted_sum', 'frac', 'weighted_frac')
}
