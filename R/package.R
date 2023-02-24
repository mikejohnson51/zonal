#' Weight Function Lookup
#' @return data.frame
#' @export

weight_functions = function(){
  data.frame(collapse = .FAST_STAT_FUN, base = substring(.FAST_STAT_FUN, 2))
}

#' exactextractr Function Lookup
#' @return  data.frame
#' @export

ee_functions = function(){
  c("min", "max", "count", "sum", 
    "mean", "median", "quantile", "mode", 
    "majority", "minority", "variety", "variance", 
    "stdev", "coefficient_of_variation", "weighted_mean", 
    'weighted_sum', 'frac', 'weighted_frac')
}


#' @importFrom jsonlite toJSON
#' @importFrom collapse collap
#' @importFrom methods formalArgs
#' @importFrom collapse .FAST_STAT_FUN collap
#' @importFrom terra rast xyFromCell extract
#' @importFrom exactextractr exact_extract
#' @importFrom data.table rbindlist
#' @importFrom stats as.formula
NULL