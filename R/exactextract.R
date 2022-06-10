#' Execute Zonal Stats by ExactExtract
#' @description  execute exactextract more flexiably
#' @param data SpatRaster or file path
#' @param geom summary units
#' @param ID the grouping ID 
#' @param fun summarization function
#' @param subds subdatasets to extract
#' @param na.rm should NA values be removed?
#' @param progress if TRUE, display a progress bar during processing
#' @return data.table
#' @export
#' @importFrom collapse collap
#' @importFrom methods formalArgs

zone_by_ee = function(data, geom, ID, fun = "mean", subds = NULL, na.rm = TRUE, progress = FALSE){
  
  data = prep_input(data, subds = subds)
  
  ee = FALSE
  
  if(class(fun) == "function"){
    fun = fun
  } else if(fun %in% ee_functions()){
    ee = TRUE
    fun = ee_functions()[which(fun == ee_functions())]
    collapse = TRUE
  } else {
    fun = fun
  }
  
  if(ee){
    suppressWarnings({
      exe     = exact_extract(data, 
                             geom,
                             fun = fun,
                             progress = progress,
                             append_cols = ID)
    })
  } else {
    suppressWarnings({
      exe     = exact_extract(data, 
                             geom,
                             fun = function(df, ...){
                               data.frame(lapply(df, 
                                                 fun, 
                                                 coverage_fraction = df$coverage_fraction))
                             },
                             summarize_df = TRUE, 
                             progress = progress,
                             append_cols = ID)
    })
    
  }
 
  sanitize(exe)
}