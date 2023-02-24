match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = TRUE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))
  
  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )
  
  match.call(sys.function(sys.parent()), call)
}

#' Execute Zonal Stats by ExactExtract
#' @description  execute exactextract more flexiably
#' @param data SpatRaster or file path
#' @param geom summary units
#' @param ID the grouping ID 
#' @param fun summarization function
#' @param subds subdatasets to extract
#' @param na.rm should NA values be removed?
#' @param progress if TRUE, display a progress bar during processing
#' @param extra extra arguments to be passed to fun
#' @return data.table
#' @export

zone_by_ee = function(data, geom, ID, fun = "mean", subds = NULL, na.rm = TRUE, progress = FALSE, extra = NULL){
  
  data = prep_input(data, subds = subds)
  
  ee = FALSE
  
  if(inherits(fun, "function")){
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

    x = unlist(extra)
    suppressWarnings({
      exe     = exact_extract(data, 
                              geom,
                              fun = function(df, ...){
                               data.frame(lapply(df, 
                                                 fun, 
                                                 coverage_fraction = df$coverage_fraction,
                                                 x))
                             },
                             summarize_df = TRUE, 
                             progress = progress,
                             append_cols = ID)
    })
    
  }
 
  sanitize(exe)
}
