match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = TRUE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))
  
  for (i in setdiff(names(formals), names(call)))
    call[i] <- list(formals[[i]])
  
  match.call(sys.function(sys.parent()), call)
}

#' Execute Zonal Stats by ExactExtract
#' @description  execute exactextract more flexiably
#' @param data SpatRaster or file path
#' @param geom summary units
#' @param ID the grouping ID
#' @param fun summarization function
#' @param subds subdatasets to extract
#' @param progress if TRUE, display a progress bar during processing
#' @param join should data be joined to geom?
#' @param ... additional arguments passed on to exact_extract
#' @return data.table
#' @export

zone_by_ee = function(data,
                      geom,
                      ID,
                      fun = "mean",
                      subds = 0,
                      progress = FALSE,
                      join = TRUE, 
                      ...) {
  
  data = prep_input(data, subds = subds, lyrs = NULL, win = ext(project(vect(geom), crs(data))))
  
  suppressWarnings({
      exe     = exact_extract(
        x = data,
        y = geom,
        fun = fun,
        progress = progress,
        append_cols = ID,
        stack_apply = TRUE,
        ...
      )
  })
  
  if(inherits(fun, "function")){
    n = "fun"
  } else {
    n = fun
  }
  
  names(exe) = c(ID, paste(n, names(data), sep = "."))
  
  exe

}
  