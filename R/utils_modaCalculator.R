#' modaCalculator 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

modafuntion <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

make_factors <- function(data, max_levels,min_levels) {
  # convert all columns in <data> that are not already factors
  # and that have fewer than <max_levels> distinct values into factors. 
  # If the column is numeric, it becomes an ordered factor.
  
  stopifnot(is.data.frame(data))
  for(n in names(data)){
    
   
    if(!is.factor(data[[n]]) && (length(unique(data[[n]])) <= min_levels) ){
      data[[n]] <- as.factor(data[[n]]) }
    
    if(!is.factor(data[[n]]) && (length(unique(data[[n]])) >= min_levels && length(unique(data[[n]])) <= max_levels) ){
      #print_dev("Ordered")
      #print_dev(unique(data[[n]]))
      data[[n]] <- ordered(data[[n]], levels = unique(data[[n]])) }
    
   
  }
  data
}