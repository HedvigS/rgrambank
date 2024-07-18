#' Takes three numeric vectors (columns of a data-frame/matrix) and maps to RGB-values
#'
#' @param x data-frame or matrix. Identifiers should be rownames and the three relevant columns for mapping to RGB should be numeric
#' @param first_three logical. If TRUE then the first three columns are used to map to RGB
#' @param cols = character vector. If first_three is FALSE, provide the names of the three relevant columns here
#' @author Hedvig Skirgård and Damián Blasi
#' @export

match_to_RGB <- function(x = NULL, 
                         first_three = TRUE, 
                         cols = NULL){

  
  if(first_three == FALSE & is.null(cols)){
    stop("first_three is set to FALSE but no columns were provided.")
    }
  
  x <- x %>% 
    as.data.frame()

    if(  !all(cols %in% colnames(x))){
    stop("The relevant cols are not in x.")
  }
  
  if(first_three == TRUE){
    cols = colnames(x[,1:3])
  }else{
    cols <- cols  
    }
  
RGB <- x %>% 
  dplyr::select(all_of(cols) ) %>%
  base::sweep(2, apply(., 2, function(x){ 2 * max(abs(x)) }), "/") %>%
  base::sweep(2, 0.5, "+") %>%
  grDevices::rgb(alpha = 1)

RGB
}