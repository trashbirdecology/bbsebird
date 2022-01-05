#' Make 2-D array (Matrix for JAGS)
#'
#' @param dat Data frame
#' @param row Variable in dat containing target row names
#' @param col Variable in dat containig target column names
#' @param val Variable containing the target cell contents
#' @export make_mat

make_mat <- function(dat, row="rteno", col="gridcellid", val) {
  
  names <- names(dat)
  ## will make row and col NULL and then add a thing for when they are NULL for ebird and bbs 
  # e.g. if(is.null(row) & "rteno" %in% names) row <- "rteno"
  
  mat <- tidyr::pivot_wider(dat, 
                            id_cols=row, 
                            names_from=col, 
                            values_from=val) %>% 
    as.matrix()
  # make first col the rownames
  rownames(mat) <- mat[,1]
  # remove it
  mat <- mat[,-1]
  
  return(mat)
  
  }