#' Make 2-D array (Matrix for JAGS)
#'
#' @param dat Data frame
#' @param row Variable in dat containing target row names
#' @param col Variable in dat containig target column names
#' @param val Variable containing the target cell contents
#' @param replace.na Whether to replace NA values with zero.
#' @export make_mat

make_mat <- function(dat, row="rteno", col="gridcellid", val, replace.na=FALSE) {

  # names <- names(dat)
  ## will make row and col NULL and then add a thing for when they are NULL for ebird and bbs
  # e.g. if(is.null(row) & "rteno" %in% names) row <- "rteno"
  mat <- tidyr::pivot_wider(dat,
                            id_cols=row,
                            names_from=col,
                            values_from=val) %>%
    as.data.frame() ## add this instead of matrix otherwise numeric cell values --> character
  # make first col the rownames
  # View(mat)

  # remove missing values in first column, which will be the rteno/checklist_id
  mat <- mat[!is.na(mat[,1]),]
  
  rownames(mat) <- mat[,1]
  # remove it
  mat <- mat[,-1]


  # if replace.na is TRUE, then supply NAs with zeroes
  if(replace.na) mat[is.na(mat)] <- 0
  return(mat)

  }
