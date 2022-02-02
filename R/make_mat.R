#' Make 2-D array (Matrix for JAGS)
#'
#' @param df.in Data frame
#' @param row Variable in df containing target row names
#' @param col Variable in df containig target column names
#' @param val Variable containing the target cell contents
#' @param replace.na Whether to replace NA values with zero.
#' @export make_mat

make_mat <- function(df.in, row="site.ind", col="year.ind", val, replace.na=FALSE) {

  # names <- names(df)
  ## will make row and col NULL and then add a thing for when they are NULL for ebird and bbs
  # e.g. if(is.null(row) & "rteno" %in% names) row <- "rteno"
  mat <- tidyr::pivot_wider(df.in,
                            id_cols = row,
                            names_from = col,
                            values_from = val) %>%
                as.data.frame() ## add this instead of matrix otherwise numeric cell values --> character
  # make first col the rownames

  # remove missing values in first column, which will be the rteno/checklist_id
  mat <- mat[!is.na(mat[,1]),]

  ## sort matrix
  mat <- mat %>%
    arrange(mat[row]) %>%
    tibble::column_to_rownames(var=row)

  ## replace nulls with NA
  mat[mat=="NULL"] <- NA
  # if replace.na is TRUE, then supply NAs with zeroes
  if(replace.na) mat[is.na(mat)] <- 0

  #ensure matrix is sorted by rownames and colnames
  ## sort column names as integers
  mat <- mat %>% dplyr::select(order(as.integer(colnames(mat))))#cols

  #### crude tests
  # stopifnot(rownames(mat)==sort(as.integer(rownames(df.in))))
  # stopifnot(colnames(mat)==sort(unique(df.in$year.ind)))

  # return object
  return(mat)

  }
