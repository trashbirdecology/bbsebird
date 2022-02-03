#' @title Cast Data to Array or Matrix for Use in JAGS
#' @param df Data frame with three or more columns
#' @param row target rownames
#' @param col target colnames
#' @param slice target slice-by values
#' @param val target value to populate the matri(ces)
#' @param drop.na.rows logical if TRUE will drop the values of arg "val" where is NA.
#' @importFrom dplyr distinct
#' @importFrom reshape2 acast
#' @importFrom stringr strsplit
#' @export make_array
make_array <- function(df = bbs,
                       val,
                       row = c("rteno", "checklist_id"),
                       col = "year",
                       slice = "gridcellid",
                       drop.na.rows = FALSE) {
  row <- row[row %in% names(df)]
  if (length(row) == 0)
    stop("no arguments in `row` were found as variables in `df`")


  keep <- c(row, col, slice, val, NA, "NULL", "NA", NULL)
  keep <-
    na.omit(keep[!grepl(paste0(c("NULL", "NA", NA, NULL), collapse = "|"), keep)])
  df <- df[, keep]
  df <- dplyr::distinct(df) %>% arrange(slice, row, col)

  expr <- paste(stringr::strsplit(names(df)[-ncol(df)], split = "#"),
                collapse = "~",
                sep = "#")

  if (drop.na.rows)
    df <- df %>% na.omit(row)

  df.new <- reshape2::acast(df, eval(parse(text = expr)), value.var = val)


  return(df.new)

}
