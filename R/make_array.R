#' @title Cast Data to Array or Matrix for Use in JAGS
#' @param df Data frame with three or more columns
#' @param row target rownames
#' @param col target colnames
#' @param slice target slice-by values
#' @param val target value to populate the matri(ces)
#' @param include.na.vals TRUE default. If FALSE will drop the values of arg "val" where is NA.
#' @importFrom dplyr distinct
#' @importFrom reshape2 "acast"
#' @export make_array
make_array <- function(df=bbs,
                     row="rteno",
                     col="year",
                     slice="gridcellid",
                     val,
                     drop.na.rows = FALSE
                     ){
  keep <- c(row, col, slice, val, NA, "NULL", "NA", NULL)
  keep <- na.omit(keep[!grepl(paste0(c("NULL","NA", NA, NULL), collapse = "|"), keep)])
  df <- df[,keep]
  df <- dplyr::distinct(df) %>% arrange(slice, row, col)

  expr <- paste(strsplit(names(df)[-ncol(df)], split = "#"),
           collapse = "~",
           sep="#")

  if(drop.na.rows) df <- df %>% na.omit(row)

  df.new <- acast(df, eval(parse(text=expr)), value.var = val)


  return(df.new)

}
