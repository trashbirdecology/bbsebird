#' Source Select Chunks Inside Another R Markdown File
#'
#' This function was borrowed from https://gist.githubusercontent.com/brshallo/e963b9dca5e4e1ab12ec6348b135362e/raw/71ad2b7288dc6e4f277f53f00e650d98a8e54aff/source-rmd-chunks.r
#' @param file Rmarkdown filename to hit up
#' @param chunk_labels chunk labels to run
#'
#' @export

source_rmd_chunks <- function(file, chunk_labels, skip_plots = TRUE, output_temp = FALSE){
  temp <- tempfile(fileext=".R")

  knitr::purl(file, output = temp)

  text <- readr::read_file(temp)

  text <- purrr::map(chunk_labels, ~stringr::str_extract(text, glue::glue("(## ----{var})(.|[:space:])*?(?=(## ----)|$)", var = .x))) %>%
    stringr::str_c(collapse = "\n")

  readr::write_file(text, temp)

  if(skip_plots) {
    old_dev = getOption('device')
    options(device = function(...) {
      .Call("R_GD_nullDevice", PACKAGE = "grDevices")
    })
  }

  source(temp)

  if(skip_plots) {
    options(device = old_dev)
  }

  if(output_temp) temp
}
