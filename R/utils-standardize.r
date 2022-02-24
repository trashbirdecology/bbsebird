#' @title Center and/or Z-Score Transform a Vector or Array
#' @description If both arguments center and scale are FALSE, function will throw an error to ensure user is aware of this potential mistake.
#' @param x a vector or 2D array of numeric or integer values
#' @param center logical if TRUE will center the resulting object
#' @param scale logical if TRUE data will be returned as z-score transform of x.
#' @export standardize
standardize <-  function (x, center = TRUE, scale = TRUE)
{
  if(!center & !scale) stop("'center' and 'scale' are FALSE. One or both must be TRUE, otherwise this function is unnecessary.")

  if (!is.numeric(x) & !is.integer(x))
    stop("'x' must be a numeric vector or array.", call. = FALSE)

  if (length(center) != 1 | !is.logical(center))
    stop("'center' must be logical or numeric of length 1.",
         call. = FALSE)

  if (length(scale) != 1 | !is.logical(scale))
    stop("'scale' must be logical or numeric of length 1.",
         call. = FALSE)

  if (center) {
    center <- mean(x, na.rm = TRUE)
    x <- x - center
  }


  if (scale) {
      scale <- sd(x, na.rm = TRUE)
      x <- x/scale
  }

    return(x)
}
