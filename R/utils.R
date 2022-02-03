#' @param ebd_zf The zero-filled ebird data object (flat)
#' @keywords internal
#' @noRd
clean_ebird_colnames <- function(df) {
  clean_df <- df %>%
    mutate(
      # convert X to NA
      observation_count = as.integer(observation_count),
      # effort_distance_km to 0 for non-travelling counts
      effort_distance_km = if_else(protocol_type != "Traveling",
                                   0, effort_distance_km)
    ) %>%
    rename(c = observation_count)

  clean_df <- clean_df %>% mutate(
    # convert time to decimal hours since midnight
    time_observations_started_hsm = lubridate::hms(time_observations_started),
    # split date into year and day of year
    year = lubridate::year(observation_date),
    yday = lubridate::yday(observation_date)
  )

  return(clean_df)

}



#' Munge eBird and BBS Column Names
#'
#' Munge the names of eBird and BBS data frames to ensure consistency.
#'
#' @param x a data frame or matrix of eBird or BBS observations. If no column names match function target, will return same object.
#' @keywords internal
match_col_names <- function(x) {
  names(x) <- tolower(names(x))

  col_names <- list(
    date = c("observation_date", "date"),
    C =    c("observation_count", "count", "routetotal"),
    yday  = c("dayofyear"),
    lat  = c("lati", "latitude"),
    lon = c("longitude", "long")
  )

  for (i in seq_along(col_names)) {
    newname = names(col_names)[i]
    oldnames = col_names[[i]]

    toreplace = names(x)[which(names(x) %in% oldnames)]
    x <- x %>%
      rename_with( ~ newname, all_of(toreplace))
  }
  return(x)

}


#' @title Convert Column Types and Names
#' @description Converts columns classes based to ensure proper integration of eBird and BBS data
#' @param x The data frame with one or more columns.
#' @keywords internal
#' @noRd
convert_cols <- function(x) {
  ## numeric
  num <- c(
    "duration_minutes",
    "effort_area_ha",
    "effort_distance_km",
    "latitude",
    "longitude"
  )
  ## integer
  ints <- c("all_species_reported",
            "number_observers",
            "observation_count")

  ## date/time
  dates <- c("observation_date",
             "date")
  # times <- c("time_observations_started")
  ## characters
  chrs <- c(
    "country",
    "common_name",
    "country_code",
    "county",
    "county_code",
    "group_identifier",
    "observer_id",
    "protocol_code",
    "protocol_type",
    "sampling_event_identifier",
    "scientific_name",
    "state",
    "state_code",
    "time_observations_started"
  )


  x <- x %>%
    mutate(across(any_of(c(chrs, num, ints)), as.character)) %>%
    mutate(across(any_of(num), as.numeric)) %>%
    mutate(across(any_of(ints), as.integer)) %>%
    mutate(across(any_of(dates), as.Date))

  return(x)


}

#' split_table internal function
#'
#' @param tibble the flat data object
#' @param col the name(s) of the column(s) used to splice the table into a list
#' @noRd
split_tibble <-
  function(tibble, col = 'col')
    tibble %>% split(., .[, col])


#' Standard Error
#'
#' Computes standard error of a vector
#' @param x Numeric vector
#' @examples
#' x <- rnorm(100)
#' se(x)
#' @keywords internal
#' @noRd
se <- function(x) {
  sd(x) / sqrt(length(x))
}

#' Force a AOU Numbers into Integer
#' Used very specifically as an internal function when munging BBS data.
#' @keywords internal
#' @noRd
make.integer <- function(x, var = c("AOU", "aou")) {
  for (i in seq_along(var)) {
    ind = var[i]
    if (ind %in% names(x)) {
      x = as.data.frame(x)
      x[, paste(ind)] = as.integer(dplyr::pull(x, ind))
    } else{
      return(x)
    }
  }
}



#' Keep Only Wanted Objects in Memory
#'
#' Removes all items except those specified. Returns a new list of the arguments you want to always retain in memory while removing duplicates.
#' @param args.save A list of new or previously specified arguments.
#' @param new.args.save Optional. One or more new object names to save (in addition to args.save)
#' @export junk_it
#' @noRd
junk_it <- function(args.save, new.args.save = NULL) {
  args.save <- c(args.save, new.args.save, "args.save") %>% unique()
  rm(list = setdiff(ls(envir = .GlobalEnv), args.save), envir = .GlobalEnv)
  return(args.save)
}


#' Spruce up R Markdown YAML parameter values that are lists into clean vectors. Called for convenience inside .RMD files.
#' @param x The parameter object from YAML `params`
#' @keywords internal
#' @export eval_params
#' @noRd
eval_params <- function(x = params) {
  y <- list() #make empty list to store new objects

  for (i in 1:length(x)) {
    obj  <- x[[i]]
    name <- names(x)[i]

    if (is.logical(obj) |
        is.numeric(obj)) {
      skip = TRUE
    } else{
      skip = FALSE
    } # skip the logicals and numerics

    if (name %in% c("year.range") & !skip) {
      yrs <- as.integer(unlist(strsplit(obj, split = ":")))
      obj <- yrs[1]:yrs[2]
    }

    if (length(obj) == 1) {
      if (stringr::str_detect(obj, ",")) {
        obj <- trimws(unlist(strsplit(obj, split = ",")))
      }
    }

    # if(obj == x[[i]])stop(print(i))
    y[[i]] <- obj
    names(y)[i] <- name
  }#end for loop
  # browser()
  return(y)
}


#' Remove column if exists as object's rownames
#' @param x a data.frame, matrix, or array of two dimensions
#' @noRd
#' @export .trim
.trim <- function(x) {
  if (is.null(rownames(x))) {
    return(x)
  } # do nothing if inappropriate for data object
  .rowtrim <- which(rownames(x) == "NA")
  if (length(.rowtrim) == 0)
    return(x)
  x <- x [-.rowtrim, ]
  return(x)
}


#' Auto generate a subdirectory name based on project parameters
#' @param species string of species names using ISO-366-2
#' @param regions string of countries or states
#' @param grid.size size of desired grid cell
#' @param max.C.ebird optional NULL or integer representing max number of birds allowed on eBird data
#' @param year.range Vector of years. will take the min and max value
#' @export proj.shorthand
#' @noRd
proj.shorthand <- function(species,
                           regions,
                           grid.size,
                           year.range,
                           max.C.ebird = NULL) {
  ## munge the states first.
  regions <- toupper(regions)
  regions <-
    gsub(
      x = regions,
      pattern = " ",
      replacement = "",
      ignore.case = TRUE
    )
  regions <-
    gsub(
      x = regions,
      pattern = "-",
      replacement = "",
      ignore.case = TRUE
    )
  regions <- gsub(x = regions, pattern = ";|,|\\|,", "-")
  regions <- paste(regions, collapse = "-")

  if (length(species) > 1)
    message(
      "multiple species indexes supplied. please check the project directory naming to ensure it properly represents desired species."
    )

  x <- paste0(
    species[nchar(species) == (max(nchar(species)))][1],
    #take min or max to assign species to dir name
    "_",
    regions,
    # regions
    "_",
    grid.size * 111,
    "km",
    # size of grid cells
    "_",
    min(year.range),
    "-",
    max(year.range),
    # time period
    "_",
    ifelse(is.null(max.C.ebird), "", paste0(max.C.ebird, "maxCebird"))
  )
  return(x)
}

#' Create a List of Repeated Initial Values for each Chain
#'
#' @param inits a list of initial values which will be repeated according to arg `nc`
#' @param nc number of chains (number of times the inits will be repeated)
#' @export make_inits_list
make_inits_list <- function(inits, nc = 3) {
  inits.out <- list()
  for (i in 1:nc) {
    inits.out[[i]] <- inits
  }
  stopifnot(length(inits.out) == nc)
  return(inits.out)

}


#' Make 2-D array (Matrix for JAGS)
#'
#' @param df.in Data frame
#' @param row Variable in df containing target row names
#' @param col Variable in df containig target column names
#' @param val Variable containing the target cell contents
#' @param replace.na Whether to replace NA values with zero.
#' @export make_mat

make_mat <-
  function(df.in,
           row = "site.ind",
           col = "year.ind",
           val,
           replace.na = FALSE) {
    # names <- names(df)
    ## will make row and col NULL and then add a thing for when they are NULL for ebird and bbs
    # e.g. if(is.null(row) & "rteno" %in% names) row <- "rteno"
    mat <- tidyr::pivot_wider(
      df.in,
      id_cols = row,
      names_from = col,
      values_from = val
    ) %>%
      as.data.frame() ## add this instead of matrix otherwise numeric cell values --> character
    # make first col the rownames

    # remove missing values in first column, which will be the rteno/checklist_id
    mat <- mat[!is.na(mat[, 1]), ]

    ## sort matrix
    mat <- mat %>%
      arrange(mat[row]) %>%
      tibble::column_to_rownames(var = row)

    ## replace nulls with NA
    mat[mat == "NULL"] <- NA
    # if replace.na is TRUE, then supply NAs with zeroes
    if (replace.na)
      mat[is.na(mat)] <- 0

    #ensure matrix is sorted by rownames and colnames
    ## sort column names as integers
    mat <-
      mat %>% dplyr::select(order(as.integer(colnames(mat))))#cols

    #### crude tests
    # stopifnot(rownames(mat)==sort(as.integer(rownames(df.in))))
    # stopifnot(colnames(mat)==sort(unique(df.in$year.ind)))

    # return object
    return(mat)

  }
