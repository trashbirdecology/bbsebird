#' @param ebd_zf The zero-filled ebird data object (flat)
#' @keywords internal
#' @importFrom lubridate year yday
#' @noRd
clean_ebird_colnames <- function(df) {

  if("observation_count" %in% colnames(df)){
    df <- df |> dplyr::rename(c=observation_count)
  }

  clean_df <- df |>
    mutate(
      # convert X to NA
      c = as.integer(c),
      # effort_distance_km to 0 for non-travelling counts
      effort_distance_km = if_else(protocol_type != "Traveling",
                                   0, effort_distance_km)
    )

  suppressWarnings(
    clean_df <- clean_df |> dplyr::mutate(
      # convert time to decimal hours since midnight
      time_observations_started_hsm = lubridate::hms(time_observations_started),
      # split date into year and day of year
      year = lubridate::year(observation_date),
      yday = lubridate::yday(observation_date)
    )
  )## may get useless warnings re: NA values in the time_observations_started
  return(clean_df)

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


  x <- x |>
    mutate(across(any_of(c(chrs, num, ints)), as.character)) |>
    mutate(across(any_of(num), as.numeric)) |>
    mutate(across(any_of(ints), as.integer)) |>
    mutate(across(any_of(dates), as.Date))

  return(x)


}

#' split_table internal function
#'
#' @noRd
#' @keywords internal
split_tibble <-
  function(tibble, col = 'col')
    tibble |> split(., .[, col])


#' compute standard error of a vector
#' @keywords internal
#' @noRd
se <- function(x) {
  sd(x) / sqrt(length(x))
}

#' Force a AOU Numbers into Integer
#' Used very specifically as an internal function when munging BBS data.
#' @keywords internal
#' @param dplyr pull
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
#' @noRd
junk_it <- function(args.save, new.args.save = NULL) {
  args.save <- c(args.save, new.args.save, "args.save") |> unique()
  rm(list = setdiff(ls(envir = .GlobalEnv), args.save), envir = .GlobalEnv)
  return(args.save)
}


#' Spruce up R Markdown YAML parameter values that are lists into clean vectors. Called for convenience inside .RMD files.
#'
#' @param x The parameter object from YAML `params`
#' @keywords internal
#' @importFrom stringr str_detect
#' @export eval_params
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

    if (name %in% c("years") & !skip) {
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
.trim <- function(x) {
  if (is.null(rownames(x))) {
    return(x)
  } # do nothing if inappropriate for data object
  .rowtrim <- which(rownames(x) == "NA")
  if (length(.rowtrim) == 0)
    return(x)
  x <- x [-.rowtrim,]
  return(x)
}




#' Make 2-D array (Matrix for JAGS)
#'
#' @param df.in Data frame
#' @param row Variable in df containing target row names
#' @param col Variable in df containing target column names
#' @param val Variable containing the target cell contents
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @param replace.na Whether to replace NA values with zero.
#' @importFrom dplyr select
#' @export make_mat

make_mat <-
  function(df.in,
           row = "site.ind",
           col = "year.ind",
           val,
           replace.na = FALSE) {
    # ensure no duplicates of the row and col identifiers exist.
    df.in <- df.in |>
      filter(!is.na(eval(parse(text = row))) &
               !is.na(eval(parse(text = col)))) |>
      distinct(eval(parse(text = row)), eval(parse(text = col)), .keep_all = TRUE)

    ## will make row and col NULL and then add a thing for when they are NULL for ebird and bbs
    # e.g. if(is.null(row) & "rteno" %in% names) row <- "rteno"
    mat <- pivot_wider(
      df.in,
      id_cols = row,
      names_from = col,
      values_from = val
    ) |>
      as.data.frame() ## add this instead of matrix otherwise numeric cell values --> character
    # make first col the rownames
    # remove missing values in first column, which will be the rteno/checklist_id
    mat <- mat[!is.na(mat[, 1]),]

    ## sort matrix
    mat <- mat |>
      arrange(mat[row]) |>
      tibble::column_to_rownames(var = row)

    ## replace nulls with NA
    mat[mat == "NULL"] <- NA
    # if replace.na is TRUE, then supply NAs with zeroes
    if (replace.na)
      mat[is.na(mat)] <- 0

    #ensure matrix is sorted by rownames and colnames
    ## sort column names as integers
    mat <-
      mat |> dplyr::select(order(as.integer(colnames(mat))))#cols

    #### crude tests
    stopifnot(rownames(mat) == sort(unique(df.in$site.ind)))
    # stopifnot(colnames(mat)==sort(unique(df.in$year.ind)))

    # return object
    return(mat)

  }




#' Munge Dates and Times of the eBird and BBS Datasets
#' @param dat data set with at least the column `date` and of class "date"
#' @param min.yday minimum day of the year to include in resulting dataset
#' @param max.yday maximum day of the year to include in resulting dataset
#' @param sunlight logical If TRUE will calculate all sunlight, moonlight, rise and set times. This is computationally demanding for the eBird data, so do not set to TRUE unless needed.
#' @param base.date character or date string (YYYY-MM-DD) to use as the origin date for calculating Julian date.
#' @importFrom lubridate as_date yday
#' @importFrom hms as_hms
#' @importFrom suncalc  getSunlightTimes
#' @importFrom dplyr bind_rows across left_join
#' @importFrom parallel splitIndices
#' @export munge_date_time

munge_date_time <-
  function(dat,
           base.date,
           min.yday = 0,
           max.yday = 365,
           sunlight = FALSE) {
    if (!class(base.date) == "Date")
      stop(
        "`base.date` is not of class `date`. please check specification of base.date. \nConsider using `lubridate::ymd()` or `lubridate::as_date()` to change base.date to class date. "
      )

    names = names(dat)

    if ("date" %in% names) {
      dat$date   <- lubridate::as_date(dat$date)
      dat$yday   <- lubridate::yday(dat$date)
      dat$julian <-
        julian(dat$date, origin = lubridate::as_date(base.date))
      ## filter on the ydays (if provided)
      dat <-
        dat |> filter((yday >= min.yday &
                          yday <= max.yday) | is.na(yday)) ## keep the NA values.
    } # end dates munging

    # bbs data has starttime and endtime
    if ("starttime" %in% names) {
      dat$starttime = hms::as_hms(as.POSIXct(dat$starttime, format = "%H%M"))
    }
    if ("endtime" %in% names) {
      dat$endtime = hms::as_hms(as.POSIXct(dat$endtime, format = "%H%M"))
    }
    # ebird data has time_obs_started
    if ("time_observations_started" %in% names) {
      dat$time_observations_started <-
        hms::as_hms(as.POSIXct(dat$time_observations_started, format = "%H:%M:%S"))
    }

    #### SUNLIGHT/MOONLIGHT/RISE/SET
    if (sunlight) {
      sunlight.keep <-
        c("dawn", "solarNoon", "sunrise", "sunriseEnd") # change these later if you need other information.

      cat(
        "Calculating astronomical statistics for each observation. This may take a few minutes.\n"
      )

      ## ebird is so large that I need to split up b/c calculating astronomical info takes forever.
      ## i'd like to use kit::funique, but cannot figure out how to do that with >1 columns.
      ### so, am resorting to this method.
      x <- dat |> dplyr::select(date, lat, lon) |>
        filter(!is.na(date))
      chunks <- parallel::splitIndices(nrow(x), 100)

      for (i in seq_along(chunks)) {
        if (i == 1)
          dat.sun <- NULL
        rows = as.data.frame(chunks[i])
        if (nrow(rows) == 0)
          next()
        chunk.start = min(rows[1])
        chunk.end   = max(rows[1])
        y = x[chunk.start:chunk.end,]
        y = suncalc::getSunlightTimes(data = y,
                                      keep = sunlight.keep)

        dat.sun <- dplyr::bind_rows(dat.sun, y)
        rm(chunk.end, chunk.start, rows, y)
      }#end chunk for loop

      ### turn vars in sunlight.keep into time only (otherwise they are in YYYY-MM-DD HH-MM-SS; we need only HH-MM)
      dat.sun <- dat.sun |>
        dplyr::mutate(across(sunlight.keep, hms::as_hms))

      ### add sunlight information to spatial data sets
      dat <- dplyr::left_join(dat, dat.sun)


    } # END SUNLIGHT



    return(dat)

  }





# set model filenames ---------------------------------------------------------
#' @title Specify Model Output Filenames
#' @description Helper function for creating filenames for exporting MCMC samples and plots thereof.
#' @param mod.name ...
#' @param dir.proj location of where to send exported files. If NULL or not specified will send to current working directory.
#' @param dir.plots path to save the MCMC samples as .rds object
#' @param dev.mode  logical If TRUE will append "dev_" preceding all filenames.
#' @param dir.samps path to save the MCMC samples as .rds object
#' @param ni number of iterations (pre-thinning and pre-burnin)
#' @param nb number of iterations to discard
#' @param nc number of chains
#' @param nt thinning rate for MCMC. Used in filenames
#' @param nbfs number of basis functions used in the model.
#' @param mod.name a shorthand name for saving outputs.
#' @export set.filenames

set.filenames <-
  function(mod.name,
           dir.proj  = getwd(),
           dir.plots = NULL,
           dir.samps = NULL,
           ni = NULL,
           nb = NULL,
           nc = NULL,
           nt = NULL,
           nbfs = NULL,
           dev.mode = FALSE
           ) {
    if (dev.mode)
      mod.name <- paste0("dev_", mod.name)

    if (is.null(dir.plots))
      dir.plots  <-
        paste0(dir.proj, "/plots/") # saves plots various plots inside this top dir
    if (is.null(dir.samps))
      dir.samps  <-
        paste0(dir.proj, "/samps/") # saves plots various plots inside this top dir

    dir.proj  <- gsub("//", "/", dir.proj)
    dir.samps <- gsub("//", "/", dir.samps)
    dir.plots <- gsub("//", "/", dir.plots)

    lapply(
      list(dir.plots, dir.samps),
      dir.create,
      showWarnings = FALSE,
      recursive = TRUE
    )

    fn <- paste0(mod.name,
                 "_",
                 ni,
                 "iters_",
                 nc,
                 "chains_",
                 nb,
                 "nb_",
                 nt,
                 "nt_",
                 constants$K,
                 "bfs")
    fn.samps   <- paste0(dir.samps, fn, ".rds")
    fn.monhat  <- paste0(dir.samps, fn, "_monitorhats.rds")
    fn.hatplot <- paste0(dir.samps, fn, "_hats.pdf")
    fn.gif     <- paste0(dir.plots, fn, "_anim.gif")
    fn.trace   <- paste0(dir.plots, fn, "_trace.pdf")
    fn.ey      <- paste0(dir.plots, fn, "_Ey.pdf")
    fn.cat     <- paste0(dir.plots, fn, "_cat.pdf")
    fn.ggmcmc  <- paste0(dir.plots, fn, "_ggmcmc.pdf")

    fns <-
      list(
        dir.samps = dir.samps,
        dir.plots = dir.plots,
        fn = fn,
        fn.ey = fn.ey,
        fn.cat = fn.cat,
        fn.ggmcmc = fn.ggmcmc,
        fn.samps = fn.samps,
        fn.monhat = fn.monhat,
        fn.hatplot = fn.hatplot,
        fn.gif = fn.gif,
        fn.trace = fn.trace
      )
    return(fns)

  } # End mod.filenames

