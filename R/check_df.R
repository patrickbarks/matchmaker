#' Check values in one or more columns of a data frame against a dictionary of
#' allowed values for each column
#'
#' @description
#' Checks a data frame of raw data against a dictionary specifing allowed values
#' for one or more columns (or sets of columns matched with a regex keyword),
#' and returns a data frame listing the non-allowed values (if any) for each
#' column of interest. The returned data frame can form the basis for a second
#' dictionary to be used for recoding non-allowed values with `match_df` (e.g.
#' to correct misspellings, or otherwise standardize).
#'
#' @param x a data frame containing one or more variables (i.e. columns) whose
#'   values are to be checked against a dictionary
#'
#' @param dictionary a data frame with at least two columns, which define
#'   variables within `x` and their corresponding set of allowed values.
#'
#' @param col_vals name (character) or position (integer) of the column within
#'   `dictionary` that represents allowed values. Defaults to `1` (first
#'   column).
#'
#' @param col_vars name (character) or position (integer) of the column within
#'   `dictionary` that represents variables (or sets of variables matched with
#'   a .regex keyword) in `x`. Defaults to `2` (second column).
#'
#' @param return_allowed logical indicating whether the returned data frame
#'   should include a third column summarizing the allowed values corresponding
#'   to each non-allowed value/variable. The elements of this column are created
#'   by collapsing the allowed values for the given variable into a single
#'   string. Defaults to FALSE.
#'
#' @param sep_allowed If `return_allowed == TRUE`, the `collapse` argument
#'   passed to `paste()` for creating strings of allowed values from the
#'   corresponding vectors. Defaults to "; ".
#'
#' @param nchar_allowed If `return_allowed == TRUE`, the maximum number of
#'   characters to return in strings defining allowed values. Longer strings are
#'   truncated and given suffix "...". Defaults to `70` (set to `Inf` to avoid
#'   truncation).
#'
#' @section Specifying columns of `x` in `dictionary[,col_vars]`:
#'
#' Elements within the `col_vars` column of `dictionary` represent keys that you
#' want to match to column names in `x` (the data set). These are expected to
#' match exactly with the exception of one reserved keyword that starts with a
#' full stop:
#'
#'  - `.regex [pattern]`: any column whose name is matched by `[pattern]`. The
#'  `[pattern]` should be an unquoted, valid, PERL-flavored regular expression.
#'
#' @return
#' A data frame of values in `x` that __don't__ match values in the
#' `dictionary`. Contains columns `value` and `variable`. If argument
#' `return_allowed == TRUE`, also contains a 3rd column `values_allowed` giving
#' the allowed values (collapsed into a string) for each variable.
#'
#' @author Patrick Barks
#'
#' @export
#'
#' @examples
#'
#' # Read in dictionaries and coded data examples --------------------
#'
#' dict_allowed <- read.csv(matchmaker_example("allowed-dictionary.csv"),
#'   stringsAsFactors = FALSE)
#'
#' dict_recode <- read.csv(matchmaker_example("spelling-dictionary.csv"),
#'   stringsAsFactors = FALSE)
#'
#' dat <- read.csv(matchmaker_example("coded-data.csv"),
#'   stringsAsFactors = FALSE)
#'
#' # check original data for non-allowed values
#' check_df(dat, dict_allowed)
#'
#' # include column of allowed values in string format
#' check_df(dat, dict_allowed, return_allowed = TRUE)
#'
#' # recode with match_df() then check for remaining non-allowed values
#' dat_recode <- match_df(dat,
#'   dictionary = dict_recode,
#'   from = "options",
#'   to = "values",
#'   by = "grp")
#'
#' check_df(dat_recode, dict_allowed)
#'
check_df <- function(x = data.frame(),
                     dictionary,
                     col_vals = 1,
                     col_vars = 2,
                     return_allowed = FALSE,
                     sep_allowed = "; ",
                     nchar_allowed = 60) {

  # # for testing purposes only
  # x <- dat
  # dictionary <- dict_allowed
  # col_vals = 1
  # col_vars = 2
  # return_allowed = FALSE
  # sep_allowed = "; "
  # nchar_allowed = 60

  if (length(x) == 0 || !is.data.frame(x)) {
    stop("x must be a data frame")
  }

  if (!is.data.frame(x)) {
    stop("dictionary must be a data frame")
  }

  # Split dictionary by col_vars
  dictionary <- split(dictionary, dictionary[[col_vars]])

  # Extract vars to check from dictionary (both plain and regex)
  vars_check   <- names(dictionary)
  is_var_regex <- grepl("^\\.regex[[:space:]]", vars_check)

  # If any .regex keys...
  if (any(is_var_regex)) {

    vars_check_plain <- vars_check[!is_var_regex]
    vars_check_regex <- vars_check[is_var_regex]
    vars_check_regex_extract <- gsub("\\.regex[[:space:]]", "", vars_check_regex)

    # which cols in x match each regex var (1 element for each .regex key)
    vars_regex_match_list <- lapply(
      vars_check_regex_extract,
      FUN = grep,
      x = names(x), value = TRUE, perl = TRUE
    )

    # check for dictionary variables with no match in x
    vars_regex_match_n <- lengths(vars_regex_match_list)
    vars_plain_nomatch <- vars_check_plain[!vars_check_plain %in% names(x)]
    vars_regex_nomatch <- vars_check_regex[vars_regex_match_n == 0]
    vars_nomatch <- unique(c(vars_plain_nomatch, vars_regex_nomatch))

    # all columns of x that match variable in dictionary
    cols_match_plain <- vars_check_plain[vars_check_plain %in% names(x)]
    cols_match_regex <- unlist(vars_regex_match_list, use.names = FALSE)
    cols_match <- c(cols_match_plain, cols_match_regex)

    # dictionary variable name corresponding to each matching column in x
    matching_var_plain <- cols_match_plain
    matching_var_regex <- rep(vars_check_regex, vars_regex_match_n)
    matching_var <- c(matching_var_plain, matching_var_regex)

    # columns of x to iterate over, and matching key from dictionary
    to_iterate_x <- cols_match
    to_iterate_dictionary <- matching_var

    # Else no .regex keys, column names in x and dictionary keys matched literally
  } else {

    to_iterate_x <- to_iterate_dictionary <- intersect(vars_check, names(x))
    vars_nomatch <- setdiff(vars_check, names(x))
  }

  # Warn if any variables in dictionary don't match any columns in x
  if (length(vars_nomatch) > 0) {
    warning("The following variable(s) in the dictionary did not match any ",
            "columns in 'x': ", paste(vars_nomatch, collapse = ", "),
            call. = FALSE)
  }

  # Initialize list to capture variable-specific outputs
  out <- list()

  # Loop over the variables and check for non-allowed values
  for (i in seq_along(to_iterate_x)) {

    i_x <- to_iterate_x[i]
    i_w <- to_iterate_dictionary[i]

    values_x <- sort(unique(x[[i_x]]), na.last = TRUE)
    values_w <- unique(dictionary[[i_w]][[col_vals]])

    non_allowed <- setdiff(values_x, values_w)

    # base output, non-allowed values and corresponding variable
    df_out <- data.frame(
      value = non_allowed,
      variable = rep(i_w, length(non_allowed)),
      stringsAsFactors = FALSE
    )

    # if return_allowed == TRUE, add third column of allowed values
    if (return_allowed) {

      values_w_str <- paste(values_w, collapse = sep_allowed)

      if (nchar(values_w_str) > nchar_allowed) {
        values_w_str <- paste0(
          substr(values_w_str, 1, nchar_allowed - 3), "..."
        )
      }

      df_out[["values_allowed"]] <- rep(values_w_str, length(non_allowed))
    }

    out[[i]] <- df_out
  }

  # return *unique* rows because .regex keywords can lead to duplicates
  unique(do.call(rbind, out))
}
