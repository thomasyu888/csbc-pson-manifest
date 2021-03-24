## FUNCTIONS BASED ON DCCVALIDATOR
# these are re-written specifically to work with csbc-pson

# unique id validation
check_id_dups <- function(data, id_col) {
  if (!id_col %in% colnames(data)) {
    failure <- check_fail(
      msg = sprintf("Can't check for duplicate IDs, %s missing from data",
        id_col),
      behavior = sprintf("Manifest should contain the %s column", id_col),
      data = colnames(data)
    )   
    return(failure)
  }
  behavior <- sprintf("%s IDs within the manifest should be unique", id_col)
  ids <- data[[id_col]]
  duplicates <- unique(ids[which(duplicated(ids))])
  if (length(duplicates) > 0) {
    check_fail(
      msg = sprintf("Duplicate %s IDs found", id_col),
      behavior = behavior,
      data = duplicates
    )
  } else {
    check_pass(
      msg = sprintf("All %s IDs are unique", id_col),
      behavior = behavior
    )
  }
}

# check values for one key (some annots may be listed)
check_listed_value <- function(values, key, annotations) {
  if (!key %in% annotations$key) {
    return(NULL)
  }
  annot_values <- annotations[annotations$key == key, ]$value
  values <- unlist(values)

  # ignore empty values (which are valid) before collapsing the list
  values <- values[!is.na(values)]
  if (!length(values)) {
    return(values)
  }

  # column may contain the annots as a list, so some concatenation and
  # splitting should be done before validation. all columns should use
  # comma as the delimiter, except tumorType and tisse (because some
  # terms may use commas).
  delim = ","
  if (key == "tissue") {
    delim = ";"
  }
  values <- lapply(
    stringr::str_split(paste0(values, collapse = delim), delim),
    trimws
  )[[1]]

  unique(values[!values %in% c(annot_values, "") & !is.na(values)])
}

# annotation validation (including those put into a list)
check_listed_values <- function(x, annotations,
                                success = "All annotation values are valid",
                                fail = "Some annotation values are invalid") {
  if (length(names(x)) == 0) {
    stop("No annotations present to check", call. = FALSE)
  }
  if (missing(annotations)) {
    annotations <- get_synapse_annotations(synID = "syn25322361", syn = syn)
  }
  if (!all(c("key", "value", "columnType") %in% names(annotations))) {
    stop(
      "Annotations must have the following columns: 'key', 'value', and 'columnType'", # nolint
      call. = FALSE
    )
  }
  values <- purrr::imap(x, check_listed_value, annotations)
  values <- purrr::compact(values)
  behavior <- "All annotation values should conform to the vocabulary. Refer to Standard Terms for accepted values." #nolint
  if (length(values) == 0) {
    check_pass(
      msg = success,
      behavior = behavior
    )
  } else {
    check_fail(
      msg = fail,
      behavior = behavior,
      data = values
    )
  }
}
