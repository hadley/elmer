prop_string <- function(default = NULL, allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_character else class_character,
    default = if (is.null(default) && !allow_null) quote(stop("Required")) else default,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        paste0("must be a single string, not ", obj_type_friendly(value), ".")
      } else if (!allow_na && is.na(value)) {
        "must not be missing."
      }
    }
  )
}

prop_bool <- function(default, allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_logical else class_logical,
    default = default,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        if (allow_na) {
          paste0("must be a single TRUE or FALSE, not ", obj_type_friendly(value), ".")
        } else {
          paste0("must be a single TRUE, FALSE or NA, not ", obj_type_friendly(value), ".")
        }
      } else if (!allow_na && is.na(value)) {
        paste0("must be a TRUE or FALSE, not NA.")
      }
    }
  )
}

prop_list_of <- function(class, names = c("any", "all", "none")) {
  force(class)
  names <- arg_match(names)

  new_property(
    class = class_list,
    validator = function(value) {
      for (i in seq_along(value)) {
        val <- value[[i]]
        if (!S7_inherits(val, class)) {
          return(paste0(
            "must be a list of <", class@name, ">s. ",
            "Element ", i, " is ", obj_type_friendly(val), "."
          ))
        }
      }
      if (names == "all" && any(names2(value) == "")) {
        "must be a named list."
      } else if (names == "none" && any(names2(value) != "")) {
        "must be an unnamed list."
      }
    }
  )
}

prop_number_whole <- function(default = NULL, min = NULL, max = NULL, allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_double else class_double,
    default = default,
    validator = function(value) {
      if (allow_null && is.null(value)) {
        return()
      }

      if (length(value) != 1) {
        paste0("must be a whole number, not ", obj_type_friendly(value), ".")
      } else if (value != trunc(value)) {
        paste0("must be a whole number, not ", obj_type_friendly(value), ".")
      } else if (!is.null(min) && value < min) {
        paste0("must be at least ", min, ", not ", value, ".")
      } else if (!is.null(max) && value > max) {
        paste0("must be at most ", max, ", not ", value, ".")
      } else if (!allow_na && is.na(value)) {
        "must not be missing."
      }
    }
  )
}
