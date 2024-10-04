prop_string <- function(allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_character else class_character,
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

prop_bool <- function(allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_logical else class_logical,
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
            "must be a list of <", turn@name, ">s. ",
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
