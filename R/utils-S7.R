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
        paste0("must be a single string, not ", obj_type_friendly(value))
      } else if (!allow_na && is.na(value)) {
        "must not be missing"
      }
    }
  )
}
