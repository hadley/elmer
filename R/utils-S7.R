prop_string <- function(allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_character else class_character,
    validator = function(value) {
      if (length(value) != 1) {
        "Must be a single string"
      } else if (!allow_na && is.na(value)) {
        "Must not be missing"
      }
    }
  )
}
