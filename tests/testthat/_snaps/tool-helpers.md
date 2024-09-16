# roxygen2 comment extraction works

    Code
      extract_comments_and_signature(has_roxygen_comments)
    Output
      [1] "#' A function for foo-ing three numbers.\n#'\n#' @param x The first param\n#' @param y The second param\n#' @param z Take a guess\n#' @returns The result of x %foo% y %foo% z.\nfunction (x, y, z = pi - 3.14)  ..."

---

    Code
      extract_comments_and_signature(aliased_function)
    Output
      [1] "#' A function for foo-ing three numbers.\n#'\n#' @param x The first param\n#' @param y The second param\n#' @param z Take a guess\n#' @returns The result of x %foo% y %foo% z.\nfunction (x, y, z = pi - 3.14)  ..."

---

    Code
      extract_comments_and_signature(indented_comments)
    Output
      [1] "  #' A function for foo-ing three numbers.\n  #'\n  #' @param x The first param\n  #' @param y The second param\n  #' @param z Take a guess\n  #' @returns The result of x %foo% y %foo% z.\nfunction (x, y, z = pi - 3.14)  ..."

---

    Code
      extract_comments_and_signature(no_srcfile)
    Output
      [1] "  #' A function for foo-ing three numbers.\nfunction (a, b, c = pi - 3.14)  ..."

# basic signature extraction works

    Code
      extract_comments_and_signature(no_roxygen_comments)
    Output
      [1] "function (i, j, k = pi - 3.14)  ..."

