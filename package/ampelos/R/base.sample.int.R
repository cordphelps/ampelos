sample.int <- function (n, size = n, replace = TRUE, prob = NULL)
{
  # copied from base and modified to set replace = TRUE
  # > sample.int <- base:::sample.int
  # https://stackoverflow.com/questions/8743390/how-do-i-override-a-non-visible-function-in-the-package-namespace
  #
  if (!replace && is.null(prob) && n > 1e+07 && size <= n/2)
    .Internal(sample2(n, size))
  else .Internal(sample(n, size, replace, prob))
}
