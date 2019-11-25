#' Hello world function
#'
#' @export
hello <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
