# 定义函数
#' Title
#'
#' @param vct  A numeric vector.
#' @param num A numeric vector of length one
#'
#' @return A character vector
#' @export
#'
#' @examples
#' (x = c(1.999, 1.005, 1.134))
#' round0(x, 2)
round0 = function(vct, num = 2){
  sprintf(paste0("%0.", num, "f"), vct)
}
