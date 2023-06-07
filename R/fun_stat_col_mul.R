#' Title
#'
#' @param df 长数据
#' @param col_mul 长数据的name列
#' @param col_mul_vet 需要分析name列中的哪些元素
#' @param ...
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' mpg %>% mutate(GROUP = rep(c("试验组", "对照组"), nrow(mpg)/2)) %>%
#'   fun_stat_col_mul(df= .,group = "GROUP", col_mul = "drv")
fun_stat_col_mul =
  function(df, col_mul, ..., col_mul_vet = NULL){

    if(is.numeric(df[[col_mul]])){stop(col_mul不能是连续型变量)}

    names(df)[which(names(df) == col_mul)] = "col_mul"

    if(is.null(col_mul_vet))
      col_mul_vet = unique(df[["col_mul"]])

    outcome = data.frame()
    for(i in 1:length(col_mul_vet)){
      out =
        df %>%
        filter(col_mul == col_mul_vet [i]) %>%
        fun_stat_col(..., index = "col_mul")
      outcome = rbind(outcome,out)
    }

    return(outcome)
  }
