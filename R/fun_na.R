
#' Title 制表,填缺失值
#'
#' @param data 数据框
#' @param vec 字符向量，向量的内容为要填补缺失值得列名
#'
#' @return a dataframe
#' @export
#'
#' @examples iris %>% fun_NA("Species")
fun_NA = function(data, vec){
  data = data %>% mutate(across(all_of(vec), as.character))
  for (i in length(vec):1) {
    group_by_vars = vec[1:i]
    mutate_var = vec[i]
    data = data %>%
      group_by(across(group_by_vars)) %>%
      mutate(!!mutate_var := ifelse(row_number() == 1, !!sym(mutate_var), NA))
  }
  return(data)
}
