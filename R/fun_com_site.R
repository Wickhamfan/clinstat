
#' Title 用于合并中心
#'
#' @param df 必须包含中心列和数据列的数据框
#' @param ls_com 代表合并中心的列表
#'
#' @return a data.frame
#' @export
#'
#' @examples -
fun_com_site = function(df, ls_com){
  for (i in 1:length(ls_com)) {
    siteid_com = ls_com[[i]]
    df =
      df %>%
      mutate(SITEID = ifelse(SITEID %in% siteid_com, paste0("合并中心", i), SITEID))
  }
  return(df)
}
