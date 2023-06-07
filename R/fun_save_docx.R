
#' Title
#'
#' @param df 要输出的data.frame
#' @param path 输出的路径,以,docx结尾
#'
#' @return none
#' @export
#'
#' @examples none
fun_save_docx = function(df,path){
  sect_properties = prop_section(
    page_size = page_size(
      orient = "landscape",
      width = 12, height = 11.7
    ),
    type = "continuous",
    page_margins = page_mar()
  )
  df |> xtable() |>
    flextable() |>
    save_as_docx(path = path,pr_section = sect_properties)
}
