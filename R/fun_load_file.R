
#' Title 批量读入文件，支持.sas7bdat、.xlsx、.csv
#'
#' @param path 要批量导入的路径
#' @param suffix 文件后缀
#' @param filEencoding 编码方式， 仅.csv文件
#'
#' @return -
#' @export
#'
#' @examples -
fun_load_file =
  function(path ,suffix, filEencoding = NULL){
  if(!(suffix %in% c(".sas7bdat",".xlsx",".csv"))){
    stop("suffix 必须是下列字符之一：.sas7bdat,.xlsx,.csv")
  }
  folder_path = path
  file_names = list.files(folder_path)
  file_names = file_names[str_detect(file_names,suffix)]
  if(suffix == ".sas7bdat"){
    for (i in 1:length(file_names)) {
      sub(suffix, "", file_names[i]) |> toupper() %>%
        assign(haven::read_sas(file.path(folder_path, file_names[i])),inherits = T)
    }
  }
  if(suffix == ".xlsx"){
    for (i in 1:length(file_names)) {
      sub(suffix, "", file_names[i]) |>
        assign(readxl::read_xlsx(file.path(folder_path, file_names[i])))
    }
  }
  if(suffix == ".csv"){
    for (i in 1:length(file_names)) {
      sub(suffix, "", file_names[i]) |>
        assign(read.csv(file.path(folder_path,
                                  file_names[i]), fileEncoding = fileEncoding))
    }
  }
}
