
#' Title
#'
#' @param df 原始数据框， 其中必须有SUBJID列
#' @param var 选择的列名，SUBJID必被选
#' @param new_names 为列修改列名
#' @param names_posit 按位置修改列名
#' @param join_df 连接的数据框， 按照SUBJID连接
#' @param join_type 连接类型
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' df = mpg %>% mutate(SUBJID = 1:nrow(mpg))
#' join_df = data.frame(SUBJID = 99:nrow(mpg), 治疗分组 = c("试验组", "对照组"))
#' df %>% fun_select(c(SUBJID, hwy),new_names = "hwy_new", df_group = join_df , join_type = "full")
fun_select =
  function(df,var, new_names = NULL, names_posit = NULL, df_group = NULL,
           join_type = "right"){
    df = df %>% select({{var}})
    if(!is.null(new_names)){
      if(!is.null(names_posit)){
        names(df)[names_posit] = names_posit
      }else{
        names(df)[2:(length(new_names) + 1)] = new_names
      }
    }
    if(join_type == "right"){
      join = right_join
    }else if(join_type == "left"){
      join = left_join
    }else if(join_type == "inner"){
      join = inner_join
    }else if(join_type == "full"){
      join = full_join
    }

    if(is.null(df_group)){
      df = df %>%
        mutate(SUBJID = as.numeric(SUBJID))
    }else{
      df =
        df %>%
        mutate(SUBJID = as.numeric(SUBJID)) %>%
        join(df_group, by = "SUBJID")
    }
    return(df)
  }
