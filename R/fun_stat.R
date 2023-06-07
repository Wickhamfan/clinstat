#' Title
#' 对各列进行统计描述和检验的函数
#'
#' @param df_all 宽数据形式的data.frame
#' @param group 指定分组列，一般为"试验药物"列
#' @param index 指定一列或者哪几列作为输出的标识，标识列不参与统计描述与检验
#' @param show_stat 是否进行假设检验
#' @param show_method 是否展示假设检验采用的方法
#' @param show_conf 是否展示置信区间，仅针对连续型变量
#' @param show_pct 对于分类变量，交叉表中是否添加百分比
#' @param show_na 是否展示各组缺失值数量
#' @param sum_col 对于分类变量, 交叉表是否展示列合计
#' @param sum_row 对于分类变量, 交叉表是否展示行合计
#' @param col_sum_pct 对于分类变量, 交叉表的列合计是否展示百分比
#' @param denominator 对于分类变量, 通过传入一个向量作为分母计算交叉表百分比
#' @param digit_s 检验统计量保留的喜小数位
#' @param digit_p p值保留的小数位
#' @param digit_des 对于连续型变量，统计描述部分保留的小数位
#' @param digit_pct 对于分类变量， 交叉表的百分比保留的小数位
#' @param print 在进行每个变量的统计分析时，是否在命令行打印使用的检验方法
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' mpg %>% mutate(GROUP = rep(c("试验组", "对照组"), nrow(mpg)/2)) %>%
#' select(where(is.character)) %>%
#'   fun_stat_col(group = "GROUP", print = T)


fun_stat_col = function(df_all,
                        group,
                        index = NULL,
                        show_stat = T, show_method = T,
                        show_conf = F,
                        show_pct = T,show_na = T,
                        sum_col = T, sum_row = T,col_sum_pct = F,
                        denominator = NULL,
                        digit_s = 3,digit_p = 4, digit_des = 2, digit_pct = 2,
                        print = F) {
  outcome = data.frame()
  df_index =
    df_all %>%
    select(all_of(index))
  names_col =
    names(df_all)[!(names(df_all) %in% c(group, index))]
  df_group_var =
    df_all %>%
    select(all_of(c(group, names_col)))
  for (i in 2:length(df_group_var)) {
    df =
      df_group_var |>
      select(1, all_of(i)) |>
      cbind(df_index)
    name = names(df)[2]
    if (print) print(name)
    names(df)[1] = "group"
    names(df)[2] = "x"
    out = if (is.numeric(df$x)) {
      fun_continuous_des(
        df = df,
        group = "group",
        col = "x",
        index = index,
        name = name,
        show_stat = show_stat, show_method = show_method,
        show_conf = show_conf,
        print = print,
        digit_p = digit_p, digit_s = digit_s, digit_des = digit_des
      )
    } else if ("rank" %in% attributes(df$x)) {
      fun_rank_des(
        df = df,
        group = "group",
        col = "x",
        index = index,
        name = name,
        sum_row = sum_row,sum_col = sum_col,show_pct = show_pct,
        show_stat = show_stat,
        denominator = denominator,
        show_na = show_na
      )
    } else{
      fun_cata_des(
        df,
        "group",
        "x",
        index = index,
        name = name,
        # ...
        print = print,
        denominator = denominator ,

        show_stat = show_stat,show_method = show_method,
        show_pct = show_pct,show_na = show_na,

        sum_col = sum_col, sum_row = sum_row,
        col_sum_pct = col_sum_pct,

        digit_p = digit_p,
        digit_s = digit_s,
        digit_pct = digit_pct
      )
    }
    outcome = outcome %>%
      rbind(out)
  }
  if (!show_stat) {
    outcome = outcome %>%
      select(-检验统计量, -P值)
  }
  return(remove_rownames(outcome))
}
