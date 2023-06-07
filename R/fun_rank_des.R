
#' Title
#'
#' @param df -
#' @param group -
#' @param col -
#' @param index -
#' @param name -
#' @param sum_row -
#' @param sum_col -
#' @param pct -
#' @param stat -
#' @param denominator -
#' @param show_na -
#'
#' @return a dataframe
#' @export
#'
#' @examples -


fun_rank_des =
  function(df, group,col, index, name, sum_row = T, sum_col = F, pct = T, stat =T, denominator = NULL,
           show_na = F){
    names(df)[names(df) %in% col] = "x"
    names(df)[names(df) %in% group] = "group"
    cro_out = fun_cross(df, group = "group", col = "x", sum_row = sum_row,
                        sum_col = sum_col, pct = pct, denominator = denominator,
                        show_na = show_na)
    if(stat){
      df = df %>% mutate(x = as.numeric(x))
      out = wilcoxon_2s(df,correct = F) |>
        data.frame() |>
        set_names("检验统计量", "P值")
      print("等级wilcox")
      statistic = out$检验统计量
      P值 = out$P值
    }else{
      statistic = NA
      P值 = NA
    }
    index_val = df[1,index] |>
      data.frame() |>
      set_names(index)
    index_val = index_val[rep(1,nrow(cro_out)),]
    cro_out |> mutate(检验统计量 = statistic,
                      P值 = P值) %>%
      cbind(data.frame(nname = name), .) %>%
      cbind(index_val, .) %>%
      rename("项目指标分类" ="col",
             "合计" = "Total",
             "项目指标" = nname) %>%
      mutate("统计指标" = ifelse(项目指标分类 == "Total", "n", "n(%)"),
             .after = 项目指标分类)
  }

