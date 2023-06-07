
#' Title
#'
#' @param df The dataframe you want to  a crosstable
#' @param group Variables as columns
#' @param col 作为行的变量
#' @param sum_row 是否计算行合计
#' @param sum_col 是否计算列合计
#' @param pct 是否计算百分比
#' @param col_sum_pct 是否计算列合计的百分比
#' @param denominator 传入数值向量作为计算百分比的分母
#' @param index 哪几列作为交叉表的标识
#' @param na.rm 是否移除缺失值
#' @param show_na 如果为TRUE,列合计的括号里将会展示缺失值的个数
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' df = data.frame(group = rep(c("a", "b"), 6), col = rep(c("c", "a", "f"),4 ))
#' fun_cross(df, group = "group", col = "col", sum_col = T,sum_row = T,  show_na = F, show_pct = T, digit_pct = 3)
fun_cross =
  function(df,group, col, sum_row = T, sum_col = T,
           show_pct = T, col_sum_pct = F, denominator = NULL,
           index = NULL, na.rm = T, show_na = T, digit_pct = 2){
    if(!sum_col) {
      col_sum_pct = F
      show_na = F
    }

    if(!sum_col & col_sum_pct){stop("未统计列合计，不能显示列合计百分比")}
    if(!sum_col & show_na){stop("未统计列合计，不能显示列合计缺失值")}
    names(df)[names(df) %in% group] = "group"
    names(df)[names(df) %in% col] = "col"
    n_na = df %>%
      group_by(group) %>%
      summarise(n_na = sum(is.na(col))) %>%
      pull(n_na)
    if(sum_row){ # 在不计算行合计的情况下，n_na会多一个
      n_na = c(n_na, sum(n_na))
    }
    if(na.rm){
      df = na.omit(df)
    }
    # 交叉表
    df_tool1 = df %>%
      tabyl(col, group)
    # 添加行
    df_tool2 =
      # if(sum_col){
      df_tool1 %>%
      adorn_totals("row")
    # }else{
    #   df_tool1
    # }
    # 添加列
    df_tool3 =
      # if(sum_row){
      df_tool2 %>% adorn_totals("col")
    # }else{
    #   df_tool2
    # }
    df_tool4 = df_tool3
    # 有时候不需要计算行合计
    row_select =
      if(col_sum_pct){
        nrow(df_tool4)
      }else{
        ifelse(sum_col,nrow(df_tool4) - 1,  nrow(df_tool4))
      }

    freq = (df_tool4[1:row_select,2:ncol(df_tool4)]) %>% unlist()

    if(show_pct){# 是否计算百分比
      if(is.null(denominator)){ # 有时候做分母的并不是列合计
        denominator = df_tool4[df_tool4$col !="Total",2:ncol(df_tool4)] %>%
          map(sum) %>%
          unlist()
        if(any(is.na(denominator))){
          warning("denominator中有NA，可能是原始数据有NA")
        }
      }
      if(length(denominator) != (length(df_tool4)-1)){
        stop("error: denominator长度不对")
      }
      prop = map2_dfc(df_tool4[1:row_select,
                               2:ncol(df_tool4)], denominator, `/`) %>%
        unlist()
      df_tool4[1:row_select, 2:ncol(df_tool4)] =
        paste0(freq, "(", round0(prop *100, digit_pct), "%)") %>%
        matrix(nrow = row_select, ncol = ncol(df_tool4)-1)
    }
    if(sum_col & show_na){
      vec_tool =
        df_tool4[nrow(df_tool4),2:ncol(df_tool4)]
      df_tool4[nrow(df_tool4),2:ncol(df_tool4)] =
        vec_tool %>%
        paste0("(",n_na, ")")
    }
    stat_type = rep(ifelse(show_pct,"n%", "n"),nrow(df_tool4))
    if(!sum_col){
      stat_type = stat_type
      # stat_type = stat_type[1:(length(stat_type) - 1)]
    }else if(show_na){
      stat_type[length(stat_type)] = "n(Missing)"
    }else if(!col_sum_pct){
      stat_type[length(stat_type)] = "n"
    }
    df_tool4 =  df_tool4 %>% mutate(col = as.character(col))
    df_tool4[df_tool4$col == "Total", "col"] = "合计"
    df_tool5 =
      df_tool4 %>%
      mutate("统计指标" = stat_type,
             .after = col) %>%
      rename("项目指标分类" ="col",
             "合计" = "Total") %>%
      as.data.frame()
    if(!sum_row){
      df_tool5 =  df_tool5 %>% select(-合计)
    }
    if(!sum_col){
      df_tool5 =  df_tool5 %>% filter(项目指标分类 != "合计")
    }
    return(df_tool5)
  }
