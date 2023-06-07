
#' Title 分类变量统计描述与推断
#'
#' @param df  被分析的dataframe
#' @param group 哪一列作为输出的列（如试验组、对照组)
#' @param col 哪一列作为交叉表的行
#' @param index 哪几列作为交叉表的标识
#' @param name 为项目指标分类设置内容
#' @param sum_row 是否计算行合计
#' @param sum_col 是否计算列合计
#' @param col_sum_pct 是否计算列合计的百分比
#' @param pct 是否计算百分比
#' @param stat 是否进行统计检验
#' @param denominator 传入一个向量用做计算列的分母
#' @param show_na 是否在列合计展示缺失值
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' mpg %>%
#'   select(where(is.character)) %>%
#'   mutate(GROUP = rep(c("试验组", "对照组"), nrow(mpg)/2)) %>%
#'   select(model, drv, GROUP) %>%
#'  fun_cata_des("GROUP",col = "model", index = "drv",name = "")

fun_cata_des =
  function(df, group,col, index, name,show_stat = T, show_method = T, print = F,
           digit_p = 4, digit_s = 3,digit_pct = 2,show_pct = T,
           ...
           # sum_row = T, sum_col = T,
           # col_sum_pct = F,
           # pct = T, denominator = NULL, show_na = T
  ){
    names(df)[names(df) %in% col] = "x"
    names(df)[names(df) %in% group] = "group"
    if(!show_stat & show_method) {
      show_method = F
      message("未进行统计检验，show_method由TRUE转为FALSE ")
    }
    cro_out =
      fun_cross(df, group = "group", col = "x",
                digit_pct = digit_pct,
                show_pct = show_pct,
                ...
                # sum_row = T, sum_col = T,
                # col_sum_pct = F,
                # pct = T, denominator = NULL, show_na = T
      )

    tab = table(df[, c("x","group")])
    if(show_stat){
      n = nrow(df)
      sr <- rowSums(tab)
      sc <- colSums(tab)
      theory_freq <- outer(sr, sc)/sum(tab)
      if(all(dim(theory_freq) == 2)){  # 2*2交叉表
        if(any(theory_freq <= 1) | n <= 40){
          out = fisher.test(tab)
          statistic_method = "fisher"
        }else if(all(theory_freq > 5)){
          out = chisq.test(tab,correct = FALSE)
          statistic_method = "卡方"
        }else{
          out = chisq.test(tab, correct = TRUE)
          statistic_method = "校正卡方"
        }
      }else{ # R*C交叉0c 7表
        if(mean(theory_freq < 5) < 0.2){  #对布尔矩阵求均数相当于求T的比例
          out = chisq.test(tab, correct = FALSE)
          statistic_method = "卡方"
        }else{
          out = fisher.test(tab)
          statistic_method = "fisher"
        }
      }
      if (print)
        print(statistic_method)
      statistic = ifelse(statistic_method == "fisher",
                         NA_character_,
                         round0(out$statistic, digit_s))
      P值 = round0(out$p.value, digit_p)
    }else{
      statistic = NA_character_
      P值 = NA_character_
    }
    index_val =
      df[1,index] |>
      data.frame() |>
      set_names(index) %>%
      remove_rownames()
    # 合并整理
    outcome =
      cro_out |>
      # remove_rownames() %>%
      mutate(检验统计量 = statistic,
                        P值 = P值) %>%
      cbind(data.frame(nname = name), .) %>%
      cbind(index_val, .) %>%
      rename("项目指标" = nname)
    if(show_method){
      outcome = outcome %>%
        mutate(statistic_method = statistic_method)
    }
    return(remove_rownames(outcome))
  }

