# df = data.frame(值 = 1:12, 治疗分组 = c(rep(c("A", "B"),6)), 指标 = rep("aa", 12))
# df = data.frame(y = rep(c("是", "否","无"),4), 治疗分组 = c(rep(c("A", "B"),6)), aa = rep("aa", 12))
# col = "值"
# group = "治疗分组"
# index = "指标"
# fun_continuous_des(df, col = "值", group = "治疗分组",index ="指标",name = ">")
# f_des(df, "治疗分组", "y","aa")

#' Title 连续型变量的统计描述和推断
#'
#' @param df 被分析的dataframe
#' @param group 哪一列作为输出的列（如试验组、对照组)
#' @param col 哪一列作为交叉表的行
#' @param index 哪几列作为交叉表的标识
#' @param name 为项目指标分类设置内容
#' @param stat 是否进行统计检验
#'
#' @return a datframe
#' @export
#'
#' @examples
#' df = data.frame(值 = 1:12, 治疗分组 = c(rep(c("A", "B", "C"),4)), 指标 = rep("C", 12), aaa = "bvbb")
#' fun_continuous_des(df, col = "值", group = "治疗分组",index =c("指标", "aaa"),name = "???", show_stat = F, show_conf = F,
#'                   digit_s = 10, digit_des = 2)

fun_continuous_des =
  function(df, group, col, index, name, show_stat = T, show_conf = T, show_method = T,print = F,
           digit_s = 3, digit_p = 4, digit_des){
    df = df %>% ungroup()
    names(df)[names(df) %in% col] = "x"
    names(df)[names(df) %in% group] = "group"
    if(!show_stat & show_method){
      show_method = F
      message("未进行统计检验，show_method由TRUE转为FALSE")
    }
    if(!is.numeric(digit_des) | !is.numeric(digit_s) | !is.numeric(digit_p))
      stop("digit_des、digit_s、digit_p必须是数值型")

    # names(df)[names(df) %in% index] = "index"
    col_sum =
      df |>
      summarize(
        count = as.character(n() - sum(is.na(x))),
        missing = as.character(sum(is.na(x))),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        Q1 = quantile(x, 0.25, na.rm = TRUE, type = 2),
        Q3 = quantile(x, 0.75, na.rm = TRUE, type = 2),
      ) |>
      mutate(across(where(is.numeric), ~ round0(., digit_des))) |>
      mutate(`n(Missing)` = paste0(count, "(", missing, ")"),
             `Mean±SD` = paste0(mean, "±", sd),
             `Min,Max` = paste0(Min, ",", Max),
             Median = as.character(median),
             `Q1,Q3` = paste0(Q1, ",", Q3)) |>
      select(`n(Missing)`,`Mean±SD`,`Min,Max`,Median,`Q1,Q3`) |>
      pivot_longer(everything(), names_to = "统计指标", values_to = "合计")
    des = df |>
      group_by(group) |>
      summarize(
        count = as.character(n() - sum(is.na(x))),
        missing = as.character(sum(is.na(x))),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        Q1 = quantile(x, 0.25, na.rm = TRUE, type = 2),
        Q3 = quantile(x, 0.75, na.rm = TRUE, type = 2),
        low = mean + qnorm(0.025) * sd/sqrt(as.numeric(count)),
        up = mean - qnorm(0.025) * sd/sqrt(as.numeric(count))
      ) |>
      mutate(across(is.numeric, ~ round0(.,digit_des))) |>
      # 注 ：此版本置信区间为各组样本量均大于40时的正态近似法，其他方法尚未添加
      mutate(`95%CI` = paste0("(", low, ",", up, ")")) %>%
      mutate(`n(Missing)` = paste0(count, "(", missing, ")"),
             `Mean±SD` = paste0(mean, "±", sd),
             `Min,Max` = paste0(Min, ",", Max),
             Median = as.character(median),
             `Q1,Q3` = paste0(Q1, ",", Q3)) |>
      select(group, `n(Missing)`,`Mean±SD`,`Min,Max`,Median,`Q1,Q3`, `95%CI`) |>
      pivot_longer(-group, names_to = "统计指标", values_to = "值") |>
      pivot_wider(names_from = group, values_from = 值) |>
      left_join(col_sum,by = "统计指标") %>%
      cbind(data.frame(项目指标分类 = ""), .)

    ###################################   test   ###############################
    # df %>% group_split(y) %>% map(~pull(., x) %>% unique()) %>%
    #   reduce(identical)
    if(show_stat | show_conf){
      df_norm =
        df |>
        group_split(group)
      if(any(map_lgl(df_norm, ~ all(.$x == .$x[1], na.rm = T)), na.rm = T)){
        normal_is = FALSE
      }else{
        # normal_is = all(map(df_norm, ~ shapiro.test(.$x)$p.value) > 0.05) | nrow(na.omit(df)) >= 40
        normal_is =
          if(nrow(na.omit(df)) >= 40){
            TRUE
          }else{
            all(map(df_norm, ~ shapiro.test(.$x)$p.value) > 0.05)
          }
      }
      var_homo_is = bartlett.test(x ~ group, df)$p.value > 0.05

      if (normal_is & var_homo_is) {
        test_all =
          t.test(x ~ group, data = df, var.equal = T)
        # statistic = test_all[["statistic"]]
        # p_value = test_all[["p.value"]]

        test =
          test_all[c("statistic", "p.value")] |>
          data.frame() |>
          set_names("检验统计量", "P值")

        conf_int = paste0(
          "(",
          round0(test_all[["conf.int"]][[1]]),
          ",",
          round0(test_all[["conf.int"]][[2]]),
          ")")
        statistic_method = "t检验"
      }else if(normal_is & !var_homo_is) {
        test_all =
          t.test(x ~ group, data = df, var.equal = F)
        test =
          test_all[c("statistic", "p.value")] |>
          data.frame() |>
          set_names("检验统计量", "P值")
        conf_int =
          paste0(
            "(",
            round0(test_all[["conf.int"]][[1]]),
            ",",
            round0(test_all[["conf.int"]][[2]]),
            ")")
        if (show_method) statistic_method = "t'检验"
      } else{
        test = wilcoxon_2s(df,correct = F) |>
          data.frame() |>
          set_names("检验统计量", "P值")
        conf_int = NA_character_
        if (show_method) statistic_method = "wilcox秩和"
      }
      test = mutate(test,
                    检验统计量 = round0(检验统计量, digit_s),
                    P值 = round0(P值, digit_p)
                    # 检验统计量 = sprintf("%0.3f", 检验统计量),
                    # P值 = sprintf("%0.4f", P值)
      )
    }else{
      # 不进行假设检验
      test = data.frame(检验统计量 = NA_character_, P值 = NA_character_)
    }
    # 是否print检验方法到终端
    if(print) print(statistic_method)
    # 结果是否显示检验方法
    if(show_method){
      test =
        test %>%
        mutate(statistic_method = statistic_method)
    }

    # 置信区间显示
    if(!show_conf){
      des =
        des %>%
        filter(统计指标 != "95%CI")
    }else{
      des[des$统计指标 == "95%CI", "合计"] = conf_int
    }
    if(!all(index %in% names(df))) stop("index必须是data.frame中存在的列")
    index_val = df[1,index] |>
      data.frame() |>
      set_names(index)
    cbind(
      remove_rownames(index_val),
      # index_val[rep(1, nrow(des)),],
      data.frame(nname = name),
      des,remove_rownames(test)) %>%
      rename("项目指标" = "nname")
  }
