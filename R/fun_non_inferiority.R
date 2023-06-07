
#' Title
#'
#' @param x1 试验组数据的数值向量
#' @param x2 肚子好租数据的数值向量
#' @param deta 非劣效界值
#'
#' @return 包含结果的列表
#' @export
#'
#' @examples -
fun_non_inferiority = function(x1, x2, deta){
  # x1
  nt = length(x1)
  nc = length(x2)
  nat = sum(is.na(x1))
  nac = sum(is.na(x2))
  x1 = na.omit(x1)
  x2 = na.omit(x2)
  mean1 = mean(x1)
  mean2 = mean(x2)
  sd1 = sd(x1)
  sd2 = sd(x2)
  min1 = min(x1)
  min2 = min(x2)
  max1 = max(x1)
  max2 = max(x2)
  median1 = median(x1)
  median2 = median(x2)
  Q1_1 = quantile(x1, 0.25, type = 2)
  Q1_2 = quantile(x2, 0.25, type = 2)
  Q3_1 = quantile(x1, 0.75, type = 2)
  Q3_2 = quantile(x2, 0.75, type = 2)
  n1 = length(x1)
  n2 = length(x2)
  sum1 = sum(x1)
  sum2 = sum(x2)
  var_homo_is = bartlett.test(list(x1, x2))$p.value > 0.05
  if(var_homo_is){
    Sc = ((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1 + n2 -2)
    Sx1x2 = sqrt(Sc*(1/n1 + 1/n2))
  }else{
    Sc = (sum(x1^2, na.rm = T)- sum1^2/n1 +
            sum(x2^2, na.rm = T) - sum2^2/n2)/
      (n1 + n2 -2)
  }
  t = (mean1-mean2-deta)/Sx1x2
  ci = qt(0.025, n1 + n2-2,lower.tail = F) * Sx1x2
  low = (mean1-mean2) -  ci
  up = (mean1-mean2) + ci
  # 如果n大于50， 小于50还没做
  # 单样本界值在正太近似的时候用qnorm(0.025)
  # 双侧0.05,所有为单侧qnorm(0.025)
  low1 = mean1 - qnorm(0.025)*sd1/sqrt(n1)
  low2 = mean2 - qnorm(0.025)*sd2/sqrt(n2)
  up1 = mean1 + qnorm(0.025)*sd1/sqrt(n1)
  up2 = mean2 + qnorm(0.025)*sd2/sqrt(n2)

  p = 1- pt((mean1-mean2 - deta)/Sx1x2, n1 + n2 -2)
  # qt(pt(0.95,n2(r + 1)-2),(n2(r + 1)-2))
  df1 = data.frame(n1 = n1,n2 = n2, mean1 = mean1, mean2 = mean2,
                   sd1 = sd1, sd2 = sd2, sum1 = sum1, sum2 = sum2,
                   Sc = Sc, Sx1x2 = Sx1x2,t = t, mean_diff = mean1-mean2,
                   low = low, up = up, p = p,deta = deta, var_homo_is = var_homo_is
  )
  # data.frame(试验组 = paste0(mean1 ,"±", sd1), 对照组 = paste0(mean2 ,"±", sd2),
  #            `差值(试验组-对照组)` = paste0(mean_diff, "(", up))
  df2 = df1 |>
    mutate(across(everything(), ~ sprintf("%0.4f", .))) |>
    mutate(试验组 = paste0(mean1 ,"±", sd1),
           对照组 = paste0(mean2 ,"±", sd2),
           差值 = paste0(mean_diff, "(", low,",",up ,")")
    ) |>
    select(试验组, 对照组, 差值,deta, p)
  names(df2) = c(paste0("试验组","(n=", n1,")"),
                 paste0("对照组","(n=", n2,")"),
                 "差值(试验组-对照组)",
                 "非劣效界值",
                 "P值")
  df = data.frame(matrix(nrow = 8, ncol = 6)) %>%
    set_names("统计指标", "试验组", "对照组", "试验组-对照组", "统计量", "P值")
  df[,1] = c("术后九个月时FFR值", NA, "n(Missing)",
             "Mean±Std", "Min,Max", "Median",
             "Q1, Q3", "95%CI")
  df[,2] = c(NA, NA, paste0(nt, "(", nat, ")"),
             paste0(round0(mean1, 1), "±", round0(sd1, 1)),
             paste0(round0(min1, 1), ",", round0(max1, 1)),
             round0(median1, 1),
             paste0(round0(Q1_1, 1), ",", round0(Q3_1, 1)),
             paste0(round0(low1), ",", round0(up1))
  )
  df[,3] = c(NA, NA, paste0(nc, "(", nac, ")"),
             paste0(round0(mean2, 1), "±", round0(sd2, 1)),
             paste0(round0(min2, 1), ",", round0(max2, 1)),
             round0(median2, 1),
             paste0(round0(Q1_2, 1), ",", round0(Q3_2, 1)),
             paste0(round0(low2), ",", round0(up2))
  )
  df[,4] = c(NA, NA, NA,
             paste0(round0(mean1-mean2, 1), "±", round0(sqrt(var(x1) + var(x2)), 1)),
             NA, NA, NA,
             paste0(round0(low), ",", round0(up)))
  df[2, ] = c(NA, NA, NA, NA, round0(t, 3), round0(p, 3))
  # 敏感性分析
  df_sen = matrix(nrow = 4,ncol = 6)
  df_sen[,1] = c("敏感性分析","Ls mean均值\n（95%CI）","Ls mean差值\n（95%CI）", "P值")
  out_list = list(df1, df2, df)
  return(out_list)
}

# round0(mean1-mean2, 1)
# mean1 + mean2
# sqrt(var(x1) + var(x2))



