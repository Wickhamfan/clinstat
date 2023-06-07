
# 两独立样本的wilcoxon秩和检验, x和y分别表示两个数值向量
#' Title
#'
#' @param df 该函数不会被导出到namespace
#' @param alternative NONE
#' @param correct NONE
#' @param mu NONE
#' @param digits.rank NONE
#'
#' @return NONE
#' @export
#'
#' @examples NONE
wilcoxon_2s <- function(df,alternative = "two.sided", correct = TRUE,
                        mu = 0, digits.rank = Inf) {
  list_test = df %>% group_split(group)
  x = list_test[[1]]$x
  y = list_test[[2]]$x
  # x去除缺失值
  x <- x[!is.na(x)]
  y = y[!is.na(y)]
  # 初始校正数值
  CORRECTION <- 0
  # 将x与y整合在一起
  r <- c(x - mu, y)
  # 返回r的秩
  r <- rank(if (is.finite(digits.rank)) signif(r, digits.rank) else r)
  # 计算x和y的数值个数, 也即长度
  n.x <- as.double(length(x))
  n.y <- as.double(length(y))

  # 计算W统计量
  STATISTIC <- sum(r[seq_along(x)]) - n.x * (n.x + 1)/2
  # 计算各个结的数量
  NTIES <- table(r)
  # 计算Z统计量
  z <- STATISTIC - n.x * n.y/2
  # 计算z的标准差
  SIGMA <- sqrt(
    (n.x * n.y/12) * ((n.x + n.y + 1) -
                        sum(NTIES^3 - NTIES)/((n.x + n.y) * (n.x + n.y - 1)))
  )
  # 进行近似正态校正
  if (correct) {
    CORRECTION <- switch(alternative, two.sided = sign(z) * 0.5,
                         greater = 0.5, less = -0.5)
  }
  # 计算Z统计量
  z <- (z - CORRECTION) / SIGMA
  # 计算P值
  PVAL <- switch(alternative,
                 less = pnorm(z),
                 greater = pnorm(z, lower.tail = FALSE),
                 two.sided = 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE)))
  res <- list(z = z, p = PVAL)
  return(res)
}


# x <- round(rnorm(80, mean = 10, sd = 10))
# y <- round(rnorm(80, mean = 10, sd = 10))
# wilcoxon_2s(x, y)
