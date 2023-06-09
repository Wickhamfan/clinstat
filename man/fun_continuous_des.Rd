% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_continous_des.R
\name{fun_continuous_des}
\alias{fun_continuous_des}
\title{Title 连续型变量的统计描述和推断}
\usage{
fun_continuous_des(
  df,
  group,
  col,
  index,
  name,
  show_stat = T,
  show_conf = T,
  show_method = T,
  print = F,
  digit_s = 3,
  digit_p = 4,
  digit_des
)
}
\arguments{
\item{df}{被分析的dataframe}

\item{group}{哪一列作为输出的列（如试验组、对照组)}

\item{col}{哪一列作为交叉表的行}

\item{index}{哪几列作为交叉表的标识}

\item{name}{为项目指标分类设置内容}

\item{stat}{是否进行统计检验}
}
\value{
a datframe
}
\description{
Title 连续型变量的统计描述和推断
}
\examples{
df = data.frame(值 = 1:12, 治疗分组 = c(rep(c("A", "B", "C"),4)), 指标 = rep("C", 12), aaa = "bvbb")
fun_continuous_des(df, col = "值", group = "治疗分组",index =c("指标", "aaa"),name = "???", show_stat = F, show_conf = F,
                  digit_s = 10, digit_des = 2)
}
