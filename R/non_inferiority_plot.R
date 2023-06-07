#' Title
#'
#' @param df 由非劣效检验生成的数据
#'
#' @return a plot
#' @export
#'
#' @examples -
non_inferiority_plot =
  function(df){
    df1 = df |> mutate(y = 0.5)
    ggplot(df1, aes(x = mean_diff, y = y)) +
      geom_point(size = 3) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_x_continuous(limits =c(-0.07, 0.07),
                         breaks = c(-0.05, 0, 0.05)) +
      geom_vline(xintercept = c(-0.05, 0), linetype = c(2, 1)) +
      geom_errorbar(aes(xmin = low,
                        xmax = up), width = 0.05) +
      labs(title = "non-inferiority margin Δ") +
      theme(panel.background = element_blank(),
            plot.title = element_text(size = 15),
            axis.line.x = element_line(size = 1),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.ticks = element_blank(),
            axis.title = element_blank())
  }
