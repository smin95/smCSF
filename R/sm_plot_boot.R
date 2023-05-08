#' Plotting eigenvalues and their 95% confidnece intervals from non-parametric
#' bootstrap
#'
#' This function plots eigenvalues and their 95% confidence interval by using
#' the data frame that is returned from `sm_np_boot()`. However, the author suggests that
#' the user plot the eigenvalues rather than to use `sm_plot_boot()` for more
#' customization. This function is for demonstration purposes
#'
#' @param boot_res
#' The returned data frame from `sm_np_boot()`.
#'
#' @param point_size
#' Size of the points.
#'
#' @param dodge_width
#' Width of the positional difference between the two plots (data and random.
#' )
#' @param line_width
#' The width of the lines.
#'
#' @param colors
#' The colors of the plots.
#'
#' @param shapes
#' The shapes of the plots.
#'
#' @param labels
#' The labels for the legend.
#'
#' @param legends_loc
#' Location of the legend (x and y).
#'
#' @export
#' @import ggplot2 cowplot
#' @importFrom smplot2 sm_classic
#'
#' @examples
#' \dontrun{
#'
#' df <- read_csv('https://www.smin95.com/data_ACh.csv') %>%
#' group_by(Subject,SpatialFreq) %>%
#'  summarise(Sensitivity = mean(Sensitivity),
#'            Repetition = 'avg')
#'
# 'res <- sm_params_list(subjects = 'Subject',
#'                      conditions = 'Repetition',
#'                      x = 'SpatialFreq',
#'                      values = 'Sensitivity',
#'                      data = df)
#'
#' boot_res <- sm_np_boot(res, n=51)
#' sm_plot_boot(boot_res)
#' }
#'

sm_plot_boot <- function(boot_res,
                         point_size = 2.8,
                         dodge_width = 0.2,
                         line_width = 0.8,
                         colors = c('#414242','#797f85'),
                         shapes = c(16,17),
                         labels =c('Data', 'Random'),
                         legends_loc = c(0.55,0.7)) {

  if (length(colors) == 1) colors <- rep(colors,2)
  if (length(shapes) == 1) shapes <- rep(shapes,2)


  fig <-  ggplot(data = boot_res,
                 aes(x = nFac, y = mean,
                     group = group,
                     color = group,
                     shape = group)) +
    geom_point(size = point_size,
               position = position_dodge(width = dodge_width)) +
    geom_line(linewidth = line_width,
              position = position_dodge(width = dodge_width)) +
    geom_linerange(aes(ymin = downCI,
                       ymax = upCI),
                   linewidth = line_width,
                   position = position_dodge(width = dodge_width),
                   show.legend = FALSE) +
    sm_classic(legends= TRUE) +
    theme(legend.position = c(0.55,0.7),
          legend.background = element_blank(),
          legend.title = element_blank()) +
    scale_x_continuous(breaks = unique(boot_res$nFac)) +
    xlab('Factor') +
    ylab('Mean eigenvalues') +
    scale_color_manual(values = colors,
                       labels = labels) +
    scale_shape_manual(values = shapes,
                       labels = labels)
  return(fig)
}

globalVariables(c('ggplot', 'geom_point', 'geom_line',
                  'upCI', 'downCI', 'mean', 'nFac',
                  'group','position','position_dodge'))
