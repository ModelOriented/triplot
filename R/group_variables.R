#' Groups numeric features into aspects
#'
#' Divides correlated features into groups, called aspects. Division is based on
#' correlation cutoff level.
#'
#' @param x dataframe with only numeric columns
#' @param p correlation value for cut-off level
#' @param clust_method the agglomeration method to be used,
#' see \code{\link[stats]{hclust}} methods
#' @param draw_tree if TRUE, function plots tree that illustrates grouping
#' @param draw_abline if TRUE, function plots vertical line at cut-off level
#'
#' @return list of aspects
#'
#' @importFrom stats hclust
#' @importFrom stats cor
#' @importFrom graphics plot
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' group_variables(dragons_data, p = 0.7, clust_method = "complete")
#'
#' @export
#'
#' @rdname group_variables

group_variables <- function(x, p = 0.5, clust_method = "complete",
                            draw_tree = FALSE, draw_abline = TRUE) {
  stopifnot(all(sapply(x, is.numeric)))
  stopifnot(p >= 0, p <= 1)

  # build and cut a tree
  x_hc <- hclust(as.dist(1 - abs(cor(x, method = "spearman"))),
                 method = clust_method)

  res <- custom_tree_cutting(x_hc, p)

  # plot a tree
  if (draw_tree == TRUE) {
    plot(plot_group_variables(x_hc, p, draw_abline))
  }

  return(res)
}


#' Plots tree with correlation values
#'
#' Plots tree that illustrates the results of group_variables function.
#'
#' @param x hclust object
#' @param p correlation value for cutoff level
#' @param show_labels if TRUE, plot will have annotated axis Y
#' @param draw_abline if TRUE, cutoff line will be drawn
#' @param axis_lab_size size of labels on axis Y, if applicable
#' @param text_size size of labels annotating values of correlations
#'
#' @return tree plot
#'
#' @importFrom stats hclust
#' @importFrom ggdendro dendro_data
#' @importFrom ggdendro segment
#' @importFrom ggdendro label
#' @importFrom ggplot2 .pt
#' @importFrom DALEX theme_drwhy
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' group_variables(dragons_data, p = 0.7, clust_method = "complete",
#'                 draw_tree = TRUE)
#'
#' @export

plot_group_variables <- function(x, p, show_labels = TRUE, draw_abline = TRUE,
                                 axis_lab_size = 10, text_size = 3) {
  stopifnot(p >= 0, p <= 1)
  stopifnot(class(x) == "hclust")

  y <- xend <- yend <- h <- NULL

  #convert tree to dendogram
  dhc <- as.dendrogram(x)
  ddata <- dendro_data(dhc, type = "rectangle")

  #get correlation values
  xy <- ddata$segments[ddata$segments$x == ddata$segments$xend, ][, c(1, 4)]
  xy <- xy[xy$yend != 0, ]
  xy <- cbind(xy, 1 - xy[, 2])
  colnames(xy) <- c("x", "y", "h")

  #build plot
  cor_plot <- ggplot(segment(ddata)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = xy, aes(x = x, y = y, label = round(h, digits = 2)),
              hjust = 1.3, size = text_size) +
    coord_flip() +
    scale_y_continuous(expand = c(0.3, 0.3)) +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(colour = "white"),
          axis.ticks.x = element_blank(),
          axis.title.x = theme_drwhy()$axis.title,
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank()) +
    labs(y = "Spearman correlations")

  if (show_labels) {
    cor_plot <- cor_plot +
      geom_text(aes(x = x, y = y, label = label, hjust = 1),
                data = label(ddata), nudge_y = -0.01,
                size = axis_lab_size / .pt,
                colour = theme_drwhy()$axis.title$colour)
  }

  #add line that shows correlation cut off level
  if (draw_abline == TRUE) {
    cor_plot <- cor_plot + geom_hline(yintercept = 1 - p, linetype = "dashed")
  }

  attr(cor_plot, "order") <- x$order
  attr(cor_plot, "labels") <- x$labels

  return(cor_plot)
}


#' Custom tree cutting
#'
#' This function creates aspect list after cutting a tree at a given height.
#'
#' @param x hclust object
#' @param h correlation value for tree cutting
#'
#' @return dataframe with aspect
#'
#' @importFrom stats cutree
#'
#' @noRd

custom_tree_cutting <- function(x, h) {
  val <- NULL
  clust_list <- cutree(x, h = 1 - h)

  #prepare a list with aspects grouping
  df <- data.frame(names(clust_list), unname(clust_list))
  colnames(df) <- c("name", "val")
  res <- vector("list", max(clust_list))
  names(res) <- paste0("aspect.group", seq_along(res))

  for (i in seq_along(res)) {
    res[i] <- list(as.character(subset(df, val == i)$name))
  }

  return(res)
}
