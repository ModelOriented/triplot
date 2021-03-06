#' Creates a cluster tree from numeric features
#'
#' Creates a cluster tree from numeric features and their correlations.
#'
#' @param x dataframe with only numeric columns
#' @param clust_method the agglomeration method to be used
#' see \code{\link[stats]{hclust}} methods
#' @param cor_method the correlation method to be used
#' see \code{\link[stats]{cor}} methods
#'
#' @return an hclust object
#'
#' @importFrom stats hclust
#' @importFrom stats cor
#' @importFrom graphics plot
#' @param ... other parameters
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' cluster_variables(dragons_data, clust_method = "complete")
#'
#' @export

cluster_variables <- function(x, ...)
  UseMethod("cluster_variables")

#' @export
#' @rdname cluster_variables

cluster_variables.default <- function(x, clust_method = "complete", 
                                      cor_method = "spearman", ...) {

  stopifnot(all(sapply(x, is.numeric)))

# build clustering tree ---------------------------------------------------

  x_hc <- hclust(as.dist(1 - abs(cor(x, method = cor_method))),
                 method = clust_method)

  class(x_hc) <- c("cluster_variables", "hclust")
  return(x_hc)
}


#' Plots tree with correlation values
#'
#' Plots tree that illustrates the results of cluster_variables function.
#'
#' @param x \code{cluster_variables} or \code{hclust} object
#' @param p correlation value for cutoff level, if not NULL, cutoff line will
#'   be drawn
#' @param show_labels if TRUE, plot will have annotated axis Y
#' @param axis_lab_size size of labels on axis Y, if applicable
#' @param text_size size of labels annotating values of correlations
#' @param ... other parameters
#'
#' @return plot
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
#' cv <- cluster_variables(dragons_data, clust_method = "complete")
#' plot(cv, p = 0.7)
#'
#' @export

plot.cluster_variables <- function(x,
                                   p = NULL,
                                   show_labels = TRUE,
                                   axis_lab_size = 10,
                                   text_size = 3, ...) {
  stopifnot(p >= 0, p <= 1)
  stopifnot("hclust" %in% class(x))

  y <- xend <- yend <- h <- NULL

# convert tree to dendogram -----------------------------------------------

  dhc <- as.dendrogram(x)
  ddata <- dendro_data(dhc, type = "rectangle")

# get correlation values --------------------------------------------------

  xy_horizon <- ddata$segments[ddata$segments$x == ddata$segments$xend, ]
  additional_x <- mean(xy_horizon[xy_horizon$y == max(xy_horizon$y), ]$x)
  additional_y <- max(xy_horizon$y)
  last_cor_value <- data.frame("x" = additional_x, "yend" = additional_y)
  xy <- xy_horizon[xy_horizon$yend != 0, ][, c(1, 4)]
  xy <- rbind(xy, last_cor_value)
  xy <- cbind(xy, 1 - xy[, 2])
  colnames(xy) <- c("x", "y", "h")


# build plot --------------------------------------------------------------

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
    labs(y = "Hierarchical clustering")

  if (show_labels) {
    cor_plot <- cor_plot +
      geom_text(aes(x = x, y = y, label = label, hjust = 1),
                data = label(ddata), nudge_y = -0.01,
                size = axis_lab_size / .pt,
                colour = theme_drwhy()$axis.title$colour)
  }

  # add line that shows correlation cut off level ---------------------------

  if (!is.null(p)) {
    cor_plot <- cor_plot + geom_hline(yintercept = 1 - p, linetype = "dashed")
  }

  attr(cor_plot, "order") <- x$order
  attr(cor_plot, "labels") <- x$labels

  return(cor_plot)
}


#' Cuts tree at custom height and returns a list
#'
#' This function creates aspect list after cutting a cluster tree of features
#' at a given height.
#'
#' @param x hclust object
#' @param h correlation value for tree cutting
#'
#' @return list of aspects
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' cv <- cluster_variables(dragons_data, clust_method = "complete")
#' list_variables(cv, h = 0.5)
#'
#' @export
#' @importFrom stats cutree


list_variables <- function(x, h) {

  val <- NULL
  clust_list <- cutree(x, h = 1 - h)
  stopifnot(h >= 0, h <= 1)

# prepare a list with aspects grouping ------------------------------------

  df <- data.frame(names(clust_list), unname(clust_list))
  colnames(df) <- c("name", "val")
  res <- vector("list", max(clust_list))
  names(res) <- paste0("aspect.group", seq_along(res))

  for (i in seq_along(res)) {
    res[i] <- list(as.character(subset(df, val == i)$name))
  }

  return(res)
}

#' Helper function that combines clustering variables and creating aspect list
#'
#' Divides correlated features into groups, called aspects. Division is based on
#' correlation cutoff level.
#'
#'
#' @param x hclust object
#' @param h correlation value for tree cutting
#' @param clust_method the agglomeration method to be used
#' see \code{\link[stats]{hclust}} methods
#' @param cor_method the correlation method to be used
#' see \code{\link[stats]{cor}} methods
#'
#' @examples
#' library("DALEX")
#' dragons_data <- dragons[,c(2,3,4,7,8)]
#' group_variables(dragons_data, h = 0.5, clust_method = "complete")
#'
#' @return list with aspect
#' @export


group_variables <- function(x, h, clust_method = "complete", 
                            cor_method = "spearman") {

# make a tree and prepare a list with aspects grouping --------------------

  cv <- cluster_variables(x, clust_method = clust_method, 
                          cor_method = cor_method)
  res <- list_variables(cv, h)

  return(res)
}
