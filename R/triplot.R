#' Three plots that sum up automatic aspect/feature importance grouping
#'
#' This function shows: \itemize{ \item plot for aspect_importance or feature_importance with
#' single aspect \item tree that shows aspect_importance for every newly
#' expanded aspect
#' \item clustering tree. }
#'
#' @param x an explainer created with the \code{DALEX::explain()} function
#' or a model to be explained.
#' @param data dataset, it will be extracted from \code{x} if it's an explainer
#' NOTE: Target variable shouldn't be present in the \code{data}
#' @param y true labels for \code{data}, will be extracted from \code{x} if it's an explainer
#' @param predict_function predict function, it will be extracted from \code{x}
#'   if it's an explainer
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model, should be without target variable
#' @param N number of rows to be sampled from data
#' @param clust_method the agglomeration method to be used, see
#'   \code{\link[stats]{hclust}} methods
#' @param absolute_value if TRUE, aspect importance values will be drawn as
#'   absolute values
#' @param cumulative_max if TRUE, aspect importance shown on tree will be max
#'   value of children and node aspect importance values
#' @param add_importance_labels if TRUE, first plot is annotated with values of aspects
#'   importance
#' @param show_axis_y_duplicated_labels if TRUE, every plot will have annotated
#'   axis Y
#' @param abbrev_labels if greater than 0, labels for axis Y in single aspect
#'   importance plot will be abbreviated according to this parameter
#' @param axis_lab_size size of labels on axis
#' @param text_size size of labels annotating values of aspects importance and
#'   correlations
#' @param ... other parameters
#'
#' @import stats
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom graphics plot
#' @importFrom ingredients feature_importance
#'
#' @examples
#' library(DALEX)
#' apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]
#' apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)
#' apartments_num_new_observation <- apartments_num[30,-1]
#' apartments_num_mod <- apartments_num[,-1]
#' triplot(x = apartments_num_lm_model,
#'   data = apartments_num_mod,
#'   new_observation = apartments_num_new_observation,
#'   add_importance_labels = FALSE)
#'
#' triplot(x = apartments_num_lm_model,
#'   data = apartments_num_mod,
#'   y = apartments_num[,1],
#'   show_axis_y_duplicated_labels = TRUE)
#'
#'
#' @export

triplot <- function(x, ...)
  UseMethod("triplot")

#' @export
#' @rdname triplot

triplot.explainer <- function(x, new_observation = NULL, N = 500,
                              clust_method = "complete",
                              absolute_value = FALSE, cumulative_max = FALSE,
                              add_importance_labels = TRUE,
                              show_axis_y_duplicated_labels = FALSE,
                              axis_lab_size = 10,
                              text_size = 3,
                              ...) {

  # extracts model, data and predict function from the explainer
  data <- x$data
  model <- x$model
  predict_function <- x$predict_function

  if (is.null(new_observation)) {
    y <- x$y
  } else {
    y <- NULL
  }

  # calls target function
  triplot.default(model, data, y, predict_function, new_observation, N,
                  clust_method, absolute_value = FALSE, cumulative_max = FALSE,
                  add_importance_labels, show_axis_y_duplicated_labels,
                  axis_lab_size = axis_lab_size, text_size = text_size)
}

#' @export
#' @rdname triplot


triplot.default <- function(x, data, y = NULL, predict_function = predict,
                            new_observation = NULL,
                            N = 500, clust_method = "complete",
                            absolute_value = FALSE, cumulative_max = FALSE,
                            add_importance_labels = TRUE,
                            show_axis_y_duplicated_labels = FALSE,
                            abbrev_labels = 0,
                            axis_lab_size = 10,
                            text_size = 3,
                            ...) {

  stopifnot(all(sapply(data, is.numeric)))

# Build second plot -------------------------------------------------------


  p2 <- plot_aspects_importance_grouping(x, data, y, predict_function,
                                         new_observation, N, clust_method,
                                         absolute_value,
                                         cumulative_max,
                                         show_labels = show_axis_y_duplicated_labels,
                                         axis_lab_size = axis_lab_size,
                                         text_size = text_size)

  if (is.null(new_observation)) {
    p2$labels$y <- "Hierarchical feature importance"
  } else {
    p2$labels$y <- "Hierarchical aspect importance"
  }

  p2 <- p2 + theme(axis.title = element_text(size = axis_lab_size))

  p2

# Build third plot -------------------------------------------------------

  x_hc <- hclust(as.dist(1 - abs(cor(data, method = "spearman"))),
                 method = clust_method)
  p3 <- plot_group_variables(x_hc, 0, show_labels = show_axis_y_duplicated_labels,
                             draw_abline = FALSE, text_size = text_size,
                             axis_lab_size = axis_lab_size)
  p3 <- p3 + theme(axis.title = element_text(size = axis_lab_size))

# Build first plot --------------------------------------------------------

  if (is.null(new_observation)) {
    importance_leaves <- feature_importance(x = x, data = data, y = y)
    p1 <- plot(importance_leaves, show_boxplots = FALSE, subtitle = "",
               title = "")
    p1$theme$text$size <- text_size

    p1$labels$y <- "Feature importance"
    p1 <- p1 + theme(axis.text = element_text(size = axis_lab_size),
                     axis.title = element_text(size = axis_lab_size),
                     strip.background = element_blank(),
                     strip.text.x = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.title = element_blank()) +
      scale_x_discrete(expand = expand_scale(mult = .01))

    order_mod <- attr(p3, "labels")[reorder(attr(p3, "labels"), attr(p3, "order"))]
    order_mod <-  match(order_mod, p1$data$variable)
    lev_mod <- p1$data$variable[order_mod]
    p1$data$variable <- factor(p1$data$variable,
                                     levels = lev_mod)
    } else {

    importance_leaves <- aspect_importance_single(x, data,
                                                         predict_function,
                                                         new_observation, N,
                                                         label = "")
    p1 <- plot(importance_leaves, add_importance = add_importance_labels,
               text_size = text_size)
    p1$labels$y <- "Single aspects importance"
    if (abbrev_labels > 0) {
      p1$data$`new observation` <- abbreviate(p1$data$`new observation`,
                                              minlength = abbrev_labels)
    }

    order_mod <- attr(p3, "labels")[reorder(attr(p3, "labels"), attr(p3, "order"))]
    order_mod <-  match(order_mod, p1$data$aspects)
    lev_mod <- p1$data$`new observation`[order_mod]
    p1$data$`new observation` <- factor(p1$data$`new observation`,
                                        levels = lev_mod)
    p1$data$aspects <- p1$data$`new observation`
    p1 <- p1 + theme(axis.text = element_text(size = axis_lab_size),
                     axis.title = element_text(size = axis_lab_size)) +
      scale_x_discrete(expand = expand_scale(mult = .01))
  }


  # # Plot
  plot_list <- list(p1, p2, p3)
  do.call("grid.arrange", c(plot_list, nrow = 1, top = "Triplot"))


}

#' Function plots tree with aspect/feature importance values
#'
#' This function plots tree that shows order of feature grouping and aspect
#' importance or feature importance values of every newly created aspect.
#'
#' @param x a model to be explained
#' @param data dataset, should be without target variable
#' @param y true labels for \code{data}, will be extracted from \code{x} if it's an explainer,
#'   need to be provided if feature importance is to be calculated
#' @param predict_function predict function
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model, should be without target variable, if NULL feature_importance will be calculated
#' @param N number of observations to be sampled (with replacement) from data
#' @param clust_method the agglomeration method to be used, see
#'   \code{\link[stats]{hclust}} methods
#' @param absolute_value if TRUE, aspect importance values will be drawn as
#'   absolute values
#' @param cumulative_max if TRUE, aspect importance shown on tree will be max
#'   value of children and node aspect importance values
#' @param show_labels if TRUE, plot will have annotated axis Y
#' @param axis_lab_size size of labels on axis Y, if applicable
#' @param text_size size of labels annotating values of aspects importance
#'
#' @return ggplot
#'
#' @import stats
#' @import ggplot2
#' @importFrom ggdendro dendro_data
#' @importFrom ggdendro segment
#' @importFrom ggdendro label
#' @importFrom DALEX theme_drwhy
#' @importFrom ingredients feature_importance
#'
#' @examples
#' library(DALEX)
#' apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]
#' apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)
#' apartments_num_new_observation <- apartments_num[2,-1]
#' apartments_num_mod <- apartments_num[,-1]
#' plot_aspects_importance_grouping(x = apartments_num_lm_model,
#' data = apartments_num_mod, new_observation = apartments_num_new_observation)
#'
#'
#' @export
#'


plot_aspects_importance_grouping <- function(x, data, y = NULL,
                                             predict_function = predict,
                                             new_observation = NULL, N = 100,
                                             clust_method = "complete",
                                             absolute_value = FALSE,
                                             cumulative_max = FALSE,
                                             show_labels = TRUE,
                                             axis_lab_size = 10,
                                             text_size = 3) {

# Building helper objects ---------------------------------------------

  predicted <- y
  y <- xend <- yend <- yend_val <- NULL

  if (is.null(new_observation)) {
    importance_leaves <- feature_importance(x = x, data = data, y = predicted)
    } else {
      importance_leaves <- aspect_importance_single(x, data,
                                                  predict_function,
                                                  new_observation, N)
  }

  x_hc <- hclust(as.dist(1 - abs(cor(data, method = "spearman"))),
                 method = clust_method)
  cutting_heights <- x_hc$height
  aspects_list_previous <-  custom_tree_cutting(x_hc, 1)
  int_node_importance <- as.data.frame(NULL)


# Calculating aspect importance -------------------------------------------

  for (i in c(1:(length(cutting_heights) - 1))) {

    aspects_list_current <- custom_tree_cutting(x_hc, 1 - cutting_heights[i])

    t1 <- match(aspects_list_current, setdiff(aspects_list_current,
                                              aspects_list_previous))
    t2 <- which(t1 == 1)
    t3 <- aspects_list_current[t2]
    group_name <- names(t3)

    if (is.null(new_observation)) {
      res_ai <- feature_importance(x = x, data = data, y = predicted,
                       variable_groups = aspects_list_current)
      res_ai <- res_ai[!(substr(res_ai$variable,1,1) == "_"),]
      res_ai <- res_ai[res_ai$permutation == "0", ]

      int_node_importance[i, 1] <- res_ai[res_ai$variable == group_name, ]$dropout_loss
    } else {
      res_ai <- aspect_importance(x = x, data = data,
                                  predict_function = predict_function,
                                  new_observation = new_observation,
                                  aspects = aspects_list_current, N = N)
      int_node_importance[i, 1] <- res_ai[res_ai$aspects == group_name, ]$importance
    }

    int_node_importance[i, 2] <- group_name
    int_node_importance[i, 3] <- cutting_heights[i]
    aspects_list_previous <- aspects_list_current
  }

  int_node_importance[length(cutting_heights), 1] <- NA


# Inserting importance values into x_hc tree ------------------------------

  x_hc$height <- int_node_importance$V1


# Modifing importance -----------------------------------------------------

  #if cumulative_max/absolute_value are true

  if (cumulative_max == TRUE) {
    for (i in seq_along(x_hc$height)) {
      if (x_hc$merge[i, 1] < 0) {
        a1 <- importance_leaves[
          importance_leaves[, 1] == x_hc$labels[-x_hc$merge[i, 1]], ]$importance
      } else {
        a1 <- x_hc$height[x_hc$merge[i, 1]]
      }
      if (x_hc$merge[i, 2] < 0) {
        a2 <- importance_leaves[
          importance_leaves[, 1] == x_hc$labels[-x_hc$merge[i, 2]], ]$importance
      } else {
        a2 <- x_hc$height[x_hc$merge[i, 2]]
      }
      a3 <- x_hc$height[i]

      if (absolute_value == TRUE) {
        x_hc$height[i] <- max(abs(a1), abs(a2), abs(a3))
      } else {
        x_hc$height[i] <- max(a1, a2, a3)
      }
    }}


# Building dendogram ------------------------------------------------------

  dend_mod <- NULL
  dend_mod <- as.dendrogram(x_hc, hang = -1)
  ddata <- dendro_data(dend_mod, type = "rectangle")



# Preparing labels --------------------------------------------------------

  ai_labels <-  na.omit(segment(ddata))
  ai_labels <- ai_labels[ai_labels$yend != 0, ]
  ai_labels$yend_val <- ai_labels$yend


# Removing values of aspect_importance for single aspects -----------------

  cc_vector <- !complete.cases(ddata$segments)
  ddata$segments[cc_vector, ] <- 1
  ddata$segments[min(which(cc_vector == TRUE)), c(1, 3)] <- min(ddata$labels$x)
  ddata$segments[min(which(cc_vector == TRUE)) + 1, c(1, 3)] <- max(ddata$labels$x)

# Modifing importance -----------------------------------------------------

  #if absolute_value is true

  if (absolute_value == TRUE) {
    ddata$segments$y <- abs(ddata$segments$y)
    ddata$segments$yend <- abs(ddata$segments$yend)
    ai_labels$yend_val <- abs(ai_labels$yend)
  }


# Adding new observation values to labels ---------------------------------

  if (!is.null(new_observation)) {
    ddata$labels[, 3] <- as.character(ddata$labels[, 3])
    for (i in seq_along(ddata$labels[, 3])) {
      ddata$labels[i, 3] <- paste0(ddata$labels[i, 3], " = ",
                                 round(new_observation[ddata$labels[i, 3]],
                                       digits = 2))
    }
  }


# Moving labels -----------------------------------------------------------

  nudge_value <- ifelse(min(ddata$segments$yend) == 0, -0.2,
                        min(ddata$segments$yend) * 1.35)


# Building plot -----------------------------------------------------------

  p <- ggplot(segment(ddata)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = ai_labels, aes(x = x, y = yend_val,
                                    label = round(yend, digits = 2)),
              hjust = 1.3, size = text_size) +
    coord_flip() +
    scale_y_continuous(expand = c(.3, .3)) +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(colour = "white"),
      axis.title.x = theme_drwhy()$axis.title,
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank()
    ) +
    labs(y = "Aspect importance for correlated variables")

  if (show_labels) {
    p <- p + geom_text(aes(x = x, y = y, label = label, hjust = 1),
                       data = label(ddata),
                       colour = theme_drwhy()$axis.title$colour,
                       size = axis_lab_size / .pt, nudge_y = nudge_value)
  }

  return(p)
}
