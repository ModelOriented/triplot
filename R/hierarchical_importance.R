#' Calculates importance of hierarchically grouped aspects
#'
#' This function plots tree that shows order of feature grouping and aspect
#' importance or feature importance values of every newly created aspect.
#'
#' @param x a model to be explained
#' @param data dataset, should be without target variable
#' @param y true labels for \code{data}, will be extracted from \code{x}
#' if it's an explainer,
#'   need to be provided if feature importance is to be calculated
#' @param predict_function predict function
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model (it should be without target variable),
#'   if NULL feature_importance will be calculated
#' @param N number of observations to be sampled (with replacement) from data
#' @param clust_method the agglomeration method to be used, see
#'   \code{\link[stats]{hclust}} methods
#' @param absolute_value if TRUE, aspect importance values will be drawn as
#'   absolute values
#' @param show_labels if TRUE, plot will have annotated axis Y
#' @param add_last_group if TRUE, plot will draw connecting line between last
#' two groups
#' @param axis_lab_size size of labels on axis Y, if applicable
#' @param text_size size of labels annotating values of aspects importance
#' @param ... other parameters
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
#' apartments_num_new_observation <- apartments_num[2,]
#' hi <- hierarchical_importance(x = apartments_num_lm_model,
#'  data = apartments_num[,-1],
#'  new_observation = apartments_num_new_observation[-1])
#' plot(hi, add_last_group = TRUE, absolute_value = TRUE)
#'
#' @export
#'


hierarchical_importance <- function(x, ...)
  UseMethod("hierarchical_importance")

#' @export
#' @rdname hierarchical_importance

hierarchical_importance <- function(x, data, y = NULL,
                                    predict_function = predict,
                                    new_observation = NULL,
                                    N = 1000,
                                    clust_method = "complete", ...) {

  # Building helper objects ---------------------------------------------

  x_hc <- hclust(as.dist(1 - abs(cor(data, method = "spearman"))),
                 method = clust_method)
  cutting_heights <- x_hc$height
  aspects_list_previous <-  list_variables(x_hc, 1)
  int_node_importance <- as.data.frame(NULL)

  # Calculating aspect importance -------------------------------------------

  for (i in c(1:(length(cutting_heights) - 1))) {

    aspects_list_current <- list_variables(x_hc, 1 - cutting_heights[i])

    t1 <- match(aspects_list_current, setdiff(aspects_list_current,
                                              aspects_list_previous))
    t2 <- which(t1 == 1)
    t3 <- aspects_list_current[t2]
    group_name <- names(t3)

    if (is.null(new_observation)) {
      res_ai <- feature_importance(x = x, data = data, y = y,
                                   predict_function = predict_function,
                                   variable_groups = aspects_list_current,
                                   n_sample = N)
      res_ai <- res_ai[!(substr(res_ai$variable, 1, 1) == "_"), ]
      res_ai <- res_ai[res_ai$permutation == "0", ]

      int_node_importance[i, 1] <-
        res_ai[res_ai$variable == group_name, ]$dropout_loss
    } else {
      res_ai <- aspect_importance(x = x, data = data,
                                  predict_function = predict_function,
                                  new_observation = new_observation,
                                  variable_groups = aspects_list_current, N = N)
      int_node_importance[i, 1] <-
        res_ai[res_ai$variable_groups == group_name, ]$importance
    }

    int_node_importance[i, 2] <- group_name
    int_node_importance[i, 3] <- cutting_heights[i]
    aspects_list_previous <- aspects_list_current
  }

  int_node_importance[length(cutting_heights), 1] <- NA

  # Inserting importance values into x_hc tree ------------------------------

  x_hc$height <- int_node_importance$V1

  class(x_hc) <- c("hierarchical_importance", "hclust")
  return(x_hc)
}


#' @export
#' @rdname hierarchical_importance
#'

plot.hierarchical_importance <- function(x, new_observation = NULL,
                                         absolute_value = FALSE,
                                         show_labels = TRUE,
                                         add_last_group = FALSE,
                                         axis_lab_size = 10,
                                         text_size = 3, ...) {

  x_hc <- x
  x <- y <- xend <- yend <- yend_val <- NULL

  # Building dendogram ------------------------------------------------------

  dend_mod <- NULL
  dend_mod <- as.dendrogram(x_hc, hang = -1)
  ddata <- dendro_data(dend_mod, type = "rectangle")

  # Modifing importance -----------------------------------------------------

  if (absolute_value == TRUE) {
    ddata$segments$y <- abs(ddata$segments$y)
    ddata$segments$yend <- abs(ddata$segments$yend)
  }

  # Preparing labels --------------------------------------------------------

  ai_labels <-  na.omit(segment(ddata))
  ai_labels <- ai_labels[ai_labels$yend != 0, ]
  ai_labels$yend_val <- ai_labels$yend

  # replace NAs      --------------------------------------------------------

  if (add_last_group) {
    ifelse(max(abs(ai_labels$yend)) > max(ai_labels$yend),
           last_val <- -max(abs(ai_labels$yend)) * 1.05,
           last_val <- max(ai_labels$yend) * 1.05)
    ddata$segments[is.na(ddata$segments)] <- last_val
  } else {
    cc_vector <- !complete.cases(ddata$segments)
    ddata$segments[cc_vector, ] <- 1
    ddata$segments[min(which(cc_vector == TRUE)), c(1, 3)] <-
      min(ddata$labels$x)
    ddata$segments[min(which(cc_vector == TRUE)) + 1, c(1, 3)] <-
      max(ddata$labels$x)
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
