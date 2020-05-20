#' Calculate triplot that sums up automatic aspect/feature importance grouping
#'
#' This function shows:
#' \itemize{ \item plot for the importance of single variables,
#' \item tree that shows importance for every newly expanded group of variables,
#' \item clustering tree. }
#'
#' @param x an explainer created with the \code{DALEX::explain()} function
#' or a model to be explained.
#' @param data dataset, it will be extracted from \code{x} if it's an explainer
#' NOTE: Target variable shouldn't be present in the \code{data}
#' @param y true labels for \code{data}, will be extracted from \code{x}
#' if it's an explainer
#' @param predict_function predict function, it will be extracted from \code{x}
#'   if it's an explainer
#' @param label name of the model. By default it's extracted from the 'class'
#'   attribute of the model.
#' @param type if \code{predict} then aspect_importance is used, if
#'   \code{model} than feature_importance is calculated
#' @param new_observation selected observation with columns that corresponds to
#'   variables used in the model, should be without target variable
#' @param N number of rows to be sampled from data
#'   NOTE: Small \code{N} may cause unstable results.
#' @param loss_function a function that will be used to assess variable
#'   importance, if \code{type = model}
#' @param B integer, number of permutation rounds to perform on each variable
#'   in feature importance calculation, if \code{type = model}
#' @param fi_type character, type of transformation that should be applied for
#'   dropout loss, if \code{type = model}. "raw" results raw drop losses,
#'   "ratio" returns \code{drop_loss/drop_loss_full_model}.
#' @param clust_method the agglomeration method to be used, see
#'   \code{\link[stats]{hclust}} methods
#' @param ... other parameters
#'
#' @import stats
#' @importFrom DALEX feature_importance
#' @importFrom DALEX explain
#' 
#' @return triplot object
#'
#' @examples
#'
#' library(DALEX)
#' set.seed(123)
#' apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]
#' apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)
#' apartments_num_new_observation <- apartments_num[30, ]
#' explainer_apartments <- explain(model = apartments_num_lm_model,
#'                                 data = apartments_num[,-1],
#'                                 y = apartments_num[, 1],
#'                                 verbose = FALSE)
#' apartments_tri <- calculate_triplot(x = explainer_apartments,
#'                                     new_observation =
#'                                       apartments_num_new_observation[-1])
#'
#' @export

calculate_triplot <- function(x, ...)
  UseMethod("calculate_triplot")

#' @export
#' @rdname calculate_triplot

calculate_triplot.explainer <- function(x,
                                        type = c("predict", "model"),
                                        new_observation = NULL,
                                        N = 1000,
                                        loss_function =
                                          DALEX::loss_root_mean_square,
                                        B = 10,
                                        fi_type =
                                          c("raw", "ratio", "difference"),
                                        clust_method = "complete",
                                        ...) {
  
  type <- match.arg(type)
  
  # extracts model, data and predict function from the explainer ------------
  
  data <- x$data
  model <- x$model
  predict_function <- x$predict_function
  y <- x$y
  label <- x$label
  
  # check if target is in data ----------------------------------------------
  
  if (!is.null(y)) {
    target_in_data_check <- any(apply(data, 2, function(z) {
      all(as.character(z) == as.character(y))
    }))
    
    if (target_in_data_check) {
      warning("It is recommended to pass `data` without the target variable 
              column")
    }
  }
  
  # calls target function ---------------------------------------------------
  
  calculate_triplot.default(x = model, data = data, y = y,
                            predict_function = predict_function,
                            type = type,
                            new_observation = new_observation,
                            N = N,
                            loss_function = loss_function,
                            B = B,
                            fi_type = fi_type,
                            clust_method = clust_method,
                            label = label)
}

#' @export
#' @rdname calculate_triplot

calculate_triplot.default <- function(x, data, y = NULL,
                                      predict_function = predict,
                                      label = class(x)[1],
                                      type = c("predict", "model"),
                                      new_observation = NULL,
                                      N = 1000,
                                      loss_function = 
                                        DALEX::loss_root_mean_square,
                                      B = 10,
                                      fi_type = c("raw", "ratio", "difference"),
                                      clust_method = "complete",
                                      ...) {
  
  type <- match.arg(type)
  fi_type <- match.arg(fi_type)
  stopifnot(all(sapply(data, is.numeric)))
  
  # Calculations for second plot ----------------------------------------------
  
  hi <- hierarchical_importance(x = x, data = data, y = y,
                                predict_function = predict_function,
                                type = type,
                                new_observation = new_observation,
                                N = N,
                                loss_function = loss_function,
                                B = B,
                                fi_type = fi_type,
                                clust_method = clust_method)
  
  # Calculations for third plot -----------------------------------------------
  
  cv <- cluster_variables(data, clust_method)
  
  # Calculations for first plot -----------------------------------------------
  
  if (type != "predict") {
    
    explainer <- explain(model = x, data = data, y = y,
                         predict_function = predict_function,
                         verbose = FALSE)
    
    importance_leaves <- feature_importance(explainer = explainer,
                                            n_sample = N,
                                            loss_function = loss_function,
                                            B = B,
                                            type = fi_type)
  } else {

    importance_leaves <- aspect_importance_single(x, data,
                                                  predict_function,
                                                  new_observation, N,
                                                  label = "")
  }

# returns list of plots ---------------------------------------------------

  tri_data <- list(
    single_importance = importance_leaves,
    hierarchical_tree_data = hi,
    cluestering_tree_data = cv,
    new_observation = new_observation,
    triplot_type = type,
    label = label)

  class(tri_data) <- c("triplot", "list")

  invisible(tri_data)

}



# print triplot object ------------------------------------------


#' @export
#' @rdname calculate_triplot

print.triplot <- function(x, ...) {

  stopifnot("triplot" %in% class(x))
  
  
  cat("Triplot object ")
  cat("for model:", x$label, "\n\n")
  
  if (x$triplot_type == "model") {
    cat("Triplot is calculated at model level.\n")
  } else {
    cat("Triplot is calculated for single prediction:\n")
    print(x$new_observation)
  }
  
  invisible(x)

}



#' Plots triplot
#'
#' Plots triplot that sum up automatic aspect/feature importance grouping
#'
#' @param x triplot object
#' @param absolute_value if TRUE, aspect importance values will be drawn as
#'   absolute values
#' @param add_importance_labels if TRUE, first plot is annotated with values of
#'   aspects importance on the bars
#' @param show_model_label if TRUE, adds subtitle with model label
#' @param abbrev_labels if greater than 0, labels for axis Y in single aspect
#'   importance plot will be abbreviated according to this parameter
#' @param add_last_group if TRUE and \code{type = predict}, plot will
#'   draw connecting line between last two groups at the level of 105% of the
#'   biggest importance value, for \code{model} this line is always drawn at
#'   the baseline value
#' @param axis_lab_size size of labels on axis
#' @param text_size size of labels annotating values of aspects importance and
#'   correlations
#' @param bar_width bar width in the first plot
#' @param margin_mid size of a right margin of a middle plot
#' @param ... other parameters
#'
#' @return plot
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom graphics plot
#'
#' @examples
#' library(DALEX)
#' set.seed(123)
#' apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))]
#' apartments_num_lm_model <- lm(m2.price ~ ., data = apartments_num)
#' apartments_num_new_observation <- apartments_num[30, ]
#' explainer_apartments <- explain(model = apartments_num_lm_model,
#'                                 data = apartments_num[,-1],
#'                                 y = apartments_num[, 1],
#'                                 verbose = FALSE)
#' apartments_tri <- calculate_triplot(x = explainer_apartments,
#'  new_observation = apartments_num_new_observation[-1])
#' plot(apartments_tri)
#'
#' @export
#'

plot.triplot <- function(x,
                         absolute_value = FALSE,
                         add_importance_labels = FALSE,
                         show_model_label = FALSE,
                         abbrev_labels = 0,
                         add_last_group = TRUE,
                         axis_lab_size = 10,
                         text_size = 3,
                         bar_width = 5,
                         margin_mid = 0.3,
                         ...) {

  importance_leaves <- x[[1]]
  hi <- x[[2]]
  cv <- x[[3]]
  new_observation <- x[[4]]
  type <- x[[5]]
  model_label <- x[[6]]

  # Builds second plot ------------------------------------------------------

  p2 <- plot(x = hi, new_observation = new_observation,
             absolute_value = absolute_value,
             show_labels = FALSE,
             add_last_group = add_last_group,
             axis_lab_size = axis_lab_size,
             text_size = text_size)

  if (type != "predict") {
    p2$labels$y <- "Hierarchical feature importance"
  } else {
    p2$labels$y <- "Hierarchical aspect importance"
  }

  p2 <- p2 + theme(axis.title = element_text(size = axis_lab_size))  

  # Builds third plot -------------------------------------------------------

  p3 <- plot(cv, show_labels = FALSE,
             axis_lab_size = axis_lab_size,
             text_size = text_size)
  p3 <- p3 + theme(axis.title = element_text(size = axis_lab_size))  

  # Builds first plot -------------------------------------------------------

  if (type != "predict") {
    p1 <- plot(importance_leaves, show_boxplots = FALSE, subtitle = "",
               title = "", bar_width = bar_width)
    p1$theme$text$size <- text_size

    p1$labels$y <- "Feature importance"
    
    p1 <- p1 + theme(axis.text = element_text(size = axis_lab_size),
                     axis.title = element_text(size = axis_lab_size),
                     strip.background = element_blank(),
                     strip.text.x = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                    plot.title = element_blank()) 

    order_mod <-
      attr(p3, "labels")[reorder(attr(p3, "labels"), attr(p3, "order"))]
    order_mod <-  match(order_mod, p1$data$variable)
    lev_mod <- p1$data$variable[order_mod]
    p1$data$variable <- factor(p1$data$variable,
                               levels = lev_mod)

  } else {
    p1 <- plot(importance_leaves, add_importance = add_importance_labels,
               text_size = text_size, bar_width = bar_width)
    p1$labels$y <- "Single aspects importance"
    if (abbrev_labels > 0) {
      p1$data$`new observation` <- abbreviate(p1$data$`new observation`,
                                              minlength = abbrev_labels)
    }

    order_mod <-
      attr(p3, "labels")[reorder(attr(p3, "labels"), attr(p3, "order"))]
    order_mod <-  match(order_mod, p1$data$variable_groups)
    lev_mod <- p1$data$`new observation`[order_mod]
    p1$data$`new observation` <- factor(p1$data$`new observation`,
                                        levels = lev_mod)
    p1$data$variable_groups <- p1$data$`new observation`
    p1 <- p1 + theme(axis.text = element_text(size = axis_lab_size),
                    axis.title = element_text(size = axis_lab_size)) 
  }


# change margins ----------------------------------------------------------

  expansion_parameter <- 0.5
  
  p1 <- p1 + scale_x_discrete(expand = expansion(add = expansion_parameter)) 
  p2 <- p2 + scale_x_continuous(expand = expansion(add = expansion_parameter))
  p3 <- p3 + scale_x_continuous(expand = expansion(add = expansion_parameter))
  
  suppressMessages(p1 <- p1 + 
                     scale_y_continuous(expand = expansion(add = c(0, 0.5))))
  suppressMessages(p2 <- p2 + 
                       scale_y_continuous(expand = 
                                            expansion(mult = c(margin_mid, 0))))
  suppressMessages(p3 <- p3 + 
                       scale_y_continuous(expand = expansion(mult = c(0.1, 0))))

# plot --------------------------------------------------------------------

  p <- p1 + p2 + p3 + 
    plot_annotation(title = "Triplot",
                    theme = theme(plot.title = element_text(hjust = 0.5),
                                  text = theme_drwhy()$plot.title))
  
  if (show_model_label) {
    p <- p + plot_annotation(subtitle = model_label)
  }
  
  p
  
}

#' @export
#' @rdname calculate_triplot

model_triplot <- function(x, ...) {
  calculate_triplot(x, type = "model", ...)
}

#' @export
#' @rdname calculate_triplot
#'
predict_triplot <- function(x, ...) {
  calculate_triplot(x, type = "predict", ...)
}