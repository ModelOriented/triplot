#' Aspects importance for single aspects
#'
#' Calculates aspect_importance for single aspects (every aspect contains only
#' one feature).
#'
#' @param x an explainer created with the \code{DALEX::explain()} function
#' or a model to be explained.
#' @param data dataset, it will be extracted from \code{x} if it's an explainer
#' NOTE: Target variable shouldn't be present in the \code{data}
#' @param predict_function predict function, it will be extracted from \code{x}
#'   if it's an explainer
#' @param new_observation selected observation with columns that corresponds to
#' variables used in the model, should be without target variable
#' @param N number of observations to be sampled (with replacement) from data
#' @param label name of the model. By default it's extracted from the 'class'
#'   attribute of the model.
#' @param sample_method sampling method in \code{\link{get_sample}}
#' @param n_var how many non-zero coefficients for lasso fitting, if zero than
#'   linear regression is used
#' @param f frequency in in \code{\link{get_sample}}
#' @param ... other parameters
#'
#' @return An object of the class 'aspect_importance'. Contains dataframe that
#'   describes aspects' importance.
#'
#' @examples
#' library("DALEX")
#'
#' model_titanic_glm <- glm(survived == 1 ~ class + gender + age +
#'                          sibsp + parch + fare + embarked,
#'                          data = titanic_imputed,
#'                          family = "binomial")
#'
#' aspect_importance_single(model_titanic_glm, data = titanic_imputed[,-8],
#'                          new_observation = titanic_imputed[1,-8])
#'
#' @export

aspect_importance_single <- function(x, ...)
  UseMethod("aspect_importance_single")

#' @export
#' @rdname aspect_importance_single

aspect_importance_single.explainer <- function(x, new_observation,
                                               N = 100,
                                               sample_method = "default",
                                               n_var = 0, f = 2, ...) {

# extracts model, data and predict function from the explainer ------------

  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label


# calls aspect_importance_single function ---------------------------------

  aspect_importance_single.default(x = model, data = data,
                                   predict_function = predict_function,
                                   new_observation = new_observation, N = N,
                                   label = label,
                                   sample_method = sample_method,
                                   n_var = n_var, f = f)
}

#' @export
#' @rdname aspect_importance_single

aspect_importance_single.default <- function(x, data,
                                             predict_function = predict,
                                             new_observation, N = 100,
                                             label = class(x)[1],
                                             sample_method = "default",
                                             n_var = 0,
                                             f = 2, ...) {


# creates list of single aspects ------------------------------------------

  single_aspect_list <- vector("list", length(colnames(data)))
  names(single_aspect_list) <- colnames(data)

  for (i in seq_along(single_aspect_list)) {
    single_aspect_list[i] <- colnames(data)[i]
  }

# calls aspect importance function ----------------------------------------

  res_ai <- aspect_importance(x, data, predict_function,
                              new_observation, single_aspect_list, N,
                              label, sample_method, n_var, f)

# creates data frame with results ------------------------------------------

  res_ai[, 3] <- as.character(res_ai[, 1])
  for (i in c(1:dim(res_ai)[1])) {
    tmp_val <- new_observation[as.character(res_ai[i, 1])]
    if (is.numeric(tmp_val[1, 1])) {
      tmp_val <- round(tmp_val[1, 1], digits = 2)
    } else {
      tmp_val <- as.character(tmp_val[1, 1])
    }
    res_ai[i, 3] <- paste0(res_ai[i, 1], " = ", tmp_val)
  }
  colnames(res_ai)[3] <- "new observation"

  return(res_ai)
}
