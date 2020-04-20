context("Check triplot() functions")

test_that("check plot_aspects_importance_grouping function",{
  library("DALEX")
  library("triplot")

  p1 <- plot_aspects_importance_grouping(x = apartments_num_lm_model,
                                         data = apartments_num_mod,
                                         new_observation = apartments_num_new_observation,
                                         cumulative_max = TRUE, absolute_value = TRUE)
  p2 <- plot_aspects_importance_grouping(x = apartments_num_lm_model,
                                         data = apartments_num_mod,
                                         new_observation = apartments_num_new_observation,
                                         cumulative_max = TRUE, absolute_value = FALSE)

  expect_true("ggplot" %in% class(p1))
  expect_true("ggplot" %in% class(p2))
})


test_that("check triplot function",{
  library("DALEX")
  library("triplot")

  p <- triplot(x = apartments_num_lm_model,
               data = apartments_num_mod, new_observation = apartments_num_new_observation)

  expect_true("gtable" %in% class(p))
})

test_that("check triplot.explainer function",{
  library("DALEX")
  library("triplot")

  apartments_explainer <- explain(model = apartments_num_lm_model,
                                  data = apartments_num_mod,
                                  verbose = FALSE)

  p <- triplot(x = apartments_explainer,
               new_observation = apartments_num_new_observation)

  expect_true("gtable" %in% class(p))
})
