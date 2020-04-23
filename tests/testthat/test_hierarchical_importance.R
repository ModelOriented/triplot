context("Check hierarchical_importance() functions")

test_that("check hierarchical_importance function for aspects_importance",{
  library("DALEX")
  library("triplot")

  hi <- hierarchical_importance(x = apartments_num_lm_model,
                                data = apartments_num_mod,
                                new_observation = apartments_num_new_observation)

  expect_true("hclust" %in% class(hi))
  expect_true("hierarchical_importance" %in% class(hi))
  expect_true("floor" %in% hi$labels)
})

test_that("check hierarchical_importance function for feature_importance",{
  library("DALEX")
  library("triplot")

  hi <- hierarchical_importance(x = apartments_num_lm_model,
                                data = apartments_num_mod,
                                y = apartments[,1])

  expect_true("hclust" %in% class(hi))
  expect_true("hierarchical_importance" %in% class(hi))
  expect_true("floor" %in% hi$labels)
})


test_that("check plot.hierarchical_importance  function",{
  library("DALEX")
  library("triplot")

  hi <- hierarchical_importance(x = apartments_num_lm_model,
                                data = apartments_num_mod,
                                y = apartments[,1])
  p <- plot(hi, add_last_group = TRUE)

  expect_true("ggplot" %in% class(p))
})

