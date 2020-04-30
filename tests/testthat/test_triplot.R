context("Check triplot() functions")

test_that("check calculate_triplot.default function",{
  library("DALEX")
  library("triplot")

  apartments_tri <- calculate_triplot(x = apartments_num_lm_model,
                                      data = apartments_num[,-1],
                                      y = apartments_num[, 1],
                                      new_observation =
                                        apartments_num_new_observation[-1])
  expect_true("list" %in% class(apartments_tri))
})

test_that("check calculate_triplot.explainer function",{
  library("DALEX")
  library("triplot")

  apartments_tri <- calculate_triplot(x = apartments_explain,
                                      new_observation =
                                        apartments_num_new_observation[-1])

  expect_true("list" %in% class(apartments_tri))
})

test_that("check calculate_triplot.default function for FI",{
  library("DALEX")
  library("triplot")
  library("ingredients")

  apartments_tri <- calculate_triplot(x = apartments_num_lm_model,
                                      data = apartments_num[,-1],
                                      y = apartments_num[, 1])
  expect_true("list" %in% class(apartments_tri))
})

test_that("check calculate_triplot.explainer function for FI",{
  library("DALEX")
  library("triplot")
  library("ingredients")

  apartments_tri <- calculate_triplot(x = apartments_explain)

  expect_true("list" %in% class(apartments_tri))
})


test_that("check plot.calculate_triplot function",{
  library("DALEX")
  library("triplot")
  library("ingredients")

  apartments_tri <- calculate_triplot(x = apartments_explain,
                                      new_observation =
                                        apartments_num_new_observation[-1])
  p <- plot(apartments_tri)


  expect_true("gtable" %in% class(p))
})

