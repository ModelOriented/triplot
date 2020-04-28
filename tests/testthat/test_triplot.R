context("Check triplot() functions")

test_that("check calculate_triplot function",{
  library("DALEX")
  library("triplot")

  p <- calculate_triplot(x = apartments_num_lm_model,
               data = apartments_num_mod, new_observation = apartments_num_new_observation)

  expect_true("list" %in% class(p))
})

test_that("check calculate_triplot.explainer function",{
  library("DALEX")
  library("triplot")

  apartments_explainer <- explain(model = apartments_num_lm_model,
                                  data = apartments_num_mod,
                                  verbose = FALSE)

  p <- calculate_triplot(x = apartments_explainer,
                         new_observation = apartments_num_new_observation)

  expect_true("list" %in% class(p))
})

test_that("check calculate_triplot function for FI",{
  library("DALEX")
  library("triplot")
  library("ingredients")

  p <- calculate_triplot(x = apartments_num_lm_model,
                         data = apartments_num_mod,
                         y = apartments[,1])

  expect_true("list" %in% class(p))
})

test_that("check calculate_triplot.explainer function for FI",{
  library("DALEX")
  library("triplot")
  library("ingredients")

  apartments_explainer <- explain(model = apartments_num_lm_model,
                                  data = apartments_num_mod,
                                  y = apartments[,1],
                                  verbose = FALSE)

  p <- calculate_triplot(x = apartments_explainer)

  expect_true("list" %in% class(p))
})


test_that("check plot.calculate_triplot function",{
  library("DALEX")
  library("triplot")
  library("ingredients")

  tri <- calculate_triplot(x = apartments_num_lm_model,
                           data = apartments_num_mod,
                           y = apartments[,1],
                           abbrev_labels = 10)
  p <- plot(tri)

  expect_true("gtable" %in% class(p))
})

