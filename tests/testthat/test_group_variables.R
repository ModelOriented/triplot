context("Check group_variables() functions")

test_that("check group_variables function",{
  library("DALEX")
  library("triplot")

  aspect_list <- group_variables(apartments_num, 0.52, draw_tree = TRUE,
                                 draw_abline = TRUE)
  expect_true(length(aspect_list) == 4)
  expect_error(group_variables(apartments, 0.6))
  expect_error(group_variables(apartments_num, 1.6))

})

test_that("check plot_group_variables function",{
  library("DALEX")
  library("triplot")

  apartments_hc <- hclust(as.dist(1 - abs(cor(apartments_num, method = "spearman"))))
  p <- plot_group_variables(apartments_hc, p = 0.5, draw_abline = TRUE)
  expect_true("ggplot" %in% class(p))
})

test_that("check custom_tree_cutting function",{
  library("DALEX")
  library("triplot")

  apartments_hc <- hclust(as.dist(1 - abs(cor(apartments_num, method = "spearman"))))
  aspect_list <- custom_tree_cutting(apartments_hc, 0.6)
  one_aspect <- custom_tree_cutting(apartments_hc, 0)

  expect_true(class(aspect_list) == "list")
  expect_true(length(aspect_list) == 4)
  expect_true(length(one_aspect) == 1)
  expect_true("surface" %in% aspect_list$aspect.group3)

})
