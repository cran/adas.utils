test_that("chauvenet works (no outlier)", {
  set.seed(0)
  sink(nullfile())
  c <- chauvenet(rnorm(1000))
  sink()
  expect_false(c$reject)
})

test_that("chauvenet works (outlier)", {
  set.seed(0)
  sink(nullfile())
  c <- chauvenet(c(rnorm(999), 4))
  sink()
  expect_true(c$reject)
})

test_that("daniel_plot_qq works", {
  set.seed(0)
  sink(nullfile())
  daniel_plot_qq(lm(Y~A*B*C*D, data=filtration))
  sink()
  expect_true(TRUE)
})

test_that("daniel_plot_hn works", {
  set.seed(0)
  sink(nullfile())
  daniel_plot_hn(lm(Y~A*B*C*D, data=filtration))
  sink()
  expect_true(TRUE)
})

test_that("pareto_chart for lm works", {
  set.seed(0)
  sink(nullfile())
  pareto_chart(lm(Y~A*B*C*D, data=filtration))
  sink()
  expect_true(TRUE)
})

test_that("pareto_chart for tibble works", {
  set.seed(0)
  sink(nullfile())
  tibble(
    val=rnorm(10, sd=5),
    cat=LETTERS[1:length(val)]
  ) %>%
    ggplot(aes(x=cat, y=val)) +
    geom_pareto_bars() +
    geom_pareto_line() +
    geom_pareto_point() +
    scale_y_pareto()
  sink()
  expect_true(TRUE)
})


test_that("normplot works", {
  set.seed(0)
  sink(nullfile())
  df <- tibble(
    xn = rnorm(100, mean=20, sd=5),
  )

  df %>% normplot(xn)
  sink()
  expect_true(TRUE)
})

test_that("ggTukey: .by, group_by() and deprecated splt agree", {
  p_by <- ggTukey(battery, Response ~ Material, .by = Temperature)
  p_grp <- ggTukey(dplyr::group_by(battery, Temperature), Response ~ Material)
  expect_warning(
    p_splt <- ggTukey(battery, Response ~ Material, splt = ~Temperature),
    "deprecated"
  )
  expect_identical(ggplot2::layer_data(p_by, 2), ggplot2::layer_data(p_grp, 2))
  expect_identical(ggplot2::layer_data(p_by, 2), ggplot2::layer_data(p_splt, 2))
  expect_equal(nrow(ggplot2::layer_data(p_by, 2)), 9)
})

test_that("ggTukey facets extra grouping variables", {
  b <- dplyr::mutate(battery, Lab = rep(c("L1", "L2"), each = 18))
  p <- ggTukey(b, Response ~ Material, .by = c(Temperature, Lab))
  expect_s3_class(p$facet, "FacetWrap")
  expect_error(
    ggTukey(dplyr::group_by(b, Lab), Response ~ Material, .by = Temperature),
    "grouped"
  )
})

test_that("ggTukey labels the axis with the `which` term", {
  p <- ggTukey(battery, Response ~ Material + Temperature, which = 2, .by = Repeat)
  expect_equal(p$labels$x, "Temperature")
})

test_that("stat_tukey matches TukeyHSD per group", {
  g <- ggplot2::ggplot(battery, ggplot2::aes(response = Response, factor = Material, colour = Temperature)) +
    geom_tukey(conf.level = 0.99)
  ld <- ggplot2::layer_data(g, 2)
  expect_equal(nrow(ld), 9)
  ref <- TukeyHSD(aov(Response ~ Material, data = battery[battery$Temperature == "70", ]),
                  conf.level = 0.99)[[1]]
  got <- ld[ld$group == 2, ]
  expect_equal(got$comparison, rownames(ref))
  expect_equal(got$x, unname(ref[, "diff"]))
  expect_equal(got$xmin, unname(ref[, "lwr"]))
  expect_equal(got$xmax, unname(ref[, "upr"]))
  labels <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]$y$get_labels()
  expect_equal(labels, c("2-1", "3-1", "3-2"))
})
