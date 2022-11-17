test_that(
  "The accel_plot() returns a ggplot object.",
  {
    data(ukb_accel)
    p <- accel_plot(ukb_accel[1:100, ])
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The accel_plot() errors when no time or freq column.",
  {
    data(iris)
    expect_error(accel_plot(iris))
  }
)

test_that(
  "The accel_plot() is correct for time-series data.xy",
  {
    data(ukb_accel)
    p <- accel_plot(ukb_accel[1:100, ])
    vdiffr::expect_doppelganger("first-100-samples-time", p)
  }
)


test_that(
  "The accel_plot() returns a tiible data frame object.",
  {
    data(ukb_accel)
    s <- spectral_signature(ukb_accel[1:100, ], take_log = TRUE)
    expect_true(inherits(s, "tbl_df"))
  }
)

test_that(
  "The accel_plot() is correct for time-series data.xy",
  {
    data(ukb_accel)
    s <- spectral_signature(ukb_accel[1:100, ], take_log = FALSE)
    p <- accel_plot(s)
    vdiffr::expect_doppelganger("first-100-samples-specsig", p)
  }
)
