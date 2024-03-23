library(stylest2)
library(quanteda)

novels_dfm <- dfm(tokens( data(novels) ))

test_that('invalid smooth raises error', {
  expect_error(stylest2_fit(dfm = novels_dfm, smoothing = -1),
               'Smoothing value must be numeric and non-negative.')
  }
)

