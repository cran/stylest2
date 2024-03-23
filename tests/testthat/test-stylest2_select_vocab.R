library(stylest2)
library(quanteda)

novels_dfm <- dfm(tokens( data(novels) ))

test_that('invalid nfolds raises error', {
  expect_error(stylest2_select_vocab(dfm = novels_dfm, nfold = 0),
               'nfold must be at least 1')
  expect_error(stylest2_select_vocab(dfm = novels_dfm, nfold = 1.5),
               'nfold must be an integer')
  }
)

test_that('invalid cutoff pct raises error', {
  expect_error(stylest2_select_vocab(dfm = novels_dfm, cutoffs = c(-50, 100)),
               'all values in cutoffs must be between 0 and 100')
  }
)

