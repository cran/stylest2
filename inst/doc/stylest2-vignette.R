## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----gh-installation, eval = FALSE--------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("ArthurSpirling/stylest2")

## ----echo=TRUE----------------------------------------------------------------
library(stylest2)
library(quanteda)

## ----echo=TRUE----------------------------------------------------------------
data(novels)

## ----echo=FALSE---------------------------------------------------------------
# show a snippet of the data
knitr::kable(novels[c(1,4,8), ])

## ----echo=TRUE----------------------------------------------------------------
novels_tok <- tokens(novels$text)
novels_dfm <- dfm(novels_tok)

unique(novels$author)
docvars(novels_dfm)["author"] <- novels$author


## ---- echo=TRUE---------------------------------------------------------------

novels_tok <- tokens(novels$text, 
                     remove_punct = T,
                     remove_symbols = T,
                     remove_numbers = T,
                     remove_separators = T,
                     split_hyphens = T)
novels_dfm <- dfm(novels_tok)

unique(novels$author)
docvars(novels_dfm)["author"] <- novels$author


## ----echo = TRUE--------------------------------------------------------------
set.seed(1234)

## ----echo=TRUE----------------------------------------------------------------
vocab_with_defaults <- stylest2_select_vocab(dfm = novels_dfm)

## ----echo=TRUE----------------------------------------------------------------
vocab_custom <- stylest2_select_vocab(dfm = novels_dfm, 
                                      smoothing = 1, 
                                      nfold = 10, 
                                      cutoffs = c(50, 75, 99))

## ----echo=TRUE----------------------------------------------------------------
# Percentile with best prediction rate
vocab_with_defaults$cutoff_pct_best

# Rate of INCORRECTLY predicted speakers of held-out texts
vocab_with_defaults$cv_missrate_results

# Data on the setup:

# Percentiles tested
vocab_with_defaults$cutoff_candidates

# Number of folds
vocab_with_defaults$nfold

## ----echo=TRUE----------------------------------------------------------------
terms_90 <- stylest2_terms(dfm = novels_dfm, cutoff = 90)

## ----echo=TRUE----------------------------------------------------------------
mod <- stylest2_fit(dfm = novels_dfm, terms = terms_90)

## ----echo = TRUE--------------------------------------------------------------
term_weights <- c(0.1,0.2,0.001)
names(term_weights) <- c("the", "and", "Floccinaucinihilipilification")

term_weights

## ----echo = TRUE--------------------------------------------------------------
mod <- stylest2_fit(dfm = novels_dfm,  terms = terms_90, term_weights = term_weights)

## ----echo = TRUE--------------------------------------------------------------
predictions <- stylest2_predict(dfm = novels_dfm, model = mod)

## ----echo = TRUE--------------------------------------------------------------
predictions <- stylest2_predict(dfm = novels_dfm, model = mod,
                                speaker_odds = TRUE, term_influence = TRUE)

## ----echo = TRUE--------------------------------------------------------------
# Pride and Prejudice
novels$text[14]

predictions$speaker_odds$log_odds_mean[14]

predictions$speaker_odds$log_odds_se[14]

## ----echo = TRUE--------------------------------------------------------------
na_text <- "No one who had ever seen Catherine Morland in her infancy would have supposed 
            her born to be an heroine. Her situation in life, the character of her father 
            and mother, her own person and disposition, were all equally against her. Her 
            father was a clergyman, without being neglected, or poor, and a very respectable 
            man, though his name was Richard—and he had never been handsome. He had a 
            considerable independence besides two good livings—and he was not in the least 
            addicted to locking up his daughters."

na_text_dfm <- dfm(tokens(na_text))

pred <- stylest2_predict(dfm = na_text_dfm, model = mod)

## ----echo = TRUE--------------------------------------------------------------
pred$posterior$predicted

pred$posterior$log_probs

## ----echo = FALSE-------------------------------------------------------------
head(predictions$term_influence$features[order(predictions$term_influence$mean_influence, decreasing = TRUE)])

## ----echo = FALSE-------------------------------------------------------------
tail(predictions$term_influence$features[order(predictions$term_influence$mean_influence, decreasing = TRUE)])

