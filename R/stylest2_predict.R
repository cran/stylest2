#' Predict authorship of texts.
#'
#' This function generates predicted probabilities of authorship for a set of texts.
#' It takes as an input a document-feature matrix of texts for which authorship is
#' to be predicted, as well as a stylest2 model containing potential authors.
#' 
#' @export
#'
#' @param dfm a quanteda \code{dfm} object. Each row should represent a text whose
#' authorship is to be predicted.
#' @param model A stylest2 model.
#' @param speaker_odds Should the model return log odds of authorship for each text, 
#' in addition to posterior probabilities?
#' @param term_influence Should the model return the influence of each term in determining
#' authorship over the prediction set, in addition to returning posterior probabilities?
#' @param prior Prior probability, defaults to \code{NULL}.
#' @return A list object:
#' 
#' @examples
#' data(novels_dfm)
#' mod <- stylest2_fit(novels_dfm)
#' stylest2_predict(dfm=novels_dfm, model=mod)
#' 
stylest2_predict <- function(dfm, model, speaker_odds=FALSE, term_influence=FALSE,
                             prior=NULL) {
  #require(Matrix)
  
  
  ## 1) check the class of the inputs to make sure they are appropriate
  
  dfm_class <- class(dfm)
  if ( dfm_class != 'dfm' | attr(dfm_class, 'package') != 'quanteda' ) {
    
    stop('`dfm` must be a quanteda "dfm" object.')
    
  }
  
  if( attr(model, 'package') != 'stylest2' ) {
    
    stop('`model` must be a "stylest2" model, fit using "stylest2_fit()".')
    
  }
  
  
  ## 2) pre-process the inputs
  
  ## need the features to correspond to the features from model dfm. Can't include
  ## features that aren't in the original
  pred_docs_dfm <- quanteda::dfm_match(dfm, features=colnames(model$rate))
  
  ## the number of tokens for each doc in the prediction set. We will use this to
  ## rescale the log-likelihoods across the prediction docs. Without rescaling,
  ## would get uniformly larger values for longer texts since they are all being
  ## multiplied by positive "rates" (I think)
  pred_docs_ntoken <- Matrix::rowSums(pred_docs_dfm)
  
  ## number of speakers
  speakers <- rownames(model$rate)
  nspeaker <- length(unique(speakers))
  
  
  
  ## 3) generate the log-likelihood of authorship
  
  ## we start with the uniform prior. Will generalize so users can specify
  ## their own priors later
  #prior <- rep(x=(1/nspeaker), times=nspeaker)
  if (is.null(prior)) {
    log_prior <- rep(-log(nspeaker), nspeaker) # log(1/nspeaker)
  } else {
    log_prior <- log(prior)
  }
  
  
  ## we take the log of the speaker-specific rate for each term in the model,
  ## because it simplifies computation of the likelihood function
  eta_tv <- log(model$rate)
  eta_store <- eta_tv # store old etas for term_influence
  
  # multiply by term weights if they exist
  if (!is.null(model$term_weights)) {
    # make sure weights are in the same order for matrix multiplication
    sorted_weights <- model$term_weights[colnames(eta_tv)]
    for (i in 1:nrow(eta_tv)) {
      eta_tv[i, ] <- eta_tv[i, ] * sorted_weights
    }
  }
  
  ## compute the dot product of the occurrences of the term in the (d) prediction
  ## document and the (n) rates from the model
  
  ## but the magnitude of these likelihoods (across documents) will be driven by the
  ## NUMBER OF TOKENS in the specific document - so we want to rescale by document,
  ## so the 
  ll_unscaled <- pred_docs_dfm %*% t(eta_tv) # dXn %*% nXm ~ dXm (prediction for each doc & speaker (m) )
  scalar <- pred_docs_ntoken %*% t(Matrix::rowSums(model$rate)) # dX1 %*% t(mX1) ~ dXm (fixed across m)
  
  ## scale each set of log likelihood values by the number of tokens in the prediction document
  loglik <- ll_unscaled - scalar
  rownames(loglik) <- rownames(pred_docs_dfm) # names of prediction documents
  
  ## compute the posterior probability for each speaker -- if prior specified,
  ## then add the log prior for each speaker, rowwise by document
  if (is.null(prior)) {
    
    ## null prior -- we arent scaling by the uniform prior? This won't affect
    ## the ordering of the posterior either way
    log_weights <- loglik
    
  } else {
    
    ## rowwise operation - add the log prior to row 1, then row 2, ...
    log_weights <- sweep(loglik, 2, log_prior, "+")
    
  }
  
  out <- list(model = model,
              posterior = posterior(log_weights=log_weights, 
                                    pred_docs_dfm=pred_docs_dfm, 
                                    speakers=speakers, 
                                    log_prior=log_prior))
  
  
  if (speaker_odds) {
    out$speaker_odds <- speaker_odds(log_weights=log_weights, 
                                     pred_docs_dfm = pred_docs_dfm, 
                                     pred_docs_ntoken=pred_docs_ntoken, 
                                     speakers=speakers)
  }
  
  if (term_influence) {
    out$term_influence <- term_influence(pred_docs_dfm=pred_docs_dfm, 
                                         eta_tv=eta_store, 
                                         model=model)
  }
  
  return(out)
  
}

#' Internal stylest2 function to predict posterior likelihoods of authorship.
#'
#' Generate the posterior likelihood of speaker given their rates and predicted
#' dfm frequencies.
#'
#' @param log_weights The logged author-level term weights from a stylest2 model.
#' @param pred_docs_dfm A document-feature matrix for the texts to be predicted.
#' @param speakers The speaker labels for \code{pred_docs_dfm} rows.
#' @param log_prior A vector of log prior probabilities.
#' @return The posterior likelihoods of authorship for prediction texts.
#' 
#' @keywords internal
#' 
posterior <- function(log_weights, pred_docs_dfm, speakers, log_prior) {
  #require(Matrix)
  
  ## subtract the HIGHEST valued log posterior associated with each document from
  ## ALL log posteriors in that document.
  ## this protects against overflow with the exponential transformation of these
  ## log posteriors, maximum value of exp(logpost) will be 1.
  log_weights <- log_weights - apply(log_weights, 1, max)
  
  ## why is THIS the constant?? Is this just coercing them to a probability
  ## distribution? How can we do this if we don't compute P(A) in P(B|A) = P(A|B)P(B) / P(A)
  const <- log( Matrix::rowSums(exp(log_weights)) )
  ## is this actually eta_cv?? The average etas across all other cases?
  
  ## subtract out the constant to obtain posterior probabilities
  log_probs <- log_weights - const
  rownames(log_probs) <- rownames(pred_docs_dfm)
  
  ## extract the most likely speaker from each document
  predicted <- speakers[apply(log_probs, 1, which.max)]
  #predicted <- factor(predicted, levels = speakers)
  ## do we need to convert to factor?
  
  return(
    list(predicted=predicted,
         log_probs=log_probs,
         log_prior=log_prior)
  )

}

#' Internal stylest2 function to predict log odds of speakership across texts.
#'
#' @param log_weights The logged author-level term weights from a stylest2 model.
#' @param pred_docs_dfm A document-feature matrix for the texts to be predicted.
#' @param pred_docs_ntoken A vector, the number of tokens in each prediction
#' document.
#' @param speakers The speaker labels for \code{pred_docs_dfm} rows.
#' 
#' @keywords internal
#' 
speaker_odds <- function(log_weights, pred_docs_dfm, pred_docs_ntoken, speakers) {
  
  ## instead of subtracting highest valued log posterior from all document-wise,
  ## we instead divide each posterior by the number of tokens in that document
  ## to normalize. Because otherwise ..........
  log_weights <- log_weights / pred_docs_ntoken
  
  ## match the position of the actual speaker in the model to each row of the
  ## actual speaker in the new dfm
  baseline <- match(quanteda::docvars(pred_docs_dfm)["author"][,1], speakers)
  
  ## locate the position of the actual speaker for each document in the test dfm,
  ## then subtract the log weight for each of these from the full set of log weights.
  ## In essence this just goes through the matrix and imposes that the actual speakers
  ## log odds is zero.
  log_odds <- log_weights[cbind(seq_along(baseline), baseline)] - log_weights
  
  nspeaker <- length(speakers)
  ## compute the average log odds for each speaker having given this speech
  log_odds_mean <- Matrix::rowSums(log_odds) / nspeaker
  
  ## compute the sample standard deviation for the log odds that each speaker
  ## gave this speech
  log_odds_se  <- apply(log_odds, 1, stats::sd) / sqrt(nspeaker)
  
  return(
    list(log_odds_mean=log_odds_mean,
         log_odds_se=log_odds_se)
  )
  
}

#' Internal stylest2 function to return the influence of each term in predicting authorship.
#'
#' @param pred_docs_dfm A document-feature matrix for the texts to be predicted.
#' @param eta_tv The author-level term weights from a stylest2 model.
#' @param model A stylest2 model.
#' 
#' @keywords internal
#' 
term_influence <- function(pred_docs_dfm, eta_tv, model) {
  
  pred_authors <- quanteda::docvars(pred_docs_dfm)["author"][,1]
  
  ## aggregate dfm by author
  x <- quanteda::dfm_group(pred_docs_dfm, pred_authors)
  ## number of documents by author
  ndoc <- table(pred_authors)
  ## ensure order is same as row order in aggregate dfm
  ndoc <- ndoc[match(rownames(x), names(ndoc))]
  
  ## scale each author-level feature count by number of documents, yielding the
  ## average frequency per author-document
  fbar <- x * (1/as.numeric(ndoc))
  rownames(fbar) <- names(ndoc)
  colnames(fbar) <- colnames(x)
  
  etabar <- Matrix::colMeans(eta_tv)
  eta_centered <- eta_tv - matrix(1, nrow(eta_tv), 1) %*% etabar
  
  ## product of average frequency and distinctiveness (centered)
  d <- fbar * eta_centered
  
  # multiply by term weights if they exist
  if (!is.null(model$term_weights)) {
    # make sure weights are in the same order for matrix multiplication
    sorted_weights <- model$term_weights[colnames(d)]
    for (i in 1:nrow(d)) {
      d[i, ] <- d[i, ] * sorted_weights
    }
  }
  
  terms <- colnames(pred_docs_dfm)
  infl_mean <- apply(abs(d), 2, mean)
  infl_max <- apply(abs(d), 2, max)
  
  return(data.frame(features=terms, 
                    mean_influence=infl_mean, 
                    max_influence=infl_max, 
                    row.names = NULL)
         )
  
}