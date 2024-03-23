#' Fit speaker model to document-feature matrix
#'
#' This function generates a model of speaker/author attribution, given a document-feature
#' matrix. 
#' 
#' @export
#'
#' @param dfm a quanteda \code{dfm} object
#' @param smoothing the smoothing parameter value for smoothing the dfm. Should 
#' be a numeric scalar, default to 0.5.
#' @param terms If not \code{NULL}, terms to be used in the model. If \code{NULL}, 
#' use all terms.
#' @param term_weights Named vector of distances (or any weights) per term in the 
#' vocab. Names should correspond to the term.
#' @param fill_weight Numeric value to fill in as weight for any term which does 
#' not have a weight specified in \code{term_weights}.
#' @return An S3 object, a model with with each term that occurs in the text, the 
#' frequency of use for each author, and the frequency of that terms' occurrence 
#' through the texts.
#' 
#' @examples
#' data(novels_dfm)
#' stylest2_fit(dfm = novels_dfm)
#' 
stylest2_fit <- function(dfm, smoothing=0.5, terms=NULL, term_weights=NULL, 
                         fill_weight=NULL) {
  
  ## check that `docs` is of acceptable class and, if so, does it have the correct
  ## columns? If yes to both, coerce `text` and `author` columns to character
  if(!inherits(dfm, 'dfm')) {
    
    stop('`dfm` object must be of class "dfm" (see quanteda help files if needed).')
    
  }
  
  if(smoothing<0 | !is.numeric(smoothing)) {
    stop('Smoothing value must be numeric and non-negative.')
  }
  
  
  
  if( !is.null(term_weights) ) {
    term_weights <- term_weights[names(term_weights) %in% colnames(dfm)]
    
    no_wts <- colnames(dfm)[!colnames(dfm) %in% names(term_weights)]
    
    term_weights[no_wts] <- NA
    
    ## generate value to fill NA term weights if applicable
    if( !is.null(fill_weight) ) {
      fill_value <- fill_weight
    } else {
      fill_value <- mean(term_weights, na.rm=TRUE)
    }
    
    ## fill missing term weights
    term_weights[ is.na(term_weights) ] <- fill_value
    
  }
  
  ## if passes all checks, fit the model
  model <- fit_term_usage(dfm=dfm, 
                          smoothing=smoothing, 
                          terms=terms, 
                          term_weights=term_weights)
  
  ## return the model output
  return(model)
}

#' Internal stylest2 function to compute term usage
#'
#' @param dfm a quanteda \code{dfm} object
#' @param smoothing the smoothing parameter value for smoothing the dfm. Should 
#' be a numeric scalar, default to 0.5.
#' @param terms If not \code{NULL}, terms to be used in the model. If \code{NULL}, 
#' use all terms.
#' @param term_weights Named vector of distances (or any weights) per term in the 
#' vocab. Names should correspond to the term.
#' @return A model with with each term that occurs in the text, the frequency of 
#' use for each author, and the frequency of that terms' occurrence through the texts.
#' 
#' @keywords internal
#' 
fit_term_usage <- function(dfm, smoothing, terms, term_weights) {
  #require("Matrix")
  #require("quanteda")
  
  ## extract authors from input data
  if( !"author" %in% names(quanteda::docvars(dfm)) ) {
    stop("Need to supply a docvar in `docs` with text authorship.")
  } else {
    author_loc <- quanteda::docvars(dfm)["author"][,1]
  }
  
  if(length(author_loc) < 2) {
    stop("Should be at least two authors in the corpus.")
  }
  if(any(is.na(author_loc))) {
    stop("Detected missingness in the authorship variable. Documents with missing authorship should be omitted from the corpus.")
  }
  if(any(duplicated(author_loc))) {
    warning("Detected multiple texts with the same author. Collapsing to author-level dfm for stylest2_fit() function.")
    ## group the document feature matrix by author. This is akin to collapsing a matrix
    ## to the author level, summing over each document for all features in the dfm.
    ## NOTE: this replaces the corpus `term_matrix()` function to generate the dfm.
    dfm <- quanteda::dfm_group(dfm, groups = author_loc)
  }
  
  ## if there are any docvars that dont contain "author"
  if( any(colnames(quanteda::docvars(dfm)) != "author") ) {
    warning("Additional docvars detected other than authorship. These will be ignored.")
  }
  
  if(!is.null(terms)) {
    dfm <- dfm[ , terms]
  }
  
  ## total number of tokens for each author. Obtained by summing over the author level
  ## dfm
  tokens_per_author <- Matrix::rowSums(dfm)
  tokens_per_term <- Matrix::colSums(dfm)
  
  ## length of author level token count vector must have the same number of elements
  ## as the author-level dfm has rows. This seems a useful check prior to applying
  ## the weights across the author level dfm.
  if(length(tokens_per_author) != nrow(dfm)) {stop("Author token counts don't equal dfm rows.")}
  ## is this a redundant check???
  
  
  ## scale the author-level dfm by the inverse total token count for that author
  ntype <- ncol(dfm)
  scaled_dfm <- ( as.matrix(dfm) + smoothing ) / ( tokens_per_author + smoothing*ntype)
  
  ## list object with:
  ## 1) vector of terms included in the dfm
  ## 2) a matrix of term frequencies for each author
  out <- list(terms=colnames(scaled_dfm),
              rate = scaled_dfm,
              tokens_per_term = tokens_per_term,
              term_weights = term_weights)
  attr(out, "package") <- "stylest2"
  return(out)
}