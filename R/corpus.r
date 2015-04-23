#' Cast data.frame to sparse matrix
#' 
#' Create a sparse matrix from matching vectors of row indices, column indices and values
#' 
#' @param rows a vector of row indices: [i,]
#' @param columns a vector of column indices: [,j]
#' @param values a vector of the values for each (non-zero) cell: [i,j] = value
#' @return a sparse matrix of the dgTMatrix class (\code{\link{Matrix}} package) 
#' @export
cast.sparse.matrix <- function(rows, columns, values=rep(1, length(rows))) {
  row_index = unique(rows)
  col_index = unique(columns)
  sm = spMatrix(nrow=length(row_index), ncol=length(col_index),
                match(rows, row_index), match(columns, col_index), values)
  sm = as(sm, 'dgCMatrix')
  rownames(sm) = row_index
  colnames(sm) = col_index
  sm
}

#' Create a document term matrix from a list of tokens
#' 
#' Create a \code{\link{DocumentTermMatrix}} from a list of document ids, terms, and frequencies. 
#' 
#' @param documents a vector of document names/ids
#' @param terms a vector of words of the same length as documents
#' @param freqs a vector of the frequency a a term in a document
#' @param minfreq the minimum frequency of terms for inclusion. Defaults to 5, set to 0 to skip filtering
#' @param minlength the minimum word length (number of characters) for inclusion, set to 0 to skip filtering
#' @param filter.chars filter out any words containing numbers or non-word characters (defaults to True)
#' @param filter an optional boolean vector of the length of documents whether each document should be included. Any additional filtering will be applied on to op this filter
#' @return a document-term matrix  \code{\link{DocumentTermMatrix}}
#' @export
dtm.create <- function (documents, terms, freqs = rep(1, length(documents)),
                        minfreq=5, minlength=3, filter.chars=TRUE, filter=rep(T, length(documents))) {
  if (minfreq>0) {
    message('Ignoring words with frequency lower than ', minfreq)
    t = table(terms)
    filter = filter & terms %in% names(t)[t>=minfreq]
  }
  #TODO: this is not very efficient if terms is a large factor
  if (minlength>0) {
    message('Ignoring words with less than ', minlength, ' characters')
    filter = filter & nchar(as.character(terms)) >= minlength
  }
  if (filter.chars) {
    message('Ignoring words that contain numbers of non-word characters')
    filter = filter & !grepl("\\W|\\d", terms)
  }
  d = data.frame(ids = documents[filter], terms = terms[filter], freqs = freqs[filter])
  if (sum(is.na(d$terms)) > 0) {
    warning("Removing ", sum(is.na(d$terms)), "rows with missing term names")
    d = d[!is.na(d$terms), ]
  }
  sparsemat = cast.sparse.matrix(rows = d$ids, columns = d$terms, 
                                 values = d$freqs)
  as.DocumentTermMatrix(sparsemat, weighting = weightTf)
}

#' Filter a dtm by selecting documents or terms
#' 
#' Filters the dtm and removes any empty rows or columns
#' 
#' @param dtm the dtm to filter
#' @param documents an optional vector of documents to include
#' @param terms an optional  vector of terms to include
#' @return the filtered dtm
#' @export
dtm.filter <- function(dtm, documents=NULL, terms=NULL) {
  if (!is.null(terms)) dtm = dtm[, colnames(dtm) %in% terms]
  if (!is.null(documents)) dtm = dtm[rownames(dtm) %in% documents,]
  dtm[row_sums(dtm) > 0, col_sums(dtm) > 0]
}

#' Compute some useful corpus statistics for a dtm
#' 
#' Compute a number of useful statistics for filtering words: term frequency, idf, etc.
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{dtm.create}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
term.statistics <- function(dtm) {
  dtm = dtm[row_sums(dtm) > 0,col_sums(dtm) > 0]    # get rid of empty rows/columns
  vocabulary = colnames(dtm)
  data.frame(term = as.character(vocabulary),
             characters = nchar(vocabulary),
             number = grepl("[0-9]", vocabulary),
             nonalpha = grepl("\\W", vocabulary),
             termfreq = col_sums(dtm),
             docfreq = col_sums(dtm > 0),
             reldocfreq = col_sums(dtm > 0) / nDocs(dtm),
             tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0)),
             stringsAsFactors=F)
}



#' Plot a word cloud from a dtm
#' 
#' Compute the term frequencies for the dtm and plot a word cloud with the top n topics
#' You can either supply a document-term matrix or provide terms and freqs directly
#' (in which case this is an alias for wordcloud::wordcloud with sensible defaults)
#' 
#' @param dtm the document-term matrix
#' @param nterms the amount of words to plot (default 100)
#' @param freq.fun if given, will be applied to the frequenies (e.g. sqrt)
#' @param terms the terms to plot, ignored if dtm is given
#' @param freqs the frequencies to plot, ignored if dtm is given
#' @param scale the scale to plot (see wordcloud::wordcloud)
#' @param min.freq the minimum frquency to include (see wordcloud::wordcloud)
#' @param rot.per the percentage of vertical words (see wordcloud::wordcloud)
#' @param pal the colour model, see RColorBrewer
#' @export
dtm.wordcloud <- function(dtm=NULL, nterms=100, freq.fun=NULL, terms=NULL, freqs=NULL, scale=c(6, .5), min.freq=1, rot.per=.15, pal=brewer.pal(6,"YlGnBu")) {
  if (!is.null(dtm)) {
    t = term.statistics(dtm)
    t = t[order(t$termfreq, decreasing=T), ]
    terms = t$term
    freqs = t$termfreq
  }
  if (length(terms) < nterms) nterms = length(terms)
  if (!is.null(freq.fun)) freqs = freq.fun(freqs)
  if (!is.null(nterms)) {
    select = order(-freqs)[1:nterms]
    terms = terms[select] 
    freqs = freqs[select]
  }
  if (is.null(terms) | is.null(freqs)) stop("Please provide dtm or terms and freqs")
  wordcloud(terms, freqs, 
          scale=scale, min.freq=min.freq, max.words=Inf, random.order=FALSE, 
          rot.per=rot.per, colors=pal)
}


#### COMPARING CORPORA

#' Compute the chi^2 statistic for a 2x2 crosstab containing the values
#' [[a, b], [c, d]]
chi2 <- function(a,b,c,d) {
  ooe <- function(o, e) {(o-e)*(o-e) / e}
  tot = 0.0 + a+b+c+d
  a = as.numeric(a)
  b = as.numeric(b)
  c = as.numeric(c)
  d = as.numeric(d)
  (ooe(a, (a+c)*(a+b)/tot)
   +  ooe(b, (b+d)*(a+b)/tot)
   +  ooe(c, (a+c)*(c+d)/tot)
   +  ooe(d, (d+b)*(c+d)/tot))
}

#' Compare two corpora
#' 
#' Compare the term use in corpus dtm with a refernece corpus dtm.ref, returning relative frequencies
#' and overrepresentation using various measures
#' 
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix
#' @param smooth the smoothing parameter for computing overrepresentation
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
corpora.compare <- function(dtm.x, dtm.y, smooth=.001, min.over=NULL, min.chi=NULL) {
  freqs = data.frame(term=colnames(dtm.x), termfreq=col_sums(dtm.x))
  freqs.rel = data.frame(term=colnames(dtm.y), termfreq=col_sums(dtm.y))
  f = merge(freqs, freqs.rel, all=T, by="term")    
  f = f[f$termfreq.x + f$termfreq.y > 0,]
  f[is.na(f)] = 0
  f$relfreq.x = f$termfreq.x / sum(freqs$termfreq)
  f$relfreq.y = f$termfreq.y / sum(freqs.rel$termfreq)
  f$over = (f$relfreq.x + smooth) / (f$relfreq.y + smooth)
  f$chi = chi2(f$termfreq.x, f$termfreq.y, sum(f$termfreq.x) - f$termfreq.x, sum(f$termfreq.y) - f$termfreq.y)
  if(!is.null(min.over)) f = f[f$over > min.over,]
  if(!is.null(min.chi)) f = f[f$chi > min.chi,]
  f
}

#' Split a dtm into a list.
#' 
#' Transform a dtm into a list containing the dtm in parts.
#' 
#' @param dtm a document-term matrix
#' @param subcorpus a vector that matches the documents (rows) of the dtm. The dtm will be split into separate dtms for each value of subcorpus.
#' @return A list of dtms, named after the split_by values
#' @export
splitDtm <- function(dtm, subcorpus) {
  dtm_list = llply(unique(subcorpus), function(x) dtm[subcorpus == x,])
  names(dtm_list) = unique(subcorpus)
  dtm_list
}

eachToAllComparison <- function(dtm, corpus_ids, ...){
  message('Comparing corpora (N=', length(corpus_ids),')')  
  compare_results = llply(names(corpus_ids), function(corpus_name) corpora.compare(dtm[corpus_ids[[corpus_name]],], 
                                                                 dtm[unlist(corpus_ids[!names(corpus_ids)==corpus_name]),], ...), .progress='text') 
  names(compare_results) = names(corpus_ids)
  compare_results
}

#' @export
unlistWindow <- function(list_object, i, window){
  indices = i + window
  indices = indices[indices > 0 & indices < length(list_object)]
  unlist(list_object[indices], use.names=F)
}

windowComparison <- function(dtm, corpus_ids, window, ...){
  message('Comparing corpora (N=', length(corpus_ids),')')
  corpus_ids = corpus_ids[order(names(corpus_ids))]
  compare_results = llply(1:length(corpus_ids), function(i) corpora.compare(dtm[corpus_ids[[i]],], 
                                                          dtm[unlistWindow(corpus_ids,i,window),], ...), .progress='text') 
  names(compare_results) = names(corpus_ids)
  compare_results
}

#' The corpora.compare function for a list of dtm's
#' 
#' @param x either a named list of document term matrices, or a single dtm (in which cast the subcorpus argument must be given)
#' @param subcorpus if x is a single dtm, subcorpus should be a vector of the length and order of the dtm rows. Each value of subcorpus then represents a separate corpus.
#' @param method different ways to compare corpora. "each_to_all" compares each corpus to a corpos consisting of all other corpora. "window" compares each corpus to a corpus consisting of the previous and/or next corpora (see window.size). The order will be determined by sorting the dtm names (or the subcorpus values) with the order function.  
#' @param return.df logical. If True, the results are returned as a data.frame. Otherwise as a list.
#' @param window.size A vector of integers. If method is 'window', this determines which previous and next corpora are used to compare a corpus to. For example: if 3, then each corpus will be compared to a corpus consisting of the 3 previous and 3 next corpora. A corpus itself will not be used to create the comparison corpus. Note that the first and last [window.size] corpora cannot be compared to a full window.  
#' @param ... additional arguments to be passed to the corpora.compare function
#' @return a list or data.frame with the corpora.compare results for each dtm.  
#' @export
corpora.compare.list <- function(x, subcorpus=NULL, method='each_to_all', return.df=F, window.size=3, ...) {
  if('list' %in% class(x)) {
    subcorpus = rep(names(x), laply(x, nrow))
    x = Reduce(c, x)
  }   
  corpus_ids = llply(unique(subcorpus), function(subcorpus_value) which(subcorpus == subcorpus_value))
  names(corpus_ids) = unique(subcorpus)
  
  if(method == 'each_to_all') results = eachToAllComparison(x, corpus_ids, ...)
  if(method == 'window') {
    window = -window.size:window.size
    window = window[!window == 0]
    results = windowComparison(x, corpus_ids, window, ...)
  }
  if(return.df) results = ldply(results, .id='corpus')
  results
}

#' @export
corpcomp.wordcloud <- function(compare_results, nterms=25, ...){
  compare_results = compare_results[compare_results$over > 1,]
  compare_results = compare_results[order(-compare_results$chi),]
  compare_results = head(compare_results, nterms)
  dtm.wordcloud(terms=compare_results$term, freqs=compare_results$over, ...)
}

## COMPARING DOCUMENTS

getCosine <- function(m1, m2=m1){
  norm.x = sqrt(Matrix::colSums(m1^2))
  norm.y = sqrt(Matrix::colSums(m2^2))
  mat = Matrix::crossprod(m1,m2)
  mat = mat / Matrix::tcrossprod(norm.x, norm.y)
  mat[is.na(mat)] = 0
  mat
}



#' Transform a dtm into a sparse matrix.
#' 
#' @param dtm a document-term matrix
#' @return a sparse matrix
#' @export
dtmToSparseMatrix <- function(dtm){
  sm = Matrix::spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
  rownames(sm) = rownames(dtm)
  colnames(sm) = colnames(dtm)
  sm
}

Nth.max <- function(x, N){
  N = min(N, length(x)) 
  -sort(-x, partial=N)[N]
}

#' Compare the documents in two corpora/dtms
#' 
#' Compare the documents in corpus dtm.x with reference corpus dtm.y. 
#' 
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix. If NULL, documents of dtm.x are compared to each ohter
#' @param measure the measure that should be used to calculate similarity/distance/adjacency. Currently only cosine is supported
#' @param min.similarity a threshold for similarity. lower values are deleted. Set to 0.1 by default.
#' @param n.topsim An alternative or additional sort of threshold for similarity. Only keep the [n.topsim] highest similarity scores for x. Can return more than [n.topsim] similarity scores in the case of duplicate similarities.
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
documents.compare <- function(dtm.x, dtm.y=NULL, measure='cosine', min.similarity=0.1, n.topsim=NULL) {
  dtm.x = Matrix::t(dtmToSparseMatrix(dtm.x))
  dtm.y = if(is.null(dtm.y)) dtm.x else Matrix::t(dtmToSparseMatrix(dtm.y))
  if(measure == 'cosine') results = getCosine(dtm.x, dtm.y)
  
  if(!is.null(min.similarity)) results@x[Matrix::which(results@x < min.similarity)] = 0
  if(!is.null(n.topsim)) {
    simthres = apply(results, 1, Nth.max, N=n.topsim)
    results@x[Matrix::which(results < simthres)] = 0
  }

  results = as(results, 'dgTMatrix')
  results = data.frame(x=colnames(dtm.x)[results@i+1], y=colnames(dtm.y)[results@j+1], similarity=results@x)
  results = results[results$similarity > 0 & !is.na(results$similarity),]
  results[!as.character(results$x) == as.character(results$y),]
}

#' Compare the documents in a dtm per time frame
#' 
#' Compare all documents within a document term matrix that are dated (e.g., pubished) within a given number of days (window.size) from each other.
#' 
#' @param dtm a document-term matrix in the tm format
#' @param document.date a vector of date class, of the same length and order as the documents (rows) of the dtm.
#' @param window.size the timeframe in days within which articles must occur in order to be compared. e.g., if 0, articles are only compared to articles of the same day. If 1, articles are compared to all articles of the previous, same or next day.
#' @param measure the measure that should be used to calculate similarity/distance/adjacency. Currently only cosine is supported
#' @param min.similarity a threshold for similarity. lower values are deleted
#' @param n.topsim An alternative or additional sort of threshold for similarity. Only keep the [n.topsim] highest similarities for x.
#' @return A data frame with columns x, y and similarity. 
#' @export
documents.window.compare <- function(dtm, document.date, window.size=3, measure='cosine', min.similarity=NULL, n.topsim=NULL, use.tfidf=F){
  if(use.tfidf) dtm = weightTfIdf(dtm) 
  days = as.Date(document.date)
  dayseq = seq.Date(min(days), max(days), by=1)
  day_ids = llply(dayseq, function(day) which(days == day))
  
  window = -window.size:window.size
  nonempty_days = which(laply(day_ids, length) > 0)
  ldply(nonempty_days, function(i) documents.compare(dtm[day_ids[[i]],], 
                                                     dtm[unlistWindow(day_ids,i,window),], measure, min.similarity, n.topsim), .progress='text') 
}



