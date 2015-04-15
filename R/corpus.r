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
                        minfreq=5, minlength=3, filter.chars=TRUE, filter=rep(T, length(documents))) 
{
  if (minfreq>0) {
    t = table(terms)
    filter = filter & terms %in% names(t)[t>=minfreq]
  }
  #TODO: this is not very efficient if terms is a large factor
  if (minlength>0) filter = filter & nchar(as.character(terms)) >= minlength
  if (filter.chars) filter = filter & !grepl("\\W|\\d", terms)
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
corpora.compare <- function(dtm.x, dtm.y, smooth=.001) {
  freqs = term.statistics(dtm.x)[, c("term", "termfreq")]
  freqs.rel = term.statistics(dtm.y)[, c("term", "termfreq")]
  f = merge(freqs, freqs.rel, all=T, by="term")    
  f[is.na(f)] = 0
  f$relfreq.x = f$termfreq.x / sum(freqs$termfreq)
  f$relfreq.y = f$termfreq.y / sum(freqs.rel$termfreq)
  f$over = (f$relfreq.x + smooth) / (f$relfreq.y + smooth)
  f$chi = chi2(f$termfreq.x, f$termfreq.y, sum(f$termfreq.x) - f$termfreq.x, sum(f$termfreq.y) - f$termfreq.y)
  f
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
  if(!is.null(dtm) & ncol(dtm) < nterms) nterms = ncol(dtm)
  if (!is.null(dtm)) {
    t = term.statistics(dtm)
    t = t[order(t$termfreq, decreasing=T), ]
    terms = t$term
    freqs = t$termfreq
  }
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