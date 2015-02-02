
#' Cast data.frame to sparse matrix
#' 
#' Create a sparse matrix from matching vectors of row indices, column indices and values
#' 
#' @param rows a vector of row indices: [i,]
#' @param columns a vector of column indices: [,j]
#' @param values a vector of the values for each (non-zero) cell: [i,j] = value
#' @return a sparse matrix of the dgTMatrix class (\code{\link{Matrix}} package) 
#' @export
cast.sparse.matrix <- function(rows, columns, values=NULL) {
  if(is.null(values)) values = rep(1, length(rows))
  d = data.frame(rows=rows, columns=columns, values=values)
  if(nrow(d) > nrow(unique(d[,c('rows','columns')]))){
    message('(Duplicate row-column matches occured. Values of duplicates are added up)')
    d = aggregate(values ~ rows + columns, d, FUN='sum')
  }
  unit_index = unique(d$rows)
  char_index = unique(d$columns)
  sm = spMatrix(nrow=length(unit_index), ncol=length(char_index),
                match(d$rows, unit_index), match(d$columns, char_index), d$values)
  rownames(sm) = unit_index
  colnames(sm) = char_index
  sm
}

#' Create a document term matrix from a list of tokens
#' 
#' Create a \code{\link{DocumentTermMatrix}} from a list of document ids, terms, and frequencies. 
#' 
#' @param documents a vector of document names/ids
#' @param terms a vector of words of the same length as documents
#' @param freqs a vector of the frequency a a term in a document
#' @return a document-term matrix  \code{\link{DocumentTermMatrix}}
#' @export
dtm.create <- function(documents, terms, freqs=rep(1, length(documents))) {
  # remove NA terms
  d = data.frame(ids=documents, terms=terms, freqs=freqs)
  if (sum(is.na(d$terms)) > 0) {
    warning("Removing ", sum(is.na(d$terms)), "rows with missing term names")
    d = d[!is.na(d$terms), ]
  }
  sparsemat = cast.sparse.matrix(rows=d$ids, columns=d$terms, values=d$freqs)
  as.DocumentTermMatrix(sparsemat, weighting=weightTf)
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
  data.frame(term = vocabulary,
             characters = nchar(vocabulary),
             number = grepl("[0-9]", vocabulary),
             nonalpha = grepl("\\W", vocabulary),
             termfreq = col_sums(dtm),
             docfreq = col_sums(dtm > 0),
             reldocfreq = col_sums(dtm > 0) / nDocs(dtm),
             tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0)))
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
  if (!is.null(dtm)) {
    t = term.statistics(dtm)
    t = t[order(t$termfreq, decreasing=T), ]
    terms = t$term
    freqs = t$termfreq
  }
  if (!is.null(nterms)) {
    terms = terms[1:nterms] 
    freqs = freqs[1:nterms]
  }
  if (!is.null(freq.fun)) freqs = freq.fun(freqs)
   
  if (is.null(terms) | is.null(freqs)) stop("Please provide dtm or terms and freqs")
  wordcloud(terms, freqs, 
          scale=scale, min.freq=min.freq, max.words=Inf, random.order=FALSE, 
          rot.per=rot.per, colors=pal)
}