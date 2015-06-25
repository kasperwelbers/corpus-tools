## FUNCTIONS FOR COMPARING DOCUMENTS

cosineSimilarity <- function(m1, m2=m1){
  norm.x = sqrt(Matrix::colSums(m1^2))
  norm.y = sqrt(Matrix::colSums(m2^2))
  mat = Matrix::crossprod(m1,m2)
  mat = mat / Matrix::tcrossprod(norm.x, norm.y)
  mat[is.na(mat)] = 0
  mat
}

termOverlap <- function(m1, m2=m1){
  m2@x[Matrix::which(m2@x > 0)] = 1
  Matrix::crossprod(m1,m2)
}

termProduct <- function(m1, m2=m1){
  #m2@x[Matrix::which(m2@x > 0)] = 1
  Matrix::crossprod(m1,m2)
}

termOverlap_pct <- function(m1, m2=m1){
  totalterms = Matrix::colSums(as(m1, 'dgCMatrix'))
  m2@x[Matrix::which(m2@x > 0)] = 1
  Matrix::crossprod(m1,m2) / totalterms
}

termIndex <- function(m1, m2=m1){
  m2@x[Matrix::which(m2@x > 0)] = 1
  totalterms = Matrix::colSums(as(m1, 'dgCMatrix'))
  Matrix::crossprod(m1,m2) / totalterms
}

#' Get the overlapping terms between two documents in a dtm
#' 
#' @return a list or data.frame with the corpora.compare results for each dtm.  
#' @export
getOverlapTerms <- function(x, y, dtm.x, dtm.y=NULL, similarity=NULL, termfreq=T){
  filterterms = colnames(dtm.x)[col_sums(dtm.x) > 0]
  dtm.x = dtm.x[,filterterms]
  dtm.y = dtm.y[,filterterms]
  m.x = Matrix::t(dtmToSparseMatrix(dtm.x))
  m.y = if(is.null(dtm.y)) m.x else Matrix::t(dtmToSparseMatrix(dtm.y)) 
  
  matches = data.frame(x=x, y=y)
  if(!is.null(similarity)) matches$similarity = similarity
  
  termids_list = llply(unique(x), getTermIds, matches, m.x=m.x, m.y=m.y, .progress='text')
  times_match = unlist(llply(termids_list, laply, length)) # get length of lists in lists, which can be used to replicate the rows in 'matches'
  matches_i = rep(1:nrow(matches), times_match)
  matches = matches[matches_i,]
  matches$term = colnames(dtm.x)[unlist(termids_list)]
  if(termfreq){
    matches$freq.x = m.x[cbind(match(matches$term, rownames(m.x)), match(matches$x, colnames(m.x)))]
    matches$freq.y = m.y[cbind(match(matches$term, rownames(m.y)), match(matches$y, colnames(m.y)))]
  }
  matches
}

getTermIds <- function(x.id, matches, m.x, m.y){
  xmatches = matches[matches$x == x.id,]
  m.x = m.x[,as.character(x.id), drop=F]
  m.y = m.y[,as.character(xmatches$y), drop=F]
  termids_list = slam_crossapply(m.x, m.y, function(x,y) which(x*y>0))
  termids_list[1,,drop=F]
}

slam_crossapply <- function(m1, m2=m1, FUN=function(x,y) sum(x*y), ...){
  m1 = as(m1, 'dgTMatrix')
  m2 = as(m2, 'dgTMatrix')
  m1 = simple_triplet_matrix(m1@i+1, m1@j+1, m1@x, nrow(m1), ncol(m1), dimnames(m1))
  m2 = simple_triplet_matrix(m2@i+1, m2@j+1, m2@x, nrow(m2), ncol(m2), dimnames(m2))
  if(ncol(m1)==1 & ncol(m2)==1) {
    matrix(list(FUN(as.vector(m1),as.vector(m2), ...)), nrow=1, dimnames=list(colnames(m1),colnames(m2)))
  } else slam::crossapply_simple_triplet_matrix(m1, m2, FUN, ...)
}

#' Transform a dtm into a sparse matrix.
#' 
#' @param dtm a document-term matrix
#' @return a sparse matrix
#' @export
dtmToSparseMatrix <- function(dtm){
  if('DocumentTermMatrix' %in% class(dtm)){
    sm = Matrix::spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
    rownames(sm) = rownames(dtm)
    colnames(sm) = colnames(dtm)
  }
  sm
}

Nth.max <- function(x, N){
  N = min(N, length(x)) 
  -sort(-x, partial=N)[N]
}

filterResults <- function(results, min.similarity, n.topsim){
  if(!is.null(min.similarity)) results@x[Matrix::which(results@x < min.similarity)] = 0
  if(!is.null(n.topsim)) {
    simthres = apply(results, 1, Nth.max, N=n.topsim)
    results@x[Matrix::which(results < simthres)] = 0
  }
  results
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
#' @param only.from A vector of ids that match the documents (rownames) in dtm. Use to compare only these documents to other documents.
#' @param return.zeros If true, all comparison results are returned, including those with zero similarity (quite possibly the worst thing to do with large data)
#' @return A data frame with sets of documents and their similarities. 
#' @export
documents.compare <- function(dtm.x, dtm.y=NULL, measure='cosine', min.similarity=0.1, n.topsim=NULL, only.from=NULL, return.zeros=F) {
  if(!is.null(only.from)) dtm.x = dtm.x[rownames(dtm.x) %in% only.from,]
  m.x = Matrix::t(dtmToSparseMatrix(dtm.x))
  m.y = if(is.null(dtm.y)) m.x else Matrix::t(dtmToSparseMatrix(dtm.y))  
  if(measure == 'cosine') results = cosineSimilarity(m.x, m.y)
  if(measure == 'overlap') results = termOverlap(m.x, m.y)
  if(measure == 'overlap_pct') results = termOverlap_pct(m.x, m.y)
  if(measure == 'product') results = termProduct(m.x, m.y)
  
  results = filterResults(results, min.similarity, n.topsim)
  
  results = as(results, 'dgTMatrix')
  if(return.zeros) {
    results = Matrix(which(!is.na(results), arr.ind=T))
    results = data.frame(x=colnames(m.x)[results[,1]], y=colnames(m.y)[results[,2]], similarity=as.vector(results))
  } else{
    results = data.frame(x=colnames(m.x)[results@i+1], y=colnames(m.y)[results@j+1], similarity=results@x)
    results = results[results$similarity > 0 & !is.na(results$similarity),]
  }
  results[!as.character(results$x) == as.character(results$y),]
}

aggregateTimeUnits <- function(datetime, unit='hours'){
  datetime = as.POSIXct(datetime)
  if(!unit %in% c('mins','hours','days','months','years')) stop('Could not recognize "', unit, '" as valid time.unit')
  
  if(unit == 'mins') return(as.POSIXct(format(datetime, '%Y-%m-%d %H:%M:00')))
  if(unit == 'hours') return(as.POSIXct(format(datetime, '%Y-%m-%d %H:00:00')))
  if(unit == 'days') return(as.POSIXct(format(datetime, '%Y-%m-%d 00:00:00')))
  if(unit == 'months') return(as.POSIXct(format(datetime, '%Y-%m-01 00:00:00')))
  if(unit == 'years') return(as.POSIXct(format(datetime, '%Y-01-01 00:00:00')))
}

getWindow <- function(window.size, window.direction){
  if(window.direction == '<=>') window = -window.size:window.size
  if(window.direction == '<>') window = c(-window.size:-1, 1:window.size)
  if(window.direction == '<=') window = -window.size:0
  if(window.direction == '<') window = -window.size:-1
  if(window.direction == '>=') window = 0:window.size
  if(window.direction == '>') window = 1:window.size
  if(window.direction == '=') window = 0
  window
}

#' Compare the documents in a dtm per time frame
#' 
#' Compare all documents within a document term matrix that are dated (e.g., pubished) within a given number of days (window.size) from each other.
#' 
#' @param dtm a document-term matrix in the tm format
#' @param document.date a vector of date class, of the same length and order as the documents (rows) of the dtm.
#' @param window.size the timeframe in days within which articles must occur in order to be compared. e.g., if 0, articles are only compared to articles of the same day. If 1, articles are compared to all articles of the previous, same or next day.
#' @param time.unit a string indicating what time unit to use. Can be 'mins','hours','days','months' or 'years'.
#' @param window.direction For a more specific selection of which articles in the window to compare to. This is given with a combination of the symbols '<' (before x) '=' (simultanous with x) and '>' (after x). default is '<=>', which means all articles. '<>' means all articles before or after the [time.unit] of an article itself. '<' means all previous articles, and '<=' means all previous and simultaneous articles. etc.  
#' @param measure the measure that should be used to calculate similarity/distance/adjacency. Currently only cosine is supported
#' @param min.similarity a threshold for similarity. lower values are deleted
#' @param n.topsim An alternative or additional sort of threshold for similarity. Only keep the [n.topsim] highest similarities for x.
#' @param only.from A vector of ids that match the documents (rownames) in dtm. Use to compare only these documents to other documents.
#' @param get.overlap.terms Add the overlapping terms of documents to the output. 
#' @param only.complete.window if True, only compare articles (x) of which a full window of reference articles (y) is available. Thus, for the first and last [window.size] days, there will be no results for x.
#' @param return.date If true, the dates for x and y are given in the output
#' @param return.zeros If true, all comparison results are returned, including those with zero similarity (quite possibly the worst thing to do with large data)
#' @return A data frame with columns x, y and similarity. If return.date == T, date.x and date.y are returned as well.
#' @export
documents.window.compare <- function(dtm, document.date, window.size=3, time.unit='days', window.direction='<=>', measure='cosine', min.similarity=NULL, n.topsim=NULL, only.from=NULL, return.date=F, return.datedif=T, return.zeros=F, only.complete.window=F){
  message('Indexing articles by date/time')
  datetime = as.POSIXct(document.date)
  datetime = aggregateTimeUnits(datetime, time.unit)
  datetimeseq = seq.POSIXt(min(datetime), max(datetime), by=time.unit)
  if(time.unit %in% c('days','months','years')) datetimeseq = as.POSIXct(format(datetimeseq, '%Y-%m-%d')) # because f! you POSIXt
    
  nonempty = which(datetimeseq %in% unique(datetime))
  nonempty_datetime_ids = llply(datetimeseq[nonempty], function(dtime) which(datetime == dtime), .progress='text')
  datetime_ids = vector("list", length(datetimeseq))
  datetime_ids[nonempty] = nonempty_datetime_ids
  #datetime_ids = llply(datetimeseq, function(dtime) which(datetime == dtime))
  #nonempty = which(laply(datetime_ids, length) > 0)
  
  message('Comparing documents')
  window = getWindow(window.size, window.direction)
  if(only.complete.window & '<' %in% window.direction) nonempty = nonempty[nonempty > window.size]
  if(only.complete.window & '>' %in% window.direction) nonempty = nonempty[nonempty <= length(datetime_ids)-window.size]
  output = ldply(nonempty, function(i) ldply_documents.compare(i, dtm, datetime_ids, window, measure, min.similarity, n.topsim, only.from, return.zeros), .progress='text')
  output = output[,!colnames(output) == '.id']
  if(return.date | return.datedif) {
    message('Matching document dates')
    date.x = document.date[match(output$x, rownames(dtm))]
    date.y = document.date[match(output$y, rownames(dtm))]
    if(return.datedif){
      output$daydif = difftime(date.y, date.x, units = 'days')
    }
    if(return.date){
      output$date.x = date.x
      output$date.y = date.y
    }
  }
  output
}

ldply_documents.compare <- function(i, dtm, datetime_ids, window, measure, min.similarity, n.topsim, only.from, return.zeros){
  ## special function to be used in ldply in document.window.compare
  dtm.x_indices = unique(datetime_ids[[i]])
  dtm.y_indices = unique(unlistWindow(datetime_ids,i,window))
  if(is.null(dtm.y_indices)) return(NULL)
  documents.compare(dtm[dtm.x_indices,], dtm[dtm.y_indices,], measure, min.similarity, n.topsim, only.from, return.zeros)
}
