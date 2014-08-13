### PREPARE DATA

#' Estimate a topic model using the lda package
#' 
#' Estimate an LDA topic model using the \code{\link{lda.collapsed.gibbs.sampler}} function
#' The parameters other than dtm are simply passed to the sampler but provide a workable default.
#' See the description of that function for more information
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.dtm.create}})
#' @param K the number of clusters
#' @param num.iterations the number of iterations
#' @param alpha the alpha parameter
#' @param eta the eta parameter
#' @return A fitted LDA model (see \code{\link{lda.collapsed.gibbs.sampler}})
#' @export
lda.fit <- function(dtm, K=50, num.iterations=250, alpha=50/K, eta=.01, burnin=250, compute.log.likelihood=F) {
  dtm = dtm[row_sums(dtm) > 0,col_sums(dtm) > 0]
  x = dtm2ldaformat(dtm)
  m = lda.collapsed.gibbs.sampler(x$documents, vocab=x$vocab, K=K, num.iterations=num.iterations, 
                                  alpha=alpha, eta=eta, burnin=burnin, compute.log.likelihood=compute.log.likelihood)
  m$dtm = dtm
  m
}

#' Add document meta to LDA output
#' 
#' Add a dataframe containing document meta to the output (a list) of \code{\link{lda.collapsed.gibbs.sampler}}. 
#' 
#' @param m The output of \code{\link{lda.collapsed.gibbs.sampler}}   
#' @param lda.document.ids A vector with document ids of the same length and order as the LDA output (matching the columns of m$document_sums)
#' @param meta A data.frame with document meta. Has to contain a vector to match the lda.document.ids
#' @param match.by The name of the vector in meta that matches the lda.document.ids 
#' @return The LDA output appended with document meta
#' @export
lda.match.meta <- function(m, document.ids, meta, match.by = 'id'){
  if('meta' %in% names(m)) {m$meta = meta[match(document.ids, meta[,match.by]),]
  } else m = c(m, list(meta=meta[match(document.ids, meta[,match.by]),]))
  m
}


#' Get the topics per document, optionally merged with 
#' 
#' Return a data frame containing article metadata and topic occurence per document
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{dtm.create}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
lda.topics.per.document <- function(topics) {
  ids = as.numeric(rownames(topics$dtm))
  cbind(id=ids, data.frame(t(topics$document_sums)))
}

### PLOT LDA TOPICS

#' Plot all topics
#' 
#' Write plots for all topics with \code{\link{lda.plot.topic}} in designated folder
#' 
#' @param m The output of \code{\link{lda.collapsed.gibbs.sampler}}   
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param path The path for a folder where output will be saved
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @return Nothing
#' @export
lda.plot.alltopics <- function(m, time_var, category_var, path, date_interval='day', value='total'){
  for(topic_nr in 1:nrow(m$document_sums)){
    print(paste('Plotting:',topic_nr))
    fn = paste(path, topic_nr, ".png", sep="")
    if (!is.null(fn)) png(fn, width=1280,height=800)
    lda.plot.topic(m, topic_nr, time_var, category_var, date_interval, value=value)
    if (!is.null(fn)) dev.off()
  }
  par(mfrow=c(1,1), mar=c(3,3,3,3))
}

#' Plots topic wordcloud, and attention over time and per category
#' 
#' Plots \code{\link{lda.plot.wordcloud}}, \code{\link{lda.plot.time}} and \code{\link{lda.plot.category}}
#' 
#' @param m The output of \code{\link{lda.collapsed.gibbs.sampler}}
#' @param The index of the topic (1 to K)
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @return Nothing, just plots
#' @export
lda.plot.topic <- function(m, topic_nr, time_var, category_var, date_interval='day', pct=F, value='total'){
  par(mar=c(4.5,3,2,1), cex.axis=1.7)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2.5,1.5), heights=c(1,2))
  lda.plot.time(m, topic_nr, time_var, date_interval, pct=pct, value=value)
  lda.plot.wordcloud(m, topic_nr)
  lda.plot.category(m, topic_nr, category_var, pct=pct, value=value)
  par(mfrow=c(1,1), mar=c(3,3,3,3))
}

#' Change date object to date_interval
#' 
#' Change date object to date_interval
#' 
#' @param time_var A vector of Date values
#' @param date_interval The desired date_interval ('day','week','month', or 'year')
#' @return A vector of Date values
#' @export
lda.prepare.time.var <- function(time_var, date_interval){
  if(class(time_var) == 'Date'){
    if(date_interval == 'day') time_var = as.Date(format(time_var, '%Y-%m-%d'))
    if(date_interval == 'month') time_var = as.Date(paste(format(time_var, '%Y-%m'),'-01',sep=''))
    if(date_interval == 'week') time_var = as.Date(paste(format(time_var, '%Y-%W'),1), '%Y-%W %u')
    if(date_interval == 'year') time_var = as.Date(paste(format(time_var, '%Y'),'-01-01',sep=''))
  } 
  time_var
}

#' Add empty values for pretty plotting
#' 
#' When plotting a timeline, gaps in date_intervals are ignored. For the attention for topics gaps should be considered as having value 0.   
#' 
#' @param d A data.frame with the columns 'time' (Date) and 'value' (numeric)  
#' @param date_interval The date_interval is required to know what the gaps are
#' @return A data.frame with the columns 'time' (Date) and 'value' (numeric)  
#' @export
lda.fill.time.gaps <- function(d, date_interval){
  if(class(d$time) == 'numeric'){
    for(t in min(d$time):max(d$time)) 
      if(!t %in% d$time) d = rbind(d, data.frame(time=t, value=0))
  }
  if(class(d$time) == 'Date'){
    date_sequence = seq.Date(from=min(d$time), to=max(d$time), by=date_interval)
    for(i in 1:length(date_sequence)){
      t = date_sequence[i]
      if(!t %in% d$time) d = rbind(d, data.frame(time=t, value=0))
    }
  }
  d[order(d$time),]
}

#' Prepares the topic values per document for plotting 
#' 
#' Prepares the topic values per document for plotting
#' 
#' @param m The output of \code{\link{lda.collapsed.gibbs.sampler}}
#' @param break_var A break vector to aggregate topic values per document
#' @param The index of the topic (1 to K)
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @return The aggregated/transformed topic values
#' @export
lda.prepare.plot.values <- function(m, break_var, topic_nr, pct=F, value='total', filter=NULL){
  hits = m$document_sums[topic_nr,]
  d = aggregate(hits, by=list(break_var=break_var), FUN='sum') 
  if(value == 'relative'){
    total_hits = colSums(m$document_sums)  
    totals = aggregate(total_hits, by=list(break_var=break_var), FUN='sum')
    d$x = d$x / totals$x
  }
  if(pct == T) d$x = d$x / sum(d$x)
  d
}

#' Plots topic values over time
#' 
#' Plots the attention for a topic over time
#' 
#' @param m The output of \code{\link{lda.collapsed.gibbs.sampler}}
#' @param The index of the topic (1 to K)
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @param return.values Logical. If true, data that is plotted is returned as a data.frame
#' @return data.frame for plotted values
#' @export
lda.plot.time <- function(m, topic_nr, time_var, date_interval='day', pct=F, value='total', return.values=F){
  par(mar=c(3,3,3,1))
  time_var = lda.prepare.time.var(time_var, date_interval)  
  d = lda.prepare.plot.values(m, break_var=time_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('time','value')
  d = lda.fill.time.gaps(d, date_interval)
  plot(d$time, d$value, type='l', xlab='', main='', ylab='', xlim=c(min(d$time), max(d$time)), ylim=c(0, max(d$value)), bty='L', lwd=5, col='darkgrey')
  par(mar=c(3,3,3,3))
  if(return.values==T) d
}

#' Plots topic values per category
#' 
#' Plots the attention for a topic per category
#' 
#' @param m The output of \code{\link{lda.collapsed.gibbs.sampler}}
#' @param The index of the topic (1 to K)
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @param return.values Logical. If true, data that is plotted is returned as a data.frame
#' @return data.frame for plotted values
#' @export
lda.plot.category <- function(m, topic_nr, category_var, pct=F, value='total', return.values=F){
  par(mar=c(15,3,1,2))
  d = lda.prepare.plot.values(m, break_var=category_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('category','value')
  barplot(as.matrix(t(d[,c('value')])), main='', beside=TRUE,horiz=FALSE,
          density=NA,
          col='darkgrey',
          xlab='',
          ylab="",
          axes=T, names.arg=d$category, cex.names=0.8, cex.axis=0.8, adj=1, las=2)
  par(mar=c(3,3,3,3))
  if(return.values==T) d
}

#' Plot wordcloud for LDA topic
#' 
#' Plots a wordcloud of the top words per topic
#' 
#' @param m The output of \code{\link{lda.collapsed.gibbs.sampler}}
#' @param topic_nr The index of the topic (1 to K)
#' @return Nothing, just plots
#' @export
lda.plot.wordcloud <- function(m, topic_nr){
  x = m$topics[topic_nr,]
  x = sort(x[x>5], decreasing=T)[1:100]
  x = x[!is.na(x)]
  names = sub("/.*", "", names(x))
  freqs = x**.5
  pal <- brewer.pal(6,"YlGnBu")
  wordcloud(names, freqs, scale=c(6,.5), min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
}
