#' Plot all topics
#' 
#' Write plots for all topics with \code{\link{topics.plot.topic}} in designated folder
#' 
#' @param document_sums. A matrix where rows are topics and columns are articles. values represent how many words in the article belong to each topic. for collapsed.gibbs.sampler (lda package) get this matrix from the $document_sums object. for the topicmodels package, use the documentsums function (from corpustools).
#' @param topics a matrix in which rows are topics and columns are terms. values represent how prominent the word is in a topic. This matrix can for instance be obtained from the $topics object of the output of collapsed.gibbs.sampler, or when using the topicmodels package from posterior(m)$terms (where m is or stems from the LDA class)
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param path The path for a folder where output will be saved
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @return Nothing
#' @export
topics.plot.alltopics <- function(document_sums, topics, time_var, category_var, path, date_interval='day', value='total'){
  for(topic_nr in 1:nrow(document_sums)){
    print(paste('Plotting:',topic_nr))
    fn = paste(path, topic_nr, ".png", sep="")
    if (!is.null(fn)) png(fn, width=1280,height=800)
    topics.plot.topic(document_sums, topics, topic_nr, time_var, category_var, date_interval, value=value)
    if (!is.null(fn)) dev.off()
  }
  par(mfrow=c(1,1), mar=c(3,3,3,3))
}

#' Plots topic wordcloud, and attention over time and per category
#' 
#' Plots \code{\link{topics.plot.wordcloud}}, \code{\link{topics.plot.time}} and \code{\link{topics.plot.category}}
#' 
#' @param document_sums. A matrix where rows are topics and columns are articles. values represent how many words in the article belong to each topic. for collapsed.gibbs.sampler (lda package) get this matrix from the $document_sums object. for the topicmodels package, use the documentsums function (from corpustools).
#' @param topics a matrix in which rows are topics and columns are terms. values represent how prominent the word is in a topic. This matrix can for instance be obtained from the $topics object of the output of collapsed.gibbs.sampler, or when using the topicmodels package from posterior(m)$terms (where m is or stems from the LDA class)
#' @param The index of the topic (1 to K)
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @return Nothing, just plots
#' @export
topics.plot.topic <- function(document_sums, topics, topic_nr, time_var, category_var, date_interval='day', pct=F, value='total'){
  par(mar=c(4.5,3,2,1), cex.axis=1.7)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2.5,1.5), heights=c(1,2))
  topics.plot.time(document_sums, topic_nr, time_var, date_interval, pct=pct, value=value)
  topics.plot.wordcloud(topics, topic_nr)
  topics.plot.category(document_sums, topic_nr, category_var, pct=pct, value=value)
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
prepare.time.var <- function(time_var, date_interval){
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
fill.time.gaps <- function(d, date_interval){
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
#' @param document_sums. A matrix where rows are topics and columns are articles. values represent how many words in the article belong to each topic. for collapsed.gibbs.sampler (lda package) get this matrix from the $document_sums object. for the topicmodels package, use the documentsums function (from corpustools).
#' @param break_var A break vector to aggregate topic values per document
#' @param The index of the topic (1 to K)
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @return The aggregated/transformed topic values
#' @export
prepare.topics.plot.values <- function(document_sums, break_var, topic_nr, pct=F, value='total', filter=NULL){
  hits = document_sums[topic_nr,]
  d = aggregate(hits, by=list(break_var=break_var), FUN='sum') 
  if(value == 'relative'){
    total_hits = colSums(document_sums)  
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
#' @param document_sums. A matrix where rows are topics and columns are articles. values represent how many words in the article belong to each topic. for collapsed.gibbs.sampler (lda package) get this matrix from the $document_sums object. for the topicmodels package, use the documentsums function (from corpustools).
#' @param The index of the topic (1 to K)
#' @param time_var A vector with time stamps (either numeric or Date class) of the same length and order of the documents (rows) in m$document_sums
#' @param date_interval The interval for plotting the values over time. Can be: 'day', 'week', 'month' or 'year'
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @param return.values Logical. If true, data that is plotted is returned as a data.frame
#' @return data.frame for plotted values
#' @export
topics.plot.time <- function(document_sums, topic_nr, time_var, date_interval='day', pct=F, value='total', return.values=F){
  par(mar=c(3,3,3,1))
  time_var = prepare.time.var(time_var, date_interval)  
  d = prepare.topics.plot.values(document_sums, break_var=time_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('time','value')
  d = fill.time.gaps(d, date_interval)
  plot(d$time, d$value, type='l', xlab='', main='', ylab='', xlim=c(min(d$time), max(d$time)), ylim=c(0, max(d$value)), bty='L', lwd=5, col='darkgrey')
  par(mar=c(3,3,3,3))
  if(return.values==T) d
}

#' Plots topic values per category
#' 
#' Plots the attention for a topic per category
#' 
#' @param document_sums. A matrix where rows are topics and columns are articles. values represent how many words in the article belong to each topic. for collapsed.gibbs.sampler (lda package) get this matrix from the $document_sums object. for the topicmodels package, use the documentsums function (from corpustools).
#' @param The index of the topic (1 to K)
#' @param category_var A vector with id values of the same length and order of the documents (rows) in m$document_sums
#' @param pct Show topic values as percentages
#' @param value Show topic values as 'total', or as 'relative' to the attention for other topics
#' @param return.values Logical. If true, data that is plotted is returned as a data.frame
#' @return data.frame for plotted values
#' @export
topics.plot.category <- function(document_sums, topic_nr, category_var, pct=F, value='total', return.values=F){
  par(mar=c(10,3,1,2))
  d = prepare.topics.plot.values(document_sums, break_var=as.character(category_var), topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('category','value')
  barplot(as.matrix(t(d[,c('value')])), main='', beside=TRUE,horiz=FALSE,
          density=NA,
          col='darkgrey',
          xlab='',
          ylab="",
          axes=T, names.arg=d$category, cex.names=1, cex.axis=1, adj=1, las=2)
  par(mar=c(3,3,3,3))
  if(return.values==T) d
}

#' Plot wordcloud for LDA topic
#' 
#' Plots a wordcloud of the top words per topic
#' 
#' @param topics a matrix in which rows are topics and columns are terms. values represent how prominent the word is in a topic. This matrix can for instance be obtained from the $topics object of the output of collapsed.gibbs.sampler, or when using the topicmodels package from posterior(m)$terms (where m is or stems from the LDA class)
#' @param topic_nr The index of the topic (1 to K)
#' @return Nothing, just plots
#' @export
topics.plot.wordcloud <- function(topics, topic_nr, wordsize_scale=0.5){
  x = topics[topic_nr,]
  x = sort(x, decreasing=T)[1:100]
  x = x[!is.na(x)]
  names = sub("/.*", "", names(x))
  freqs = x^wordsize_scale
  pal <- brewer.pal(6,"YlGnBu")
  wordcloud(names, freqs, scale=c(6,.5), min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
}
