library(corpustools)
setwd("~/projects/corpus-tools")

load('howto/wos_comsci_dtm.rdata')
load('howto/wos_comsci_meta.rdata')

## filter dtm
termstats = term.statistics(dtm)
termstats[sample(1:nrow(termstats), 10), ]

termstats = termstats[termstats$docfreq > 1 & termstats$number == F, ]
voca = as.character(termstats[order(termstats$tfidf, decreasing = T), ][1:3000, "term"])
filtered_dtm = dtm[, voca]  # select only the terms we want to keep

## fit model
m = topmod.lda.fit(filtered_dtm, K=50)
meta = topmod.order.meta(m, meta)

## plot
topic_nr = 1
topmod.plot.wordcloud(m, topic_nr)
topmod.plot.time(m, topic_nr, time_var=meta$date, date_interval='year', value='relative')
topmod.plot.category(m, topic_nr, category_var=meta$journal.top10, value='relative')

topmod.plot.topic(m, topic_nr, time_var=meta$date, category_var=meta$journal.top10, value='relative')
