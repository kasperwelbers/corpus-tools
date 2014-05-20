library(corpustools)

load('demo/wos_comsci_dtm.rdata')
dtm

termstats = term.statistics(dtm)
head(termstats)
head(termstats[order(termstats$tfidf, decreasing=T),])

termstats = termstats[termstats$docfreq > 1 & termstats$nonalpha==F & termstats$number==F,]
voca = as.character(termstats[order(termstats$tfidf, decreasing=T),][1:3000,'term'])
filtered_dtm = dtm[,voca]

m = lda.fit(filtered_dtm, K=25, num.iterations=1000)
top.topic.words(m$topics)
names(m)

## add meta
load('demo/wos_comsci_meta.rdata')
head(meta)

m = lda.match.meta(m, rownames(m$dtm), meta, match.by='id')
names(m)

load('demo/wos_comsci_lda.rdata')

## visualize topics
lda.plot.wordcloud(m, 1)
lda.plot.topic(m, 1, m$meta$date, m$meta$journal.top10, 'year')

lda.plot.topic(m, 1, m$meta$date, m$meta$journal.top10, date_interval='year', value='relative')
lda.plot.topic(m, 2, m$meta$date, m$meta$journal.top10, date_interval='year', value='relative')
lda.plot.topic(m, 17, m$meta$date, m$meta$journal.top10, date_interval='year', value='relative')
