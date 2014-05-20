library(corpustools)

tokens = load('demo/rwanda_tokens.rdata')
head(td)
head(tokens)

dtm = dtm.create(tokens$aid, tokens$lemma, tokens$freq)
dtm

