library(corpustools)
library(networktools)

load('demo/wos_comsci_lda.rdata')

names(m)
document.topic.matrix = t(m$document_sums)
document.topic.matrix = Matrix(document.topic.matrix, sparse=T)

dim(document.topic.matrix)
head(m$meta)

g = content.similarity.graph(document.topic.matrix, 
                             vertex.grouping.vars=list(journal=m$meta$journal,
                                                       journal.top10=m$meta$journal.top10),
                             similarity.measure='correlation',
                             min.similarity=0)

g = graph.color.vertices(g, V(g)$journal.top10) 
V(g)$color[V(g)$journal.top10 == 'Other'] = 'white'
V(g)$size = sqrt(V(g)$n)*2
V(g)$label = ''
E(g)$width = E(g)$width / 5

graph.plot(g, min.edge=0.5)

##### over time

g = content.similarity.graph(document.topic.matrix, 
                             vertex.grouping.vars=list(journal=m$meta$journal,
                                                       journal.top10=m$meta$journal.top10,
                                                       year=format(m$meta$date,'%Y')), 
                             similarity.measure='correlation',
                             min.similarity=0)

g = graph.color.vertices(g, V(g)$journal.top10) 
V(g)$color[V(g)$journal.top10 == 'Other'] = 'white'
V(g)$size = sqrt(V(g)$n)*6
V(g)$label = ''
E(g)$width = E(g)$width / 2

graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2000)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2001)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2002)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2003)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2004)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2005)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2006)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2007)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2008)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2009)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2010)


##### with topics coloured

lda.plot.topic(m, 1, m$meta$date, m$meta$journal.top10, date_interval='year', value='relative')

g = content.similarity.graph(document.topic.matrix, 
                             vertex.grouping.vars=list(journal=m$meta$journal,
                                                       journal.top10=m$meta$journal.top10,
                                                       year=format(m$meta$date,'%Y')), 
                             similarity.measure='correlation',
                             min.similarity=0,
                             content.totals.as.vertexmeta=1)

g = graph.color.vertices(g, V(g)$C1)
V(g)$size = sqrt(V(g)$n)*6
V(g)$label = ''
E(g)$width = E(g)$width / 2

graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2000)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2001)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2002)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2003)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2004)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2005)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2006)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2007)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2008)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2009)
graph.plot(g, min.edge=0.5, select.vertices=V(g)$year == 2010)



##
##












load('demo/parlproceedings_lda.rdata')

document.topic.matrix = t(m$document_sums)
document.topic.matrix = Matrix(document.topic.matrix, sparse=T)

dim(document.topic.matrix)
head(m$meta)

g = content.similarity.graph(document.topic.matrix, 
                             vertex.grouping.vars=list(party=m$meta$party, 
                                                       year=format(m$meta$date, '%Y')), 
                             similarity.measure='correlation')

g = graph.color.vertices(g, V(g)$party) # color vertices by party
V(g)$label = as.character(V(g)$year) # use year as vertex label

graph.plot(g, min.edge=0.1)

####

dtm = weightTfIdf(dtm)

g = content.similarity.graph(m$dtm, 
                             vertex.grouping.vars=list(party=m$meta$party, 
                                                       year=format(m$meta$date, '%Y')), 
                             similarity.measure='correlation',
                             min.similarity=0.5)

min(E(g)$weight)


g = graph.color.vertices(g, V(g)$party) # color vertices by party
V(g)$label = as.character(V(g)$year) # use year as vertex label

graph.plot(g, min.edge=0.6)




g = content.similarity.graph(m$dtm, 
                             vertex.grouping.vars=list(id=m$meta$id), 
                             similarity.measure='correlation')







#####

lda.plot.topic(m, 1, m$meta$date, m$meta$party, date_interval='year')


g = content.similarity.graph(document.topic.matrix, 
                             vertex.grouping.vars=list(party=m$meta$party, 
                                                       year=format(m$meta$date, '%Y')), 
                             similarity.measure='correlation',
                             content.totals.as.vertexmeta=1:10)

data.frame(vertex.attributes(g))
g = graph.color.vertices(g, V(g)$party) # color vertices by party
V(g)$label = as.character(V(g)$year) # use year as vertex label

graph.plot(g, min.edge=0.1)



##### 

