d = data.frame(conversation=c(1,1,1,1,1,1,2,2,2,2),
               author=c('Alice','Bob','Alice','Charlie','Bob','Bob','Alice','Bob','Alice','Bob'),
               order.nr=c(1,2,3,4,5,6,1,2,3,4))

d
g = author.coincidence.graph(d$conversation, d$author) # In how many conversations did author.X and author.Y communicate?
plot(g, edge.label=E(g)$weight, vertex.size=50, edge.label.cex=3, edge.width=E(g)$weight*5) 
g = author.coincidence.graph(d$conversation, d$author, 'overlap_jacard') # Similar to default (coincidence_count) but with direction (by dividing coincidence by number of conversations author participated in)
plot(g, edge.label=E(g)$weight, vertex.size=50, edge.label.cex=3, edge.width=E(g)$weight*5)
g = author.coincidence.graph(d$conversation, d$author, 'cosine') # Cosine can be used to also take into account how many times each author participated within conversations
plot(g, edge.label=round(E(g)$weight,2), vertex.size=50, edge.label.cex=3, edge.width=E(g)$weight*5)

d
g = previous.authors.graph(d$conversation, d$author, d$order.nr, lookback=1) # how many times did author.X communicate directly after author.Y? 
plot(g, edge.label=E(g)$weight, vertex.size=50, edge.label.cex=3, edge.width=E(g)$weight*5)
g = previous.authors.graph(d$conversation, d$author, d$order.nr, lookback=2) # how many times did author.X communicate within two messages after author.Y? 
plot(g, edge.label=E(g)$weight, vertex.size=50, edge.label.cex=3, edge.width=E(g)$weight*5)

###

load('demo/parlproceedings_lda.rdata')

head(m$meta)
m$meta$author = paste(m$meta$name, m$meta$party)

g = previous.authors.graph(m$meta$meeting, m$meta$author, m$meta$order.nr, lookback=5) # how many times did author.X communicate directly after author.Y? 

E(g)$width = log(E(g)$weight)
E(g)$arrow.size = E(g)$width / 50
V(g)$label = ''
V(g)$size = log(V(g)$n.messages)/2

graph.plot(g, min.edge=1)
