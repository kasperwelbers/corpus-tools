


Difference howto_latent_dirichlet_allocation.Rmd and this file
========================================================
This file is almost identical to howto_latent_dirichlet_allocation.Rmd.
The difference is that this file uses the topicmodels package, whereas the other uses the lda package.
Both packages offer LDA modelling, and both have certain advanges and disadvantages.
At some point we might decide to only support one package.
Note that at present some of the code is a bit messy and redundant, since we rather hastily copy-edited some functions to work with the topicmodels package.


Latent Dirichlet Allocation
========================================================

Topic modelling techniques such as Latent Dirichlet Allocation (LDA) can be a usefull tool for social scientists to analyze large amounts of natural language data. Algorithms for LDA are available in R, for instance in the `lda` package. In this howto we demonstrate several function in the `corpustools` package that we developed to facilitate the use of LDA, based on the `lda` package.

As a starting point we use a Document Term Matrix (dtm) in the `DocumentTermMatrix` format offered in the `tm` package. Note that we also offer a howto for creating the dtm. 


```r
library(corpustools)
```

```
## Loading required package: slam
## Loading required package: Matrix
## Loading required package: lda
## Loading required package: tm
## Loading required package: reshape2
## Loading required package: topicmodels
## Loading required package: RColorBrewer
## Loading required package: wordcloud
## Loading required package: Rcpp
```

```r
load("wos_comsci_dtm.rdata")  ## Abstracts in 10 communication sciences journals from Web of Science, with 'social network' as a topic, from 2000 till 2010
dtm
```

```
## A document-term matrix (848 documents, 9753 terms)
## 
## Non-/sparse entries: 84132/8186412
## Sparsity           : 99%
## Maximal term length: 80 
## Weighting          : term frequency (tf)
```


Not all terms are equally informative of the underlying semantic structures of texts, and some terms are rather useless for this purpose. For interpretation and computational purposes it is worthwhile to delete some of the less usefull words from the dtm before fitting the LDA model. We offer the `term.statistics` function to get some basic information on the vocabulary (i.e. the total set of terms) of the corpus.


```r
termstats = term.statistics(dtm)
termstats[sample(1:nrow(termstats), 10), ]
```

```
##                            term characters number nonalpha termfreq
## high-resolution high-resolution         15  FALSE     TRUE        1
## Change                   Change          6  FALSE    FALSE        4
## correctlyus         correctlyus         11  FALSE    FALSE        1
## mp3                         mp3          3   TRUE    FALSE        4
## Amer                       Amer          4  FALSE    FALSE        6
## li                           li          2  FALSE    FALSE        1
## sort                       sort          4  FALSE    FALSE        2
## attributable       attributable         12  FALSE    FALSE        4
## July                       July          4  FALSE    FALSE        1
## Habbo                     Habbo          5  FALSE    FALSE        1
##                 docfreq reldocfreq   tfidf
## high-resolution       1   0.001179 0.03366
## Change                4   0.004717 0.02996
## correctlyus           1   0.001179 0.07370
## mp3                   2   0.002358 0.11376
## Amer                  6   0.007075 0.03635
## li                    1   0.001179 0.04610
## sort                  2   0.002358 0.03906
## attributable          4   0.004717 0.04242
## July                  1   0.001179 0.04610
## Habbo                 1   0.001179 0.03800
```


We can now filter out words based on this information. In our example, we filter on terms that occur at least in two documents and that do not contain numbers. We also select only the 3000 terms with the highest tf-idf score (this is not a common standard. For large corpora it makes sense to include more terms). 


```r
termstats = termstats[termstats$docfreq > 1 & termstats$number == F, ]
voca = as.character(termstats[order(termstats$tfidf, decreasing = T), ][1:3000, 
    "term"])
filtered_dtm = dtm[, voca]  # select only the terms we want to keep
```


Now we are ready to fit the model! We made a wrapper called `topmod.lda.fit` for the `LDA` function in the `topicmodels` package. This wrapper really doesn't do anything interesting, except for deleting empty columns/rows from the dtm. The main reason for the wrapper is that we also used one for working with the other package that offers LDA modeling (for the use of the `lda.collapsed.gibbs.sampler` in the `lda` package some additional steps were required, so the wrapper makes more sense there)

The main input for `topmod.lda.fit` is:
- the document term matrix
- K: the number of topics (this has to be defined a priori)
- Optionally, it can be usefull to increase the number of iterations. This takes more time, but increases performance (to some point)


```r
m = topmod.lda.fit(filtered_dtm, K = 30, num.iterations = 1000)
terms(m, 10)[, 1:5]  # show first 5 topics, with ten top words per topic
```

```
##       Topic 1       Topic 2        Topic 3       Topic 4        
##  [1,] "student"     "exchange"     "campaign"    "collaboration"
##  [2,] "Facebook"    "resident"     "audience"    "human"        
##  [3,] "profile"     "variable"     "marketing"   "older"        
##  [4,] "college"     "distance"     "channel"     "region"       
##  [5,] "post"        "neighborhood" "consumer"    "scientific"   
##  [6,] "privacy"     "frequency"    "advertising" "law"          
##  [7,] "alcohol"     "agent"        "product"     "advocacy"     
##  [8,] "personality" "mobility"     "radio"       "presentation" 
##  [9,] "topic"       "desire"       "along"       "go"           
## [10,] "learning"    "provision"    "business"    "University"   
##       Topic 5   
##  [1,] "woman"   
##  [2,] "%"       
##  [3,] "ask"     
##  [4,] "barrier" 
##  [5,] "she"     
##  [6,] "percent" 
##  [7,] "p"       
##  [8,] "african" 
##  [9,] "planning"
## [10,] "american"
```


We now have a fitted lda model. The terms function shows the most prominent words for each topic (we only selected the first 4 topics for convenience). 

One of the thing we can do with the LDA topics, is analyze how much attention they get over time, and how much they are used by different sources (e.g., people, newspapers, organizations). To do so, we need to match this article metadata. We can order the metadata to the documents in the LDA model by matching it to the documents slot.


```r
load("wos_comsci_meta.rdata")
colnames(meta)  # the id column matches the rownames of the dtm
```

```
## [1] "id"            "date"          "journal"       "length"       
## [5] "journal.top10"
```

```r
meta = meta[match(m@documents, meta$id), ]
```


We can now do some plotting. First, we can make a wordcloud for a more fancy (and actually quite informative and intuitive) representation of the top words of a topic.


```r
topic_term_matrix = posterior(m)$terms
topics.plot.wordcloud(topic_term_matrix, topic_nr = 1)
```

![plot of chunk unnamed-chunk-7](figures_lda_topmod/unnamed-chunk-7.png) 


With `lda.plot.time` and `lda.plot.category`, we can plot the salience of the topic over time and for a given categorical variable.


```r
topics.plot.time
```

```
## function(document_sums, topic_nr, time_var, date_interval='day', pct=F, value='total', return.values=F){
##   par(mar=c(3,3,3,1))
##   time_var = prepare.time.var(time_var, date_interval)  
##   d = prepare.topics.plot.values(document_sums, break_var=time_var, topic_nr=topic_nr, pct=pct, value=value)
##   colnames(d) = c('time','value')
##   d = fill.time.gaps(d, date_interval)
##   plot(d$time, d$value, type='l', xlab='', main='', ylab='', xlim=c(min(d$time), max(d$time)), ylim=c(0, max(d$value)), bty='L', lwd=5, col='darkgrey')
##   par(mar=c(3,3,3,3))
##   if(return.values==T) d
## }
## <environment: namespace:corpustools>
```

```r
topic_document_matrix = documentsums(m, weight.by.dtm = dtm)  # the documentsums function extracts a matrix identical to the documentsums slot of the output of the lda.collapsed.gibbs.sampler in the lda package. If the dtm is given in the weight.by.dtm parameter, then word-to-topic assignments are multiplied by the word occurence (which, I believe, is also what the lda package does)
topics.plot.time(topic_document_matrix, 1, meta$date, date_interval = "month", 
    value = "relative")
```

![plot of chunk unnamed-chunk-8](figures_lda_topmod/unnamed-chunk-8.png) 

```r
# Sidenote: the `return.values` argument can be set to TRUE to also let the
# function output the values that are plotted.
```


In our example data, we can use the names of the journals as categories. However, since there are so many journals, this becomes messy. We therefore only look at the top.10 most frequent journals (in our sample) and categorize the rest as `other`.


```r
topics.plot.category(topic_document_matrix, 1, meta$journal.top10, value = "relative")
```

![plot of chunk unnamed-chunk-9](figures_lda_topmod/unnamed-chunk-9.png) 

```r
# Sidenote: the `return.values` argument can be set to TRUE to also let the
# function output the values that are plotted.
```


Finally, it can be usefull to print all this information together. That is what the following function does.


```r
topics.plot.topic(document_sums = topic_document_matrix, topics = topic_term_matrix, 
    1, meta$date, meta$journal.top10, date_interval = "year", value = "relative")
```

![plot of chunk unnamed-chunk-10](figures_lda_topmod/unnamed-chunk-101.png) 

```r
topics.plot.topic(document_sums = topic_document_matrix, topics = topic_term_matrix, 
    2, meta$date, meta$journal.top10, date_interval = "year", value = "relative")
```

![plot of chunk unnamed-chunk-10](figures_lda_topmod/unnamed-chunk-102.png) 

```r
topics.plot.topic(document_sums = topic_document_matrix, topics = topic_term_matrix, 
    10, meta$date, meta$journal.top10, date_interval = "year", value = "relative")
```

![plot of chunk unnamed-chunk-10](figures_lda_topmod/unnamed-chunk-103.png) 

