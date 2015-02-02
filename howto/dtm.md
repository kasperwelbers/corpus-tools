

The Document-Term Matrix (dtm)
========================================================

For many methods of text analysis, specifically the so-called bag-of-word approaches, the common data structure for the corpus is a document-term matrix (dtm). This is a matrix in which the rows represent documents and the columns represent terms (e.g., words, lemmata). The values represent how often each word occured in each document. 

For example, in this dtm we can see how often the words 'spam', 'bacon', and 'egg', occured in each of the 4 documents:

```r
m = matrix(sample(0:2, replace=T, 12), ncol=3, dimnames=list(documents=c('doc1','doc2','doc3','doc4'), terms=c('spam','bacon','egg')))
m
```

```
##          terms
## documents spam bacon egg
##      doc1    1     2   1
##      doc2    2     2   0
##      doc3    0     1   0
##      doc4    1     0   1
```

Commonly, this matrix is very sparse. That is, a large majority of the values will be zero, since documents generally only use a small portion of the words used in the entire corpus. For efficient computing and data storage, it is therefore worthwhile to not simply use a matrix, but a type of sparse matrix. We suggest the use of the `DocumentTermMatrix` class that is available in the `tm` (text mining) package. This is a type of sparse matrix dedicated to text analysis.


```r
library(tm)
dtm = as.DocumentTermMatrix(m, weight=weightTf)
dtm
```

```
## <<DocumentTermMatrix (documents: 4, terms: 3)>>
## Non-/sparse entries: 8/4
## Sparsity           : 33%
## Maximal term length: 5
## Weighting          : term frequency (tf)
```

Since the `tm` package is quite popular, this dtm class is compatible with various packages for text analysis. For many of the functions offered in the `corpustools` package we also use this dtm class as a starting point. In the next steps of this howto we provide some pointers for importing your data and transforming it into a dtm.

Creating the Document-Term Matrix from full-text
========================================================

If your data is in common, unprocessed textual form (such as this document) then a good approach is to use the `tm` package. `tm` offers various ways to import texts to the `tm` corpus structure. Additionally, it offers various functions to pre-process the data. For instance, removing stop-words, making everything lowercase and reducing words to their stem. From the `corpus` format the texts can then easily be tokenized (i.e. cut up into terms) and made into a dtm. If this is your weapon of choice, we gladly refer to the [tm package vignette](http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf).

Another option is using the `create_matrix` command from the [RTextTools](http://rtexttools.com) package, which is a convenient wrapper for `tm` with stemming and stop-word removal:


```r
library(RTextTools)
```

```
## Loading required package: SparseM
## 
## Attaching package: 'SparseM'
## 
## The following object is masked from 'package:base':
## 
##     backsolve
```

```r
create_matrix(c("One document", "And another"), removeStopwords=T, stemWords=T, language="english")
```

```
## <<DocumentTermMatrix (documents: 2, terms: 3)>>
## Non-/sparse entries: 3/3
## Sparsity           : 50%
## Maximal term length: 8
## Weighting          : term frequency (tf)
```

Creating the Document-Term Matrix from tokens
========================================================

It could also be that you prefer to pre-process and tokenize your data outside of R; 
possibly using more advanced methods such as lemmatizing and part-of-speech tagging, 
(that to our knowledge are not supported well within R). 
For reference, an open-source software package to accommodate this process that we are affiliated with is [AmCAT](https://github.com/amcat/). A howto for creating a DTM using amcat can be found [here](https://github.com/amcat/amcat-r/blob/master/howto/howto_corpus.Rmd)

If data has been pre-processed and tokenized elsewhere, you can usually export a list of words in a csv like format (e.g. the standardized conll format). This can be imported into R is as a data.frame (e.g., using read.csv), in which each row represents how often a token occured within a document. 

In the following example, we use the `sotu` data set provided with this package, which contains a list of tokens produced by using Stanford's CoreNLP preprocessing on the State of the Union addresses by presidents (George W.) Bush and Obama


```r
library(corpustools)
```

```
## Loading required package: slam
## Loading required package: Matrix
## Loading required package: lda
## Loading required package: reshape2
## Loading required package: topicmodels
## Loading required package: RColorBrewer
## Loading required package: wordcloud
```

```r
data(sotu)
head(sotu.tokens)
```

```
##         word sentence  pos      lemma offset       aid id pos1 freq
## 1         It        1  PRP         it      0 111541965  1    O    1
## 2         is        1  VBZ         be      3 111541965  2    V    1
## 3        our        1 PRP$         we      6 111541965  3    O    1
## 4 unfinished        1   JJ unfinished     10 111541965  4    A    1
## 5       task        1   NN       task     21 111541965  5    N    1
## 6         to        1   TO         to     26 111541965  6    ?    1
```

To our awareness, `tm` has no functions to directly transform this data format into a `DocumentTermMatrix`, but it can quite easily be casted into a sparse matrix, and then transformed into a dtm. The `dtm.create` function, offered in the `corpustools` package, can do this for you. For our example, we use the [lemma](http://en.wikipedia.org/wiki/Lemma_(morphology)) of the words. In the `dtm.create` function we simply give the vector for the lemma, together with a vector indicating in which documents the lemma occured (aid).



```r
dtm = dtm.create(documents=sotu.tokens$aid, terms=sotu.tokens$lemma)
dtm
```

```
## <<DocumentTermMatrix (documents: 1090, terms: 5264)>>
## Non-/sparse entries: 60617/5677143
## Sparsity           : 99%
## Maximal term length: 35
## Weighting          : term frequency (tf)
```

Since we are normally not interested in punctuation, prepositions and the like, we can filter the data set on part-of-speech tag, for example keeping only the nouns:


```r
tokens = sotu.tokens[sotu.tokens$pos1 == 'N', ]
dtm = dtm.create(documents=tokens$aid, terms=tokens$lemma)
dtm
```

```
## <<DocumentTermMatrix (documents: 1078, terms: 2210)>>
## Non-/sparse entries: 15945/2366435
## Sparsity           : 99%
## Maximal term length: 17
## Weighting          : term frequency (tf)
```


As you can see, we now have 2210 unique terms rather than 5264. 
One of the cool things to do with a dtm is create a word cloud, which is made easy by the `dtm.wordcloud` function:


```r
t = term.statistics(dtm)
dtm.wordcloud(dtm)
```

![plot of chunk unnamed-chunk-8](figures_dtm/unnamed-chunk-8.png) 

For more information on how to work with `dtm`s using corpustools, please see the other howto's:

* [Frequency analysis](howto/frequency.md)
* [Corpus comparison](howto/compare.md)
* [LDA topic modeling](howto/lda.md)
