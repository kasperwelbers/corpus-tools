Comparing corpora
-----------------

It can be interesting to compare vocabulary use in two corpora. For instance, to find out:
- which terms certain actors are more/less likely to use compared to other actors.
- which terms are more/less common in texts about certain actors/issues compared to other actors/issues.
- whether the use of certain terms has increased/decreased between two periods in time.

In this howto we demonstrate how to compare vocabulary use in two corpora, and offer a function to do so conveniently. 
For data we use wikinews articles about Iraq.


```r
library(corpustools)
```

```
## Loading required package: slam
## Loading required package: Matrix
## Loading required package: lda
## Loading required package: tm
## Loading required package: topicmodels
## Loading required package: RColorBrewer
## Loading required package: wordcloud
## Loading required package: Rcpp
```

```r
load("wikinews_iraq_dtm_meta.rdata")
dtm
```

```
## A document-term matrix (674 documents, 10707 terms)
## 
## Non-/sparse entries: 68649/7147869
## Sparsity           : 99%
## Maximal term length: 369 
## Weighting          : term frequency (tf)
```

```r
head(meta)
```

```
##         id       date   medium length       year      month       week
## 1 81112681 2013-08-29 Wikinews    406 2013-01-01 2013-08-01 2013-08-26
## 2 81112682 2012-01-20 Wikinews    149 2012-01-01 2012-01-01 2012-01-16
## 3 81112683 2011-02-06 Wikinews    486 2011-01-01 2011-02-01 2011-01-31
## 4 81112684 2010-09-23 Wikinews    314 2010-01-01 2010-09-01 2010-09-20
## 5 81112685 2013-08-30 Wikinews    394 2013-01-01 2013-08-01 2013-08-26
## 6 81112686 2009-08-12 Wikinews    244 2009-01-01 2009-08-01 2009-08-10
```


For example, lets split our corpus into those articles that mention Bush and those that do not:


```r
ncol(dtm)
```

```
## [1] 10707
```

```r
w = as.matrix(dtm[, "Bush"])
dtm.bush = dtm[w > 0, ]
dtm.rest = dtm[w == 0, ]
```


Now, we can compute and compare the term frequencies for both. First, we use the `term.statistics` function to get the term statistics (including term frequency) for both corpora. 


```r
terms.bush = term.statistics(dtm.bush)
terms.rest = term.statistics(dtm.rest)
head(terms.bush)
```

```
##                          term characters number nonalpha termfreq docfreq
## accuracy             accuracy          8  FALSE    FALSE        5       5
## administration administration         14  FALSE    FALSE       67      39
## America               America          7  FALSE    FALSE       74      64
## answer                 answer          6  FALSE    FALSE        9       7
## anyone                 anyone          6  FALSE    FALSE        4       2
## ask                       ask          3  FALSE    FALSE       35      25
##                reldocfreq    tfidf
## accuracy          0.04348 0.022752
## administration    0.33913 0.013215
## America           0.55652 0.006128
## answer            0.06087 0.019722
## anyone            0.01739 0.050177
## ask               0.21739 0.013605
```


We then match the term frequencies of dtm.rest to the term statistics of dtm.bush.

```r
freqs.rest = terms.rest[, c("term", "termfreq")]
terms.bush = merge(terms.bush, freqs.rest, all.x = TRUE, by = "term")
terms.bush[is.na(terms.bush)] = 0
head(terms.bush)
```

```
##   term characters number nonalpha termfreq.x docfreq reldocfreq   tfidf
## 1               0  FALSE    FALSE          1       1   0.008696 0.01956
## 2   >>          2  FALSE     TRUE          2       2   0.017391 0.05667
## 3    |          1  FALSE     TRUE        138      51   0.443478 0.02090
## 4   __          2  FALSE    FALSE          2       2   0.017391 0.01664
## 5    *          1  FALSE     TRUE         16      16   0.139130 0.01744
## 6    %          1  FALSE     TRUE         71      14   0.121739 0.05040
##   termfreq.y
## 1          1
## 2          0
## 3        519
## 4          8
## 5         64
## 6         98
```


`terms.bush` now contains the term statistics in the articles mentioning bush, 
as well as the frequency in the reference corpus consisting of the other articles.
We can now compute and sort by the overrepresentation of terms:


```r
terms.bush$relfreq.x = terms.bush$termfreq.x/sum(terms.bush$termfreq.x)
terms.bush$relfreq.y = terms.bush$termfreq.y/sum(terms.rest$termfreq)
terms.bush$over = terms.bush$relfreq.x/(terms.bush$relfreq.y + 0.001)
terms.bush = terms.bush[order(-terms.bush$over), ]
head(terms.bush, n = 10)
```

```
##                term characters number nonalpha termfreq.x docfreq
## 541            Bush          4  FALSE    FALSE        311     115
## 2830      President          9  FALSE    FALSE        177      82
## 1531         George          6  FALSE    FALSE         92      65
## 3920             W.          2  FALSE     TRUE         78      59
## 3937            war          3  FALSE    FALSE        155      54
## 80   administration         14  FALSE    FALSE         67      39
## 1712          House          5  FALSE    FALSE         78      44
## 2310           memo          4  FALSE    FALSE         49      13
## 793        Congress          8  FALSE    FALSE         53      26
## 3983          White          5  FALSE    FALSE         52      29
##      reldocfreq    tfidf termfreq.y relfreq.x relfreq.y   over
## 541      1.0000 0.000000          0  0.013227 0.0000000 13.227
## 2830     0.7130 0.005675         75  0.007528 0.0008940  3.975
## 1531     0.5652 0.007598         14  0.003913 0.0001669  3.353
## 3920     0.5130 0.008545          5  0.003317 0.0000596  3.131
## 3937     0.4696 0.013838        131  0.006592 0.0015615  2.574
## 80       0.3391 0.013215         14  0.002849 0.0001669  2.442
## 1712     0.3826 0.012347         44  0.003317 0.0005245  2.176
## 2310     0.1130 0.047050          9  0.002084 0.0001073  1.882
## 793      0.2261 0.023120         18  0.002254 0.0002146  1.856
## 3983     0.2522 0.016142         19  0.002212 0.0002265  1.803
```


This gives a list of the words that occur 'too much' in the articles mentioning Bush,
or in other words the collocates of the word 'Bush'.
To make this easier, and to also provide statistical association measures such as chi-squared,
the function `corpora.compare` is provided. 
The following example selects all words that are underrepresented in the `bush` corpus,
and sorts them by chi-squared:


```r
terms = corpora.compare(dtm.bush, dtm.rest)
terms = terms[terms$over < 1, ]
terms = terms[order(-terms$chi), ]
head(terms)
```

```
##         term termfreq.x termfreq.y relfreq.x relfreq.y   over   chi
## 3406 soldier         22        326 0.0009357  0.003886 0.3962 49.49
## 2052    kill         58        512 0.0024667  0.006103 0.4881 46.00
## 304   attack         45        415 0.0019138  0.004947 0.4900 39.61
## 362  Baghdad         69        532 0.0029345  0.006341 0.5360 38.31
## 2779  police          8        193 0.0003402  0.002300 0.4061 37.78
## 643   charge          7        148 0.0002977  0.001764 0.4695 27.41
```


What can be seen from these two word lists is that the articles mentioning Bush are more political in nature,
while the other articles describe more (military) action. 

Of course, this can also be used to compare e.g. vocabulary differences between newspapers, speakers, periods, etc.
For example, the following uses the article metadata to compare vocabulary after 2012 with the vocabulary before that date.


```r
dtm.before = dtm[meta$date < as.Date("2012-01-01"), ]
dtm.after = dtm[meta$date >= as.Date("2012-01-01"), ]
terms = corpora.compare(dtm.after, dtm.before)
terms = terms[order(-terms$chi), ]
head(terms[terms$over > 1, ])
```

```
##        term termfreq.x termfreq.y relfreq.x relfreq.y  over    chi
## 3812  Islam        230          4  0.003708 8.813e-05 4.327 158.02
## 7006  Sunni        196          0  0.003160 0.000e+00 4.160 143.70
## 7759 weapon        138         19  0.002225 4.186e-04 2.273  58.60
## 7422 Turkey         83          3  0.001338 6.610e-05 2.193  53.02
## 7         %        142         27  0.002290 5.949e-04 2.063  47.91
## 5287  Party        109         16  0.001757 3.525e-04 2.039  44.50
```

```r
head(terms[terms$over < 1, ])
```

```
##            term termfreq.x termfreq.y relfreq.x relfreq.y   over   chi
## 6747    soldier        127        221 2.048e-03 0.0048691 0.5193 64.60
## 2836       Ford          1         40 1.612e-05 0.0008813 0.5401 51.41
## 5851       rape          2         36 3.225e-05 0.0007932 0.5757 42.91
## 4843     murder         24         68 3.870e-04 0.0014982 0.5552 37.82
## 3925 journalist         57        108 9.190e-04 0.0023795 0.5679 36.44
## 3604   incident         41         88 6.611e-04 0.0019388 0.5652 35.67
```


So, the later articles mention the scandals with Abu Ghraib and phosphorus munition more frequently, 
while the earlier articles metion the constitution and refugees. 
