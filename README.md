Deprecation warning!!
============

This code for corpustools will no longer be improved, because we've created a new (and better!) version of corpustools. 

The new corpustools is available on https://github.com/kasperwelbers/corpustools (without the hyphen between "corpus" and "tools", which was silly anyway). This page (corpus-tools, with the hyphen) will still be available for current users, and I will respond to issues, as long as they're not feature requests (for which you really should use the new corpustool).

Corpus Tools
============

This package offers various functions to make bag-of-words approaches to text analysis more user-friendly.
The central concept in this module is the Document-Terms matrix from the `tm` package.

Installation
----

You can install directly from github:

```{r}
library(devtools)
install_github("kasperwelbers/corpus-tools")
```

Usage
-----

Please see the following how-to documents:

* [Creating a Document Term Matrix](howto/dtm.md)
* [Frequency analysis](howto/frequency.md)
* [Corpus comparison](howto/compare.md)
* [LDA topic modeling](howto/lda.md)
