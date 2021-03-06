% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/corpus.r
\name{corpora.compare.list}
\alias{corpora.compare.list}
\title{The corpora.compare function for a list of dtm's}
\usage{
corpora.compare.list(x, subcorpus = NULL, method = "each_to_all",
  return.df = F, window.size = 3, .progress = "text", ...)
}
\arguments{
\item{x}{either a named list of document term matrices, or a single dtm (in which cast the subcorpus argument must be given)}

\item{subcorpus}{if x is a single dtm, subcorpus should be a vector of the length and order of the dtm rows. Each value of subcorpus then represents a separate corpus.}

\item{method}{different ways to compare corpora. "each_to_all" compares each corpus to a corpos consisting of all other corpora. "window" compares each corpus to a corpus consisting of the previous and/or next corpora (see window.size). The order will be determined by sorting the dtm names (or the subcorpus values) with the order function.}

\item{return.df}{logical. If True, the results are returned as a data.frame. Otherwise as a list.}

\item{window.size}{A vector of integers. If method is 'window', this determines which previous and next corpora are used to compare a corpus to. For example: if 3, then each corpus will be compared to a corpus consisting of the 3 previous and 3 next corpora. A corpus itself will not be used to create the comparison corpus. Note that the first and last [window.size] corpora cannot be compared to a full window.}

\item{...}{additional arguments to be passed to the corpora.compare function}
}
\value{
a list or data.frame with the corpora.compare results for each dtm.
}
\description{
The corpora.compare function for a list of dtm's
}

