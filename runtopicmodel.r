# 'command line' script to run a topic model
# Usage: Rscript runtopicmodel.r /path/to/dtm.rdata /path/to/model.out.rdata K alpha
#
# dtm.rdata should be an R data file containing a dtm variable (e.g. created by save(dtm, file="dtm.rdata"))
# model.out will be an R data containing a variable m with the fitted model
# K and alpha are the topicmodels parameters

log <- function(...) message("[", date(), "] ", ...)
 
a = commandArgs(trailingOnly=T)
if (length(a) != 4) {
  stop("Usage: Rscript runtopicmodel.r /path/to/dtm.rdata /path/to/model.out.rdata K alpha")
}
 
infile = a[1]
outfile = a[2]
k = as.numeric(a[3])
alpha = as.numeric(a[4])
 
log("Loading dtm from ", infile)
# load infile and check dtm
load(infile)
if (!("dtm" %in% ls())) stop("Input file does not contain variable named 'dtm': ", infile)
if (!("DocumentTermMatrix" %in% class(dtm))) stop("Variable 'dtm' is not a document term matrix")
 
# check output file - better error now than run calculations first?
if (file.exists(outfile)) stop("Output file ",outfile, " already exists!")
if (!file.exists(dirname(outfile))) stop("Path does not exist: ", dirname(outfile))
 
# run topic model
library(corpustools)
dtm = weightTf(dtm)
log("Running topicmodels, K=", k, ", alpha=", alpha)
m = topmod.lda.fit(dtm, K=k, alpha=alpha)
 
log("Saving fitted model to ", outfile)
save(dtm, file=outfile)
