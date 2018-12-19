# Markov Chain text generator in R
# https://gist.github.com/primaryobjects/4c7cca705eeba0d8bad6

library(markovchain)

setwd("~/GitHub/SATU/StarTrekNN")

text <- readLines('alloutput_edit.txt')
text <- text[nchar(text) > 300]

text <- gsub('.', ' .', text, fixed = TRUE)
text <- gsub(',', ' ,', text, fixed = TRUE)
text <- gsub('!', ' !', text, fixed = TRUE)
text <- gsub('(', '( ', text, fixed = TRUE)
text <- gsub(')', ' )', text, fixed = TRUE)
text <- gsub('/', '', text, fixed = TRUE)

terms <- unlist(strsplit(text, ' '))

fit <- markovchainFit(data = terms)

plot(fit$estimate)

paste(markovchainSequence(n=100, markovchain=fit$estimate), collapse=' ')
