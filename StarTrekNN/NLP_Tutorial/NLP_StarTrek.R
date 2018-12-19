# Natural Language Processing Tutorial
# https://www.kaggle.com/rtatman/nlp-in-r-topic-modelling

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

# load a list of hotel reviews, some of which are real, and some of which are fake
reviews <- read_csv("NLP_Tutorial/deceptive-opinion.csv")

# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
  {    
    # create a corpus (type of object expected by tm) and document term matrix
    Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
    DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
    
    # remove any empty rows in our document term matrix (if there are any 
    # we'll get an error when we try to run our LDA)
    unique_indexes <- unique(DTM$i) # get the index of each unique value
    DTM <- DTM[unique_indexes,] # get a subset of only those indexes
    
    # preform LDA & get the words/topic in a tidy text format
    lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
    topics <- tidy(lda, matrix = "beta")
    # get the top ten terms for each topic
    top_terms <- topics  %>% # take the topics data frame and..
      group_by(topic) %>% # treat each topic as a different group
      top_n(10, beta) %>% # get the top 10 most informative words
      ungroup() %>% # ungroup
      arrange(topic, -beta) # arrange words in descending informativeness
    
    # if the user asks for a plot (TRUE by default)
    if(plot == T){
      # plot the top ten terms for each topic in order
      top_terms %>% # take the top terms
        mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
        ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
        geom_col(show.legend = FALSE) + # as a bar plot
        facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
        labs(x = NULL, y = "Beta") + # no x label, change y label 
        coord_flip() # turn bars sideways
    }else{ 
      # if the user does not request a plot
      # return a list of sorted terms instead
      return(top_terms)
  }
}

# plot top ten terms in the hotel reviews by topic
top_terms_by_topic_LDA(reviews$text, number_of_topics = 2)
