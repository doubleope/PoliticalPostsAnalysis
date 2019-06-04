library(tm)
library(topicmodels)
library(gutenbergr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(purrr)
library(ggplot2)
library(dplyr)
library(SnowballC)
library(sqldf)
library(readr)





red <- read_csv("C:/Users/Ope Ope/Google Drive/School Documents/4331 Social Media/Project/Reddit Mining/PoliticalRedditsPosts.csv")


electionred <- red[which(red$created_utc >= 1478563200 & red$created_utc <= 1478649599),]

donelectionred <- electionred[which(electionred$subreddit =="The_Donald"),]

setwd("C:/Users/Ope Ope/Google Drive/School Documents/4331 Social Media/Project/Final Analysis/Topic Analysis/donald/dayof")


params <- list(minDocFreq = 1, removeNumbers = TRUE, stopwords = TRUE, stemming = TRUE, weighting = weightTf)




The_Donald <- donelectionred[(donelectionred$subreddit == "The_Donald"), "selftext"]
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste('\\b', stopwords_regex, '\\b')
The_Donald = stringr::str_replace_all(The_Donald, stopwords_regex, '')
The_Donald <- gsub('[[:punct:]]', "", The_Donald)
The_Donald <-gsub ('[[:cntrl:]]', "", The_Donald)
The_Donald <-gsub('\\d+', "", The_Donald)
The_Donald <- tolower(The_Donald)
The_Donald <- gsub("[^a-zA-Z0-9 ]","", The_Donald)
write.table(The_Donald, file = "The_Donald.txt", sep = "")


filenames <- list.files(getwd(), pattern = ".txt")


files <- lapply(filenames, readLines)

docs <- Corpus(VectorSource(files))


docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, stripWhitespace)

dtm <- DocumentTermMatrix(docs, control = params)

rownames(dtm) <- filenames

freq <- colSums(as.matrix(dtm))

length(freq)

ord <- order(freq, decreasing=TRUE)

freq[ord]

write.csv(freq[ord], "word_freq.csv")


burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003,5,63, 100001,765)
nstart <- 5
best <- TRUE

k <- 4


ldaOut <- LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))


ldaOut.terms <- as.matrix(terms(ldaOut,100))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicToTerms.csv"))

topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))




topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


topic2ToTopic3 <- lapply(1:nrow(dtm),function(x) 
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic1ToTopic3.csv"))

ap_topics <- tidy(ldaOut, matrix="beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  labs(x = NULL, y = "Beta") +
  coord_flip()

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))

write.csv(beta_spread, "beta_spread.csv")


