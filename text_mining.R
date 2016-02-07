# text_mining.R
# 
# perform text mining on references

library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(sentiment)
library(openNLP)

# use ref.assess
if (!exists("ref.assess")) {
    ref.assess <- readRDS("ref.assess.Rds")
}

# convert text into a corpus
vs <- VectorSource(ref.assess$comment)
corpus <- Corpus(vs)

# clean up text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# inspect(corpus[100])

# create document term matrix
# tdm <- TermDocumentMatrix(corpus)
dtm <- DocumentTermMatrix(corpus)
# dtm.dict <- DocumentTermMatrix(corpus, list(dictionary = c("improv", "benefi")))

# inspect(dtm)

dtm.common <- removeSparseTerms(dtm, 0.9)

dtm.dense <- as.matrix(dtm.common)
freq <- colSums(dtm.dense)
freq <- sort(freq, decreasing = TRUE)

# make wordcloud
words <- names(freq)
wordcloud(words[1:50], freq[1:50])

# find frequent terms
findFreqTerms(dtm.common, 5)
findAssocs(dtm.common, "will", 0.7)

# sentiment package
# classify emotion and polarity
class_emo <- classify_emotion(ref.assess$comment)
class_pol <- classify_polarity(ref.assess$comment)

test <- data.frame(emotion = class_emo[, 7], polarity = class_pol[, 4])
test2 <- bind_cols(ref.assess, test)

# openNLP
# text <- ref.assess %>%
    # mutate(string.text = as.String(comment))
text <- as.String(ref.assess$comment)

word.ann <- Maxent_Word_Token_Annotator()
sent.ann <- Maxent_Sent_Token_Annotator()
