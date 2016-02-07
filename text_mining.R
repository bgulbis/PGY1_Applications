# text_mining.R
# 
# perform text mining on references

library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(sentiment)
library(openNLP)
library(koRpus)

# use ref.assess
if (!exists("ref.assess")) {
    ref.assess <- readRDS("ref.assess.Rds")
}

# tm----
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

# wordcloud----
words <- names(freq)
wordcloud(words[1:50], freq[1:50])

# find frequent terms
findFreqTerms(dtm.common, 5)
findAssocs(dtm.common, "will", 0.7)

# sentiment----
# classify emotion and polarity
class_emo <- classify_emotion(ref.assess$comment)
class_pol <- classify_polarity(ref.assess$comment)

test <- data.frame(emotion = class_emo[, 7], polarity = class_pol[, 4])
test2 <- bind_cols(ref.assess, test)

# openNLP----
# text <- ref.assess %>%
#     mutate(string.text = ifelse(!is.na(comment), as.String(comment), NA))


text2 <- ref.assess %>%
    filter(!is.na(comment)) 

str.text <- as.String(text2$comment)

word.ann <- Maxent_Word_Token_Annotator()
sent.ann <- Maxent_Sent_Token_Annotator()

my.text <- annotate(str.text, list(sent.ann, word.ann))

pos.ann <- Maxent_POS_Tag_Annotator()

my.text2 <- annotate(str.text, pos.ann, my.text)

# my.sent <- sents(my.text)

# koRpus----
write.table(text2$comment, file = file("rec_text.txt", "wb"), row.names = FALSE, col.names = FALSE)
tagged.txt <- treetag("rec_text.txt", treetagger = "C:/TreeTagger/bin/tag-english.bat", lang = "en")
