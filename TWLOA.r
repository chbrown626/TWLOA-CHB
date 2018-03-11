
#Loading the required libraries
library(xlsx)
library(tm)
library(readr)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(proxy)
library(dplyr)
library(lsa)
library(LSAfun)
library(stylo)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

#Load the dataset
twloa_full <- read.xlsx("D:/Users/Christianna Brown/Desktop/MSDS 692/Data Files/TWLOA All - Masked.xlsx", 1, header = T)
twloa_full <- twloa_full[, -c(12:25)]
attach(twloa_full)

#Data overview
head(twloa_full)
str(twloa_full)

#EDA
twloa_EDA <- twloa_full[, c(1, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15)]
pairs(twloa_EDA)

twloa_city <- within(twloa_EDA, City <- factor(City, levels = names(sort(table(City), decreasing = TRUE)[1:10])))
ggplot(subset(twloa_city, !is.na(City)), aes(City, fill=City)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("City") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_state <- within(twloa_EDA, State <- factor(State, levels = names(sort(table(State), decreasing = TRUE)[1:10])))
ggplot(subset(twloa_state, !is.na(State)), aes(State, fill=State)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("State") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_class <- within(twloa_EDA, Class <- factor(Class, levels = names(sort(table(Class), decreasing = TRUE)[1:5])))
ggplot(subset(twloa_class, !is.na(Class)), aes(Class, fill=Class)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Class Level") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_college <- within(twloa_EDA, College <- factor(College, levels = names(sort(table(College), decreasing = TRUE))))
ggplot(twloa_college, aes(College, fill=College)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("College") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_period <- within(twloa_EDA, Period <- factor(Period, levels = names(sort(table(Period), decreasing = TRUE))))
ggplot(twloa_period, aes(Period, fill=Period)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Leave Period") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_wd <- within(twloa_EDA, Withdrawal <- factor(Withdrawal, levels = names(sort(table(Withdrawal), decreasing = TRUE))))
ggplot(twloa_wd, aes(Withdrawal, fill=Withdrawal)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Current Withdrawal") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_time <- within(twloa_EDA, Duration <- factor(Duration, levels = names(sort(table(Duration), decreasing = TRUE)[1:10])))
ggplot(subset(twloa_time, !is.na(Duration)), aes(Duration, fill=Duration)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Completion Time (in seconds)") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_browser <- within(twloa_EDA, Browser <- factor(Browser, levels = names(sort(table(Browser), decreasing = TRUE))))
ggplot(twloa_browser, aes(Browser, fill=Browser)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("Web Browser") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

twloa_os <- within(twloa_EDA, OS <- factor(OS, levels = names(sort(table(OS), decreasing = TRUE))))
ggplot(twloa_os, aes(OS, fill=OS)) + geom_bar(position="dodge") + guides(fill=FALSE) + ylab("Count") + xlab("OS Used") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Text analytics
twloa_text <- twloa_full[, c(1, 12)]
twloa_corpus <- VCorpus(VectorSource(twloa_text$Text))
twloa_corpus

twloa_corpus <- tm_map(twloa_corpus, removePunctuation)
twloa_corpus <- tm_map(twloa_corpus, removeNumbers)
twloa_corpus <- tm_map(twloa_corpus, content_transformer(tolower))
twloa_corpus <- tm_map(twloa_corpus, removeWords, stopwords("english"))
twloa_corpus <- tm_map(twloa_corpus, removeWords, c("regis", "need", "change", "person", "time", "direct", "longer", "fit", "feel", "student", "didnt", "felt"))
twloa_corpus <- tm_map(twloa_corpus, stripWhitespace)
twloa_corpus <- tm_map(twloa_corpus, stemDocument)

#Inspect words used at least 100 times and at least 300 times
#Creating a Document-Term Matrix
twloa_dtm <- DocumentTermMatrix(twloa_corpus)
inspect(twloa_dtm)

#Finding terms used at least 100 times and 300 times
findFreqTerms(twloa_dtm, 100)
findFreqTerms(twloa_dtm, 300)

#Creating a frequency table for the wordclouds
twloa_freq <- colSums(as.matrix(twloa_dtm))
ord <- order(twloa_freq, decreasing = T)

#Listing the most and least frequent terms
twloa_freq[head(ord)]
twloa_freq[tail(ord)]

#Plotting the words used at least 100 and 300 times
twloa_wf <- data.frame(word = names(twloa_freq), freq = twloa_freq)
head(twloa_wf)
p <- ggplot(subset(twloa_wf, freq>100), aes(x = reorder(word, -freq), y = freq)) + xlab("Frequently Used Words") + ylab("Frequency") + geom_bar(stat = "identity", color = "darkmagenta", fill = "darkmagenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
p <- ggplot(subset(twloa_wf, freq>300), aes(x = reorder(word, -freq), y = freq)) + xlab("Frequently Used Words") + ylab("Frequency") + geom_bar(stat = "identity", color = "darkslategray2", fill = "darkslategray2") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

#Word Clouds
#Plotting words that occur at least 100 times
set.seed(1234)
wordcloud(names(twloa_freq), twloa_freq, min.freq = 100, scale = c(5, 0.1), colors = brewer.pal(9, "Set1"))

#Plotting words that occur at least 300 times
set.seed(1234)
wordcloud(names(twloa_freq), twloa_freq, min.freq = 300, scale = c(5, 0.1), colors = brewer.pal(10, "Paired"))

#Applying TF-IDF, LSA, and cosine distance to specific terms in the dataset
#Reviewing the term 'financi'
#TF-IDF
twloa_dtm <- DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F)))
findAssocs(twloa_dtm, "financi", 0.5)
inspect(DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F), dictionary = c("financi"))))

#LSA
twloa_tdm <- TermDocumentMatrix(twloa_corpus)
twloa_lsa <- lsa(twloa_tdm, dims = dimcalc_share())
twloa_matrix <- as.textmatrix(twloa_lsa)
as.textmatrix(twloa_lsa)
associate(twloa_matrix, "financi", threshold = 0.5)

#Reviewing the term 'health'
#TF-IDF
twloa_dtm <- DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F)))
findAssocs(twloa_dtm, "health", 0.5)
inspect(DocumentTermMatrix(twloa_corpus, control = list(weighting = weightTfIdf(twloa_dtm, normalize = F), dictionary = c("health"))))

#LSA
twloa_tdm <- TermDocumentMatrix(twloa_corpus)
twloa_lsa <- lsa(twloa_tdm, dims = dimcalc_share())
twloa_matrix <- as.textmatrix(twloa_lsa)
as.textmatrix(twloa_lsa)
associate(twloa_matrix, "health", threshold = 0.5)

#Looking at how 'academ' and 'financi' are related with Cosine
Cosine("academ", "financi", tvectors = twloa_matrix)

#Zipf's & Heaps Plots of the Term-Document Matrix
Zipf_plot(twloa_tdm, col = c("deeppink"))
Heaps_plot(twloa_tdm, col = c("deepskyblue"))

#Sentiment analysis
twloa_txt <- twloa_full[, c(12:12)]
twloa_txt <- trimws(twloa_txt)
twloa_txt <- gsub("\\$", "",twloa_txt)
twloa_tokens <- data_frame(text = twloa_txt) %>% unnest_tokens(word, text)

bing_sent <- twloa_tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words
bing_sent

nrc_sent <- twloa_tokens %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)
nrc_sent

bing_word_counts <- twloa_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill = sentiment)) +
          geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment", x = NULL) +
          coord_flip()

nrc_word_counts <- twloa_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill = sentiment)) +
          geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Contribution to sentiment", x = NULL) +
          coord_flip()
