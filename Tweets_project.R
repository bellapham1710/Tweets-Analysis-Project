install.packages("fastR")
install.packages("lattice")
install.packages("mosaic")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("broom")
install.packages("tidytext")
install.packages("janeaustenr")
install.packages("stringr")
install.packages("Hmisc")
install.packages("fs")
install.packages("textdata")
install.packages("topicmodels")
install.packages("Rtools")
install.packages("installr")
install.packages("wordcloud")
install.packages("tm")
require(fastR)
require(lattice)
require(mosaic)
require(Hmisc)
require(dplyr)
require(tidyr)
require(ggplot2)
require(broom)
require(tidytext)
require(janeaustenr)
require(stringr)
require(Hmisc)
require(fs)
require(textdata)
require(topicmodels)
require(Rtools)
require(installr)
require(wordcloud)
require(tm)
library(fastR)
library(lattice)
library(mosaic)
library(Hmisc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(tidytext)
library(janeaustenr)
library(stringr)
library(Hmisc)
library(fs)
library(textdata)
library(topicmodels)
library(Rtools)
library(installr)
library(wordcloud)
library(tm)
#8
Pain_Var1_G3 = file.choose()
Pain_Var2_G3 = file(Pain_Var1_G3, open = "r")
Pain_Var2_G3
Pain_Var3_G3= readLines(Pain_Var2_G3)
length(Pain_Var3_G3)
Pain_df_G3 = data_frame(text = Pain_Var3_G3) %>%
  mutate(doc_numbe=row_number())
Pain_df_tidy_G3 = Pain_df_G3 %>% unnest_tokens(word,text)
Pain_df_tidy_G3
Pain_df_tidy_Nostopwords_G3 = Pain_df_tidy_G3 %>% 
  anti_join(stop_words)
Pain_df_tidy_Nostopwords_G3
Pain_df_tidy_Nostopwords_G3 %>%
  count(word, sort = TRUE) %>%
  filter(n > 50)
Pain_df_tidy_Nostopwords_G3 %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip() 
#9
Sentiment_G3 = Pain_df_tidy_G3 %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
Sentiment_G3
Sentiment_G3 %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  ungroup()%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(word,n, fill=sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Sentiment", x="Terms")+
  coord_flip()
#10
Pain_df_tidy_G3 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 100))

#11
corpus_G3 = Corpus(VectorSource(Pain_Var3_G3), readerControl=list(language="en"))
corpus_G3 = tm_map(corpus_G3, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus_G3 = tm_map(corpus_G3, removeWords, c("http","https"))
DTM_G3 <- DocumentTermMatrix(corpus_G3, control = list(stemming = FALSE, stopwords=TRUE, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE ))
rowTotal = apply(DTM_G3, 1, sum)
DTM_matrix_G3 = DTM_G3[rowTotal>0,] %>%
  as.matrix(DTM_G3)
dim(DTM_matrix_G3)
Pain_lda_G3 = LDA(DTM_matrix_G3, k = 10)
Pain_topics_G3 = tidy(Pain_lda_G3, matrix = "beta")
Pain_top_terms_G3 = Pain_topics_G3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
Pain_top_terms_G3 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
