install.packages('fastR')
install.packages("magrittr")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("lattice")
install.packages("mosaic")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("broom")
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
library(fastR)
library(tidyverse)
library(magrittr)
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
library(wordcloud)
library(tm)
library(reshape2)

InputFile_Pain = file.choose()

openFile_Pain = file(InputFile_Pain, open="r")

Pain_G3 = readLines(InputFile_Pain)

Pain_df = data_frame(text = Pain_G3) %>% mutate(doc_number =row_number())
Pain_df

Pain_df_tidy = Pain_df %>% unnest_tokens(word,text)
Pain_df_tidy

Pain_df_tidy_Nostopwords= Pain_df_tidy %>% anti_join((stop_words))
Pain_df_tidy_Nostopwords

# Creating a bar chart to find words whose frequencies are more than 50. 

Pain_df_tidy_Nostopwords %>%
  count(word, sort= TRUE) %>%
  filter(n > 50) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip() 

# Creating two bar charts to show the frequency of top 10 positive and negative words tweets. 
  # The label for Y axis is “Sentiment” and the label for X axis is “Terms”.

bing_word_counts = Pain_df_tidy_Nostopwords %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup()%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(word,n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Sentiment", x="Terms")+
  coord_flip()

# Word cloud for 100 positive and negative words tweets

Pain_df_tidy_Nostopwords %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=100))

Pain_df_tidy_Nostopwords %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill=0) %>%
  comparison.cloud(colors=c("red", "green"), max.cloud=200)
