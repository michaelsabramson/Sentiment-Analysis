# libraries
#install.packages("RedditExtractoR")
library(dplyr)
library(readr)
library(textreadr)
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(textdata)
library(reshape2)
library(wordcloud)
library(RedditExtractoR)
library(tm)
library(tidyr)
library(igraph)
library(ggraph)
library(plotly)
library(scales)

#setting working directory 
setwd("C:\\Users\\mabra\\Downloads")

#ammending stop words list
custom_stop_words <- tribble(~word, ~lexicon, 
                             "i'm", "CUSTOM",
                             "it's", "CUSTOM",
                             "https", "CUSTOM",
                             "pcpartpicker.com", "CUSTOM",
                             "NA", "CUSTOM",
                             "they're", "CUSTOM")
updated_stop_words <- stop_words %>%
  bind_rows(custom_stop_words)

#scraping and tokenizing intel comments from reddit
intel_df <- get_reddit(search_terms = "Intel", subreddit = "buildapc",
                            cn_threshold = 1, page_threshold = 10, sort_by = "comments",
                            wait_time = 2)

save(intel_df, file="intel_df.Rda")

intel_tokens <- intel_df %>%
  unnest_tokens(word, comment) %>%
  count(word, sort=TRUE) %>%
  anti_join(updated_stop_words)

#scraping and tokenizing AMD comments from reddit
amd_df <- get_reddit(search_terms = "AMD", subreddit = "buildapc",
                       cn_threshold = 1, page_threshold = 10, sort_by = "comments",
                       wait_time = 2)

save(amd_df, file="amd_df.Rda")

amd_tokens <- amd_df %>%
  unnest_tokens(word, comment) %>%
  count(word, sort=TRUE) %>%
  anti_join(updated_stop_words)

#create single token df for idf and sentiment analysis
combo_tokens <- bind_rows(mutate(intel_tokens,
                                     author="intel"),
                              mutate(amd_tokens,
                                     author="amd")) 

amd_tokens
intel_tokens
combo_tokens

#frequency by topic: intel or amd
combo_tokens_totals <- combo_tokens %>%
  group_by(author) %>%
  summarize(total=sum(n))

words_df <- left_join(combo_tokens, combo_tokens_totals)

ggplot(words_df, aes(n/total, fill = author))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~author, ncol=2, scales="free_y")

#idf dataframe
combo_idf <- combo_tokens %>%
  bind_tf_idf(word, author, n)

combo_idf <- combo_idf[order(combo_idf$tf),]
#graph idf

#frequency histogram
freq_hist <- combo_idf %>%
  filter(n >3000) %>% 
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)


idf_plot <- plot_ly(data=combo_idf,x=~tf_idf,y=~word,color=~author)
idf_plot

#lexicons
afinn <- get_sentiments('afinn')
bing <- get_sentiments('bing')
nrc <- get_sentiments('nrc')

#ggplot of bing sentiment for intel
intel_sentiment <- select(intel_df, id, comment) %>%
  unnest_tokens(word, comment) %>%
  anti_join(updated_stop_words) %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
  
intel_bing_sentiment <- intel_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Bing Sentiment Analysis of Intel", x =NULL) +
  coord_flip()

intel_bing_sentiment <- ggplotly(intel_bing_sentiment)
intel_bing_sentiment

#ggplot of bing sentiment for amd
amd_sentiment <- select(amd_df, id, comment) %>%
  unnest_tokens(word, comment) %>%
  anti_join(updated_stop_words) %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

amd_bing_sentiment <- amd_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Bing Sentiment Analysis of AMD", x =NULL) +
  coord_flip()

amd_bing_sentiment <- ggplotly(amd_bing_sentiment)
amd_bing_sentiment

#intel nrc sentiment dataframe for plotly
intel_sentiment_nrc <- select(intel_df, id, comment) %>%
  unnest_tokens(word, comment) %>%
  anti_join(updated_stop_words) %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

#distribution plot of sentiment for intel
plot_ly(intel_sentiment_nrc, x=~sentiment, y=~n, type="bar", color =~sentiment) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of sentiment for Intel")

#wordcloud for intel sentiment
intel_sentiment_nrc %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("grey20","grey80"),
                   title.colors=c("red","blue"),
                   random.order=FALSE,
                   max.words=5000, fixed.asp=TRUE,
                   scale=c(1,1), title.size =1, rot.per=.25)

#amd nrc sentiment dataframe for plotly
amd_sentiment_nrc <- select(amd_df, id, comment) %>%
  unnest_tokens(word, comment) %>%
  anti_join(updated_stop_words) %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

#distribution of sentiment for amd
plot_ly(amd_sentiment_nrc, x=~sentiment, y=~n, type="bar", color =~sentiment) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of sentiment for AMD")

#word cloud for amd
amd_sentiment_nrc %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("grey20","grey80"),
                   title.colors=c("red","blue"),
                   random.order=T,
                   max.words=3500, fixed.asp=TRUE,
                   scale=c(2,2), title.size =1, rot.per=.25)

#bigrams
intel_bigrams <- select(intel_df, id, comment) %>%
  unnest_tokens(bigram, comment, token = "ngrams", n=2) %>%
  separate(bigram,c("word1","word2"), sep= " ") %>%
  filter(!word1 %in% updated_stop_words$word) %>%
  filter(!word2 %in% updated_stop_words$word) %>%
  count(word1, word2, sort=TRUE)

intel_bigrams_plot <- intel_bigrams %>%
  filter(n>200) %>% 
  graph_from_data_frame()

ggraph(intel_bigrams_plot, layout="fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)

#AMD bigrams
amd_bigrams <- select(amd_df, id, comment) %>%
  unnest_tokens(bigram, comment, token = "ngrams", n=2) %>%
  separate(bigram,c("word1","word2"), sep= " ") %>%
  filter(!word1 %in% updated_stop_words$word) %>%
  filter(!word2 %in% updated_stop_words$word) %>%
  count(word1, word2, sort=TRUE)

amd_bigrams_plot <- amd_bigrams %>%
  filter(n>200) %>% 
  graph_from_data_frame()

ggraph(amd_bigrams_plot, layout="fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)



#correlograms
frequency <- bind_rows(mutate(intel_tokens, author="intel"),
                       mutate(amd_tokens, author= "amd"))%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `amd`)


ggplot(frequency, aes(x=proportion, y=`intel`, 
                      color = abs(`intel`- proportion)))+
  geom_abline(color="red4", lty=2)+
  geom_jitter(alpha=0.7, size=1, width=1, height=1)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Intel", x='AMD')


cor.test(data=frequency[frequency$author == "amd",],
         ~proportion + `intel`)

















