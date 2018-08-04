##### Code adapted with minor updates from "Declutter R Workflow w/ Tidy Tools
##### by Zev Ross, Hadley Wickham, and David Robinson
#PeerJ Preprints | https://doi.org/10.7287/peerj.preprints.3180v1 | CC BY 4.0 Open Access | rec: 23 Aug 2017, publ: 23 Aug 2017

setwd("D:/R Projects")

library(readr)

shakespeare <- read_csv("shakespeare.csv")


library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)

#make all words lowercase and group them, then combine counts for like words in like corpus
shakespeare <- shakespeare %>%            #we will be replacing shakespeare df
  mutate(word = str_to_lower(word)) %>%   #replace word values with lower case versions
  group_by(word, corpus) %>%                #group data by word, corpus
  summarize(word_count = sum(word_count)) #summarize combines groups into one row

#create new df that counts words across entire dataset, and how many corpuses they appear in
words <- shakespeare %>%
  group_by(word) %>%
  summarize(n = sum(word_count), corpusi = n_distinct(corpus)) %>%
  arrange(desc(n))

#remove common words like "the"
#anti-join is the opposite of a join, removes items with a match
words <- words %>%
  anti_join(tidytext::stop_words, by = "word") %>% #remove stopwords by comparing against list in tidytext package
  filter(corpusi <42, nchar(word) > 4) %>% #remove words that appear in over 42 corpusi and have fewer than 4 letters
  arrange(desc(n))


#joins the top 5 overall words to the main dataset
#semi-join is identical to an inner join 
# except it does not keep any columns from the right side table
#Then use spread to put the words into columns rather than rows
# for easier reading
shakespeare %>%
  semi_join(head(words, 5), by = "word") %>%
  tidyr::spread(word, word_count, fill = 0)


#Visualize the relationship between number of word occurrences across all works
# and the number of works they appear in
ggplot(words, aes(corpusi, n)) +
  geom_point()


#Improve the visual by log transform of y axis, using boxplots to summarize each unique x,
# and improving labels
ggplot(words, aes(corpusi, n)) +
  geom_boxplot(aes(group = corpusi)) +
  geom_smooth(se = FALSE) + 
  scale_y_log10() +
  labs(
    x = "Number of works",
    y = "Word count"
  )

#The above plot shows that many words are used only once.
#Use dplyr to list the words, sorted by string length
words %>%
  filter(n == 1) %>%
  arrange(desc(str_length(word)))


#Use TidyText to analyze in more depth
#So far the analysis shows a general sense of word counts. Some words seem to appear
# mostly in a single work/corpusi. 
#We can quantify this with Term Frequency-Inverse Document Frequency (TF-IDF).
#TF-IDF shows how important a word is to a particular document.

shakespeare_tf_idf <- shakespeare %>%
  bind_tf_idf(word, corpus, word_count) %>%
  arrange(desc(tf_idf))

shakespeare_tf_idf
#This provides term frequency (tf), which is the frequency within a work
#Inverse Document Frequency (idf) is the inverse of the frequency across works. 
#TF_IDF = TF * IDF

#High TF_IDF words tend to be unique to and common within a particular document. 
#Use dplyr and ggplot2 to visualize the highest TF-IDF words within works.
top_tf_idf_words <- shakespeare_tf_idf %>%
  filter(corpus %in% c("macbeth", "hamlet", "romeoandjuliet", "othello")) %>%
  group_by(corpus) %>%
  top_n(12, tf_idf) %>%
  mutate(word = reorder(word, tf_idf))

ggplot(top_tf_idf_words, aes(word, tf_idf)) +
  geom_bar(stat = "identity") + 
    coord_flip() +
  facet_wrap(~ corpus, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#We can also analyze sentiment using datasets provided within TidyText
#The sentiments dataset includes word sentiment scores and categorizes in three lexicons.
#We will use Finn Arup Nielsen (AFINN) lexicon to measure sentiment.
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, score) %>%
  arrange(desc(score))

AFINN
#Shows the most positive words in the lexicon

#Join sentiment scores to Shakespeare data to see most influential words on sentiment
shakespeare_sentiment <- shakespeare %>%
  inner_join(AFINN, by = "word")

shakespeare_sentiment %>%
  group_by(word) %>%
  summarize(impact = sum(word_count * score)) %>%
  top_n(25, abs(impact)) %>%
  mutate(word = reorder(word, impact)) %>%
  ggplot(aes(word, impact, fill = impact > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  ylab("Impact on sentiment scores in Shakespeare")


