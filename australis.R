library(Rfacebook)
token <- 'EAACEdEose0cBAB2RmoAvleZCHeiyvLrqFW2ct930oiqMEDvC666tSvszlI6rRAAjLFsxZAc5TG1gRGPxMbfTb6FDZA5LnnHcggMzfqF3MqRJ2hMgTUwtbKKzea9z84JyCQywdPpDI0ZB7DfONo5RZCs4fhUoblGqq3107aqe4HgZDZD'

#getting posts
australis_raw <- getPage(183675737042, token, n=5000, reactions = T)
id <- australis_raw$id[1]


#getting comments
com <- list()
for (i in 1:length(australis_raw$id)){
  com[[i]] <- getPost(post=australis_raw$id[i], token, comments = TRUE, likes = FALSE)
}

#merging australis and com
for(i in 1:length(com)){
  australis_raw$comments[i] <- paste(com[[i]]$comments$message,collapse=" ")
}
#loading the file into a data frame
#the file is saved in th R working directory (can be found using getwd())

australis_raw = read.csv("australis_raw.csv")
#creating a subse and removing the first 10 rows that contain 'life events'
australis_raw <- subset(australis_raw[11:2054,])

library(lubridate)
#changing time format
australis_raw$created_time <- as.POSIXct(australis_raw$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz ="GMT")

#changing the time zone 
attr(australis_raw$created_time, "tzone") <- "Australia/Sydney"

write.csv(australis_raw, "australis.csv")


australis = read.csv("australis.csv")
#View(australis)
library(tidytext)
library(dplyr)

#changing message and comment columns from factor ro chatracter type
australis$message <- as.character(australis$message)
australis$comments <- as.character(australis$comments)

post <- australis$message
comment <- australis$comments

#transforming to data frames
post_df <- data_frame(line = 11:2054,  text = post)
comment_df <- data_frame(line = 11:2054,  text = comment)

#breaking up text into individual tokens - single words
posts_un <- post_df %>%
  unnest_tokens(word, text)
comments_un <- comment_df %>%
  unnest_tokens(word, text)

#removing stop words
posts1 <- posts_un[,2]
comments1 <- comments_un[,2]
stopwords <- as.vector(stop_words[,1])
cleanpost <- anti_join(posts1, stopwords, by = "word")
cleancom <- anti_join(comments1, stopwords, by = "word")

#cleaning expressions 
cleanpost2 <- subset(cleanpost, cleanpost$word!="http" & cleanpost$word!="bit.ly")
cleancom2 <- subset(cleancom, cleancom$word!="http" & cleancom$word!="bit.ly")

#removing digits
posts <- gsub('[[:digit:]]+', '', cleanpost2$word)
comments <- gsub('[[:digit:]]+', '', cleancom2$word)

posts <- as.data.frame(posts)
comments <- as.data.frame(comments)

library(tm)

#using posts to create document term matrix 
corppst <- Corpus(VectorSource(posts))
dtmpst <- DocumentTermMatrix(corppst)

#finding most frequently occuring words
freqpst <- sort(colSums(as.matrix(dtmpst)), decreasing=TRUE)   
freqpst_df <- as.data.frame(freqpst)

#using comments to create document term matrix 
corpc <- Corpus(VectorSource(comments))
dtmc <- DocumentTermMatrix(corpc)

#finding most frequently occuring words
freqc <- sort(colSums(as.matrix(dtmc)), decreasing=TRUE)   
freqc_df <- as.data.frame(freqc)

smallfreqpost_df <- as.data.frame(head(freqpst, 20))

smallfreqc_df <- as.data.frame(head(freqc, 20))

library(data.table)

freqpst_tb <- setDT(smallfreqpost_df, keep.rownames = TRUE)[]
colnames(freqpst_tb) <- c("word", "frq")

freqc_tb <- setDT(smallfreqc_df, keep.rownames = TRUE)[]
colnames(freqc_tb) <- c("word", "frq")

library(ggplot2)

#word column ordered as in data frame
freqpst_tb$word <- factor(freqpst_tb$word, levels=unique(as.character(freqpst_tb$word)))
freqplot <- 
  ggplot(freqpst_tb, aes(freqpst_tb$word, freqpst_tb$frq)) +
  geom_bar(width = .7, fill="salmon3", stat = "identity") +
  xlab(NULL) +
  ylab("frequency") +
  theme_light() 
freqplot

#word column ordered as in data frame
freqc_tb$word <- factor(freqc_tb$word, levels=unique(as.character(freqc_tb$word)))
freqplot2 <- 
  ggplot(freqc_tb, aes(freqc_tb$word, freqc_tb$frq)) +
  geom_bar(width = .7, fill="plum2", stat = "identity") +
  xlab(NULL) +
  ylab("frequency") +
  theme_light()
freqplot2

##################################################################################sentiment##########
library(tidyr)

posts_s <- post_df %>%
  unnest_tokens(word, text)

#comments that have no positive/negativ sentiment are omitted, sum of sentiment in post
postssentiment <- posts_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


ggplot(postssentiment, aes(index, sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "blueviolet") +
  theme_bw()

#comments sentiment bing
comments_s <- comment_df %>%
  unnest_tokens(word, text)

#comments that have no positive/negativ sentiment are omitted, sum of sentiment in post
comsentiment <- comments_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#all comments in one line per one post
ggplot(comsentiment, aes(index, sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "dodgerblue3") +
  theme_bw()

################################## compare 3 lexicons
afinn <- posts_s %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(posts_s %>% 
                            inner_join(get_sentiments("bing"), by = "word") %>%
                            mutate(method = "Bing et al."),
                          posts_s %>% 
                            inner_join(get_sentiments("nrc"), by = "word") %>% 
                            mutate(method = "NRC")) %>%
  count(method, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

############################################################################################
#most positive /negative words

postscount <- posts_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()



postscount %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

comcount <- comments_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()



comcount %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

###############################wordcloud

library(wordcloud)



freqpst_df <- setDT(freqpst_df, keep.rownames = TRUE)[]
colnames(freqpst_df) <- c("word", "frq")

freqc_df <- setDT(freqc_df, keep.rownames = TRUE)[]
colnames(freqc_df) <- c("word", "frq")


freqpst_df %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 70, scale=c(1,.4), min.freq=100, random.order = FALSE))

freqc_df %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 70, scale=c(1,.4), min.freq=500, random.order = FALSE))
?wordcloud

library(reshape2)

posts_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 80)


comments_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 80)
####################################################################

com_bigrams <- comment_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

com_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- com_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_separated %>%
  filter(word1 == "australis") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

aus_words <- bigrams_separated %>%
  filter(word1 == "australis") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

aus_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(30) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Words preceded by \"australis\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

########################################################
#contribution to sentiment posts
contributions <- posts_s %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))

contributions %>%
  top_n(30, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip()

#comments

contributions2 <- comments_s %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution2 = sum(score))

contributions2 %>%
  top_n(30, abs(contribution2)) %>%
  mutate(word = reorder(word, contribution2)) %>%
  ggplot(aes(word, contribution2, fill = contribution2 > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip()

################################################3

nrcanger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

posts_anger <- posts_s %>%
  inner_join((nrcanger), by = "word") %>%
  group_by(index = line) %>% 
  count(word, sort = TRUE)

posts_anger[posts_anger$index>=1780,]


library(ggplot2)

ggplot(posts_anger, aes(x=posts_anger$index, y=posts_anger$n)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "anger count") +
  theme_bw()

australis_reactions <- australis[1780:2044,]

ggplot(australis_reactions, aes(x=australis_reactions$created_time, y=australis_reactions$angry_count)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "anger count") +
  theme_bw()

######

nrcsadness <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")

posts_sadness <- posts_s %>%
  inner_join((nrcsadness), by = "word") %>%
  group_by(index = line) %>% 
  count(word, sort = TRUE)

posts_sadness[posts_sadness$index>=1780,]


ggplot(posts_sadness, aes(x=posts_sadness$index, y=posts_sadness$n)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "sadness count") +
  theme_bw()


ggplot(australis_reactions, aes(x=australis_reactions$created_time, y=australis_reactions$sad_count)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "sadness count") +
  theme_bw()

#####
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

posts_joy <- posts_s %>%
  inner_join((nrcjoy), by = "word") %>%
  group_by(index = line) %>% 
  count(word, sort = TRUE)

posts_joy[posts_joy$index>=1780,]


ggplot(posts_joy, aes(x=posts_joy$index, y=posts_joy$n)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "sadness count") +
  theme_bw()

ggplot(australis_reactions, aes(x=australis_reactions$created_time, y=australis_reactions$love_count)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "love count") +
  theme_bw()

##############wow

nrcant <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

posts_ant <- posts_s %>%
  inner_join((nrcant), by = "word") %>%
  group_by(index = line) %>% 
  count(word, sort = TRUE)

posts_ant[posts_ant$index>=1780,]


ggplot(posts_ant, aes(x=posts_ant$index, y=posts_ant$n)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "sadness count") +
  theme_bw()

ggplot(australis_reactions, aes(x=australis_reactions$created_time, y=australis_reactions$wow_count)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "wow count") +
  theme_bw()

#no haha no nrc emotions really matched