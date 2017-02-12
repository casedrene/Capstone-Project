#loading the libraries
library(Rfacebook)
library(lubridate)
library(ggplot2)
library(tidytext)
library(dplyr)
library(tm)
library(data.table)
library(tidyr)
library(reshape2)
library(wordcloud)
library(pROC)

token <- 'EAACEdEose0cBAB2RmoAvleZCHeiyvLrqFW2ct930oiqMEDvC666tSvszlI6rRAAjLFsxZAc5TG1gRGPxMbfTb6FDZA5LnnHcggMzfqF3MqRJ2hMgTUwtbKKzea9z84JyCQywdPpDI0ZB7DfONo5RZCs4fhUoblGqq3107aqe4HgZDZD'

#getting posts from Facebook using Page ID
australis_raw <- getPage(183675737042, token, n=5000, reactions = TRUE)
#setting 'id' column as the first column in the data frame
id <- australis_raw$id[1]

#getting comments as a separate data frame 
com <- list()
for (i in 1:length(australis_raw$id)){
  com[[i]] <- getPost(post=australis_raw$id[i], token, comments = TRUE, likes = FALSE)
}

#merging australis and com
for(i in 1:length(com)){
  australis_raw$comments[i] <- paste(com[[i]]$comments$message,collapse=" ")
}

#saving the original data set
australis_raw = write.csv("australis_raw.csv")

######################################################################################
#Data Cleanng

#loading the file into a data frame
#the file is saved in th R working directory (can be found using getwd())
australis_raw = read.csv("australis_raw.csv")

#creating a subset and removing the first 10 rows that contain 'life events'
australis_raw <- subset(australis_raw[11:2054,])

#changing time format
australis_raw$created_time <- as.POSIXct(australis_raw$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz ="GMT")

#changing the time zone 
attr(australis_raw$created_time, "tzone") <- "Australia/Sydney"

write.csv(australis_raw, "australis.csv")

######################################################################################
#Exploratory analysis

#loading the file into a data frame
australis = read.csv("australis.csv")

#creating new columns for day of the week and hour from created_time column
australis$weekday <- lubridate::wday(australis$created_time, label = TRUE)
australis$hour <- lubridate::hour(australis$created_time)

#plot showing likes count and created time 
australis$created_time <- as.Date(australis$created_time)
ggplot(australis, aes(x=australis$created_time, y=australis$likes_count)) +
  geom_point(color = "blue", alpha = 0.3) +
  labs(x = "created time", y = "likes count") +
  scale_x_date(date_labels = "%Y") +
  ggtitle("The distribution of likes since the creation of the page") +
  theme_bw()

#plot showing comments count and created time 
ggplot(australis, aes(x=australis$created_time, y=australis$comments_count)) +
  geom_point(color = "green", alpha = 0.3)  +
  labs(x = "created time", y = "comments count") +
  scale_x_date(date_labels = "%Y") +
  ggtitle("The distribution of comments since the creation of the page") +
  theme_bw()

#plot showing shares count and created time 
ggplot(australis, aes(x=australis$created_time, y=australis$shares_count)) +
  geom_point(color = "red", alpha = 0.3)  +
  labs(x = "created time", y = "shares count") +
  scale_x_date(date_labels = "%Y") +
  ggtitle("The distribution of shares since the creation of the page") +
  theme_bw()

#pie chart showing proportion of reactions
#sums of reaction counts for each reaction saved as separate variables
love <- sum(australis$love_count, na.rm=TRUE)
haha <- sum(australis$haha_count, na.rm=TRUE)
wow <- sum(australis$wow_count, na.rm=TRUE)
sad <- sum(australis$sad_count, na.rm=TRUE)
angry <- sum(australis$angry_count, na.rm=TRUE)

#creating a reactions data frame using the sums of reactions
df_reactions <- data.frame(
  reactions = c("love","haha","wow","sad","angry"),
  value = c(love, haha, wow, sad, angry)
)
#creating a pie chart 
bp_reactions <- ggplot(df_reactions, aes(x="", y=value, fill=reactions))+
  geom_bar(width = 1, stat = "identity")
pie <- bp_reactions + coord_polar("y", start=0)
pie <- pie + theme_minimal() + ggtitle("The distribution of reactions the page received") 
pie <- pie + xlab(NULL) + ylab(NULL)
pie

#base bar plot of post types
barplot(prop.table(table(australis$type)), col = c("palegreen2", "plum2", "skyblue2", "salmon2"), border = NA, space = 0.1, main = "Types of posts")

#likes per type of post
ggplot(australis, aes(x=australis$type, y=australis$likes_count)) +
  geom_point(color = "purple", alpha = 0.3) +
  labs(x = "type of post", y = "likes count")+
  ggtitle("Distribution of likes per type of post") +
  theme_bw()

#comments per type of post
ggplot(australis, aes(x=australis$type, y=australis$comments_count)) +
  geom_point(color = "brown", alpha = 0.3) +
  labs(x = "type of post", y = "comments count")+
  ggtitle("Distribution of comments per type of post") +
  theme_bw()

#shares per type of post
ggplot(australis, aes(x=australis$type, y=australis$shares_count)) +
  geom_point(color = "black", alpha = 0.3) +
  labs(x = "type of post", y = "shares count")+
  ggtitle("Distribution of shares per type of post") +
  theme_bw()

#dividing days into sections
time <- hm("00:00", "6:00", "12:00", "18:00", "23:59")
time <- as.Date(time, origin="1970-01-01")
day_breaks <- hour(time)
day_labels <- c("Night", "Morning", "Afternoon", "Evening")
day_sections <- cut(x=australis$hour, breaks=day_breaks, labels=day_labels, include.lowest=TRUE)

#bar plot showing the distribution of posts by day section
barplot(prop.table(table(day_sections)), col = c("sky blue", "pink", "green", "magenta"), border = NA, space = 0.1, main = "The time of creating the posts divided into day sections")

#bar plot showing the distribution of posts by day of the week
barplot(prop.table(table(australis$weekday)), col = c("aquamarine3", "antiquewhite3", "coral1", "steelblue1", "brown1", "darkseagreen3", "goldenrod2"),  border = NA, space = 0.1, main = "The time of creating the posts divided into days of the week")

#creating a vector consisting of sums of likes per day section
nightlikesvec <- grep("Night", day_sections, value = FALSE)
nightlikes <- sum(australis$likes_count[nightlikesvec])
morninglikesvec <- grep("Morning", day_sections, value = FALSE)
morninglikes <- sum(australis$likes_count[morninglikesvec])
afternoonlikesvec <- grep("Afternoon", day_sections, value = FALSE)
afternoonlikes <- sum(australis$likes_count[afternoonlikesvec])
eveninglikesvec <- grep("Evening", day_sections, value = FALSE)
eveninglikes <- sum(australis$likes_count[eveninglikesvec])

#creating a data frame 
df_dayseclikes <- data.frame(
  dayseclikes = c("night","morning","afternoon","evening"),
  value = c(nightlikes, morninglikes, afternoonlikes, eveninglikes)
)
#creating a pie chart
bp_dayseclikes <- ggplot(df_dayseclikes, aes(x="", y=value, fill=dayseclikes))+
  geom_bar(width = 1, stat = "identity")
pie2 <- bp_dayseclikes + coord_polar("y", start=0)
pie2 <- pie2 + theme_minimal() + ggtitle("Total number of likes per day section")
pie2 <- pie2 + scale_fill_discrete(name = "day section") 
pie2 <- pie2 + xlab(NULL) + ylab(NULL)
pie2

#creating a vector consisting of sums of comments per day section
nightcomvec <- grep("Night", day_sections, value = FALSE)
nightcom <- sum(australis$comments_count[nightcomvec])
morningcomvec <- grep("Morning", day_sections, value = FALSE)
morningcom <- sum(australis$comments_count[morningcomvec])
afternooncomvec <- grep("Afternoon", day_sections, value = FALSE)
afternooncom <- sum(australis$comments_count[afternooncomvec])
eveningcomvec <- grep("Evening", day_sections, value = FALSE)
eveningcom <- sum(australis$comments_count[eveningcomvec])

#creating a data frame
df_dayseccom <- data.frame(
  dayseccom = c("night","morning","afternoon","evening"),
  value = c(nightcom, morningcom, afternooncom, eveningcom)
)
#creating a pie chart
bp_dayseccom <- ggplot(df_dayseccom, aes(x="", y=value, fill=dayseccom))+
  geom_bar(width = 1, stat = "identity")
piecom <- bp_dayseccom + coord_polar("y", start=0)
piecom <- piecom + theme_minimal() + ggtitle("Total number of comments per day section")
piecom <- piecom + scale_fill_discrete(name = "day section")
piecom <- piecom + xlab(NULL) + ylab(NULL)
piecom

#creating a vector consisting of sums of shares per day section
nightshvec <- grep("Night", day_sections, value = FALSE)
nightsh <- sum(australis$shares_count[nightshvec])
morningshvec <- grep("Morning", day_sections, value = FALSE)
morningsh <- sum(australis$shares_count[morningshvec])
afternoonshvec <- grep("Afternoon", day_sections, value = FALSE)
afternoonsh <- sum(australis$shares_count[afternoonshvec])
eveningshvec <- grep("Evening", day_sections, value = FALSE)
eveningsh <- sum(australis$shares_count[eveningshvec])

#creating a data frame
df_daysecsh <- data.frame(
  daysecsh = c("night","morning","afternoon","evening"),
  value = c(nightsh, morningsh, afternoonsh, eveningsh)
)
#creating a pie chart
bp_daysecsh <- ggplot(df_daysecsh, aes(x="", y=value, fill=daysecsh))+
  geom_bar(width = 1, stat = "identity")
piesh <- bp_daysecsh + coord_polar("y", start=0)
piesh <- piesh + theme_minimal() + ggtitle("Total number of shares per day section")
piesh <- piesh + scale_fill_discrete(name = "day section")
piesh <- piesh + xlab(NULL) + ylab(NULL)
piesh

#creating a vector consisting of sums of likes per day weekday
monlikesvec <- grep("Mon", australis$weekday, value = FALSE)
monlikes <- sum(australis$likes_count[monlikesvec])
tuelikesvec <- grep("Tues", australis$weekday, value = FALSE)
tuelikes <- sum(australis$likes_count[tuelikesvec])
wedlikesvec <- grep("Wed", australis$weekday, value = FALSE)
wedlikes <- sum(australis$likes_count[wedlikesvec])
thulikesvec <- grep("Thurs", australis$weekday, value = FALSE)
thulikes <- sum(australis$likes_count[thulikesvec])
frilikesvec <- grep("Fri", australis$weekday, value = FALSE)
frilikes <- sum(australis$likes_count[frilikesvec])
satlikesvec <- grep("Sat", australis$weekday, value = FALSE)
satlikes <- sum(australis$likes_count[satlikesvec])
sunlikesvec <- grep("Sun", australis$weekday, value = FALSE)
sunlikes <- sum(australis$likes_count[sunlikesvec])

#creating a data frame
df_weekdaylikes <- data.frame(
  weekdaylikes = c("Monday","Tuesday","Wednesday","Thursday", "Friday", "Saturday", "Sunday"),
  value = c(monlikes, tuelikes, wedlikes, thulikes, frilikes, satlikes, sunlikes)
)
#creating a pie chart
bp_weekdaylikes <- ggplot(df_weekdaylikes, aes(x="", y=value, fill=weekdaylikes))+
  geom_bar(width = 1, stat = "identity")
pie3 <- bp_weekdaylikes + coord_polar("y", start=0)
pie3 <- pie3 + theme_minimal() + ggtitle("Total number of likes per weekday")
pie3 <- pie3 + scale_fill_discrete(name = "weekday")
pie3 <- pie3 + xlab(NULL) + ylab(NULL)
pie3

#creating a vector consisting of sums of comments per weekday
moncomvec <- grep("Mon", australis$weekday, value = FALSE)
moncom <- sum(australis$comments_count[moncomvec])
tuecomvec <- grep("Tues", australis$weekday, value = FALSE)
tuecom <- sum(australis$comments_count[tuecomvec])
wedcomvec <- grep("Wed", australis$weekday, value = FALSE)
wedcom <- sum(australis$comments_count[wedcomvec])
thucomvec <- grep("Thurs", australis$weekday, value = FALSE)
thucom <- sum(australis$comments_count[thucomvec])
fricomvec <- grep("Fri", australis$weekday, value = FALSE)
fricom <- sum(australis$comments_count[fricomvec])
satcomvec <- grep("Sat", australis$weekday, value = FALSE)
satcom <- sum(australis$comments_count[satcomvec])
suncomvec <- grep("Sun", australis$weekday, value = FALSE)
suncom <- sum(australis$comments_count[suncomvec])

#creating a data frame
df_weekdaycom <- data.frame(
  weekdaycom = c("Monday","Tuesday","Wednesday","Thursday", "Friday", "Saturday", "Sunday"),
  value = c(moncom, tuecom, wedcom, thucom, fricom, satcom, suncom)
)
#creating a pie chart
bp_weekdaycom <- ggplot(df_weekdaycom, aes(x="", y=value, fill=weekdaycom))+
  geom_bar(width = 1, stat = "identity")
pie4 <- bp_weekdaycom + coord_polar("y", start=0)
pie4 <- pie4 + theme_minimal() + ggtitle("Total number of comments per weekday")
pie4 <- pie4 + scale_fill_discrete(name = "weekday")
pie4 <- pie4 + xlab(NULL) + ylab(NULL)
pie4

#creating a vector consisting of sums of shares per weekday
monshvec <- grep("Mon", australis$weekday, value = FALSE)
monsh <- sum(australis$shares_count[monshvec])
tueshvec <- grep("Tues", australis$weekday, value = FALSE)
tuesh <- sum(australis$shares_count[tueshvec])
wedshvec <- grep("Wed", australis$weekday, value = FALSE)
wedsh <- sum(australis$shares_count[wedshvec])
thushvec <- grep("Thurs", australis$weekday, value = FALSE)
thush <- sum(australis$shares_count[thushvec])
frishvec <- grep("Fri", australis$weekday, value = FALSE)
frish <- sum(australis$shares_count[frishvec])
satshvec <- grep("Sat", australis$weekday, value = FALSE)
satsh <- sum(australis$shares_count[satshvec])
sunshvec <- grep("Sun", australis$weekday, value = FALSE)
sunsh <- sum(australis$shares_count[sunshvec])

#creating a data frame
df_weekdaysh <- data.frame(
  weekdaysh = c("Monday","Tuesday","Wednesday","Thursday", "Friday", "Saturday", "Sunday"),
  value = c(monsh, tuesh, wedsh, thush, frish, satsh, sunsh)
)
#creating a pie chart
bp_weekdaysh <- ggplot(df_weekdaysh, aes(x="", y=value, fill=weekdaysh))+
  geom_bar(width = 1, stat = "identity")
pie5 <- bp_weekdaysh + coord_polar("y", start=0)
pie5 <- pie5 + theme_minimal() + ggtitle("The proportion of shares of posts posted per weekday")
pie5 <- pie5 + scale_fill_discrete(name = "weekday")
pie5 <- pie5 + xlab(NULL) + ylab(NULL)
pie5

######################################################################################
#Semantic analysis

#changing message and comment columns from factor to character type
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
cleanpost2 <- subset(cleanpost, cleanpost$word!="http" & cleanpost$word!="bit.ly" & cleanpost$word!="australis" & cleanpost$word!="online")
cleancom2 <- subset(cleancom, cleancom$word!="http" & cleancom$word!="bit.ly" & cleancom$word!="australis")

#removing digits
posts <- gsub('[[:digit:]]+', '', cleanpost2$word)
comments <- gsub('[[:digit:]]+', '', cleancom2$word)

#creating data frames
posts <- as.data.frame(posts)
comments <- as.data.frame(comments)

#using posts data frame to create document term matrix 
corppst <- Corpus(VectorSource(posts))
dtmpst <- DocumentTermMatrix(corppst)

#finding most frequently occuring words
freqpst <- sort(colSums(as.matrix(dtmpst)), decreasing=TRUE)   
freqpst_df <- as.data.frame(freqpst)

#using comments data frame to create document term matrix 
corpc <- Corpus(VectorSource(comments))
dtmc <- DocumentTermMatrix(corpc)

#finding most frequently occuring words
freqc <- sort(colSums(as.matrix(dtmc)), decreasing=TRUE)   
freqc_df <- as.data.frame(freqc)

#saving 20 words that appear most frequently in each data frame
smallfreqpost_df <- as.data.frame(head(freqpst, 20))
smallfreqc_df <- as.data.frame(head(freqc, 20))

#editing the data frames and changing column names
freqpst_tb <- setDT(smallfreqpost_df, keep.rownames = TRUE)[]
colnames(freqpst_tb) <- c("word", "frq")

freqc_tb <- setDT(smallfreqc_df, keep.rownames = TRUE)[]
colnames(freqc_tb) <- c("word", "frq")

#word column ordered as in data frame
freqpst_tb$word <- factor(freqpst_tb$word, levels=unique(as.character(freqpst_tb$word)))

#creating a plot for the most frequent words in posts data frame
freqplot <- 
  ggplot(freqpst_tb, aes(freqpst_tb$word, freqpst_tb$frq)) +
  geom_bar(width = .7, fill="salmon3", stat = "identity") +
  xlab(NULL) +
  ylab("frequency") +
  theme_light() 
freqplot

#word column ordered as in data frame
freqc_tb$word <- factor(freqc_tb$word, levels=unique(as.character(freqc_tb$word)))

#creating a plot for the most frequent words in comments data frame
freqplot2 <- 
  ggplot(freqc_tb, aes(freqc_tb$word, freqc_tb$frq)) +
  geom_bar(width = .7, fill="plum2", stat = "identity") +
  xlab(NULL) +
  ylab("frequency") +
  theme_light()
freqplot2

#creating a one-term-per-row table for posts
posts_s <- post_df %>%
  unnest_tokens(word, text)

#getting afinn sentiment
afinn <- posts_s %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

#creating a one-term-per-row table for comments
comments_s <- comment_df %>%
  unnest_tokens(word, text)

#getting afinn sentiment
afinn1 <- comments_s %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

#getting bing and nrc sentiment for posts
bing_and_nrc <- bind_rows(posts_s %>% 
                            inner_join(get_sentiments("bing"), by = "word") %>%
                            mutate(method = "Bing et al."),
                          posts_s %>% 
                            inner_join(get_sentiments("nrc"), by = "word") %>% 
                            mutate(method = "NRC")) %>%
  count(method, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#getting bing and nrc sentiment for comments
bing_and_nrc1 <- bind_rows(comments_s %>% 
                             inner_join(get_sentiments("bing"), by = "word") %>%
                             mutate(method = "Bing et al."),
                           comments_s %>% 
                             inner_join(get_sentiments("nrc"), by = "word") %>% 
                             mutate(method = "NRC")) %>%
  count(method, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#plotting the sentiments for posts
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#plotting the sentiments for comments
bind_rows(afinn1, 
          bing_and_nrc1) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

posts_s <- post_df %>%
  unnest_tokens(word, text)

#getting sentiment for posts using bing lexicon
postscount <- posts_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#plotting top 10 positive and negative words in posts that contribute most to sentiment
postscount %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  ggtitle("Words that contribute most to sentiment in posts")

comments_s <- comment_df %>%
  unnest_tokens(word, text)

#getting sentiment for comments using bing lexicon
comcount <- comments_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#plotting top 10 positive and negative words in comments that contribute most to sentiment
comcount %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  ggtitle("Words that contribute most to sentiment in comments")

#getting the sentiment of posts using bing lexicon, calculating frequency of each word and creating a word
#cloud
posts_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 80)

#getting the sentiment of comments using bing lexicon, calculating frequency of each word and creating a word
#cloud
comments_s %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 80)

#dividing comments into bigrams, counting the frequencies
com_bigrams <- comment_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

com_bigrams %>%
  count(bigram, sort = TRUE)

#removing stop words
bigrams_separated <- com_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#filtering bigrams that contain 'australis' as the first word
bigrams_separated %>%
  filter(word1 == "australis") %>%
  count(word1, word2, sort = TRUE)

#getting sentiment
AFINN <- get_sentiments("afinn")

aus_words <- bigrams_separated %>%
  filter(word1 == "australis") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

#plotting the results
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

#removing posts that do not have reactions and removing NAs
australis_reactions <- australis[1780:2044,]
australis_reactions$created_time <- as.Date(australis_reactions$created_time)

#getting sentiment for 'joy' emotion from nrc lexicon
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

#getting sentiment for comments
comments_joy <- comments_s %>%
  inner_join((nrcjoy), by = "word") %>%
  group_by(index = line) %>% 
  count(word, sort = TRUE)

#removing posts without reactions
comments_joy[comments_joy$index>=1780,]

#plotting the result
ggplot(comments_joy, aes(x=comments_joy$index, y=comments_joy$n)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "love count") +
  theme_bw() +
  ggtitle("Joy sentiment in the comments")

#the distribution of love reactions
ggplot(australis_reactions, aes(x=australis_reactions$created_time, y=australis_reactions$love_count)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%Y") +
  labs(x = "created time", y = "love count") +
  ggtitle("The distribution of love reactions") +
  theme_bw()

##############wow
#getting sentiment for 'anticipation' emotion from nrc lexicon
nrcant <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

#getting sentiment for comments
comments_ant <- comments_s %>%
  inner_join((nrcant), by = "word") %>%
  group_by(index = line) %>% 
  count(word, sort = TRUE)

#removing posts without reactions
comments_ant[comments_ant$index>=1780,]

#plotting the result
ggplot(comments_ant, aes(x=comments_ant$index, y=comments_ant$n)) +
  geom_bar(stat = "identity") +
  labs(x = "created time", y = "wow count") +
  ggtitle("Anticipation sentiment in the comments") +
  theme_bw()

#the distribution of wow reactions
ggplot(australis_reactions, aes(x=australis_reactions$created_time, y=australis_reactions$wow_count)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%Y") +
  labs(x = "created time", y = "wow count") +
  ggtitle("The distribution of wow reactions") +
  theme_bw()

##################################################################
# Prediction of engagement

#adding columns to data frame
comlen <- as.character(australis$comments)
comlen <- nchar(comlen)
australis$comlen <- paste(comlen)
australis$weekday <- lubridate::wday(australis$created_time, label = TRUE)
australis$weekday <- paste(australis$weekday)
australis$hour <- paste(australis$hour)

#adding sentiment using afinn lexicon
#splitting the data set into one-term-per-row
australis$comments <- as.character(australis$comments)
comments <- australis$comments
com_df <- data_frame(line = 11:2054,  text = post)
com_s <- com_df %>%
  unnest_tokens(word, text)

#getting the sentiment
afinn2 <- com_s %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

#creating a new data frame for sentiment and merging it with aistralis data frame
afinn_df2 = data.frame(matrix(NA, nrow=2044, ncol=2))
colnames(afinn_df2) <- c("index", "sentiment")
afinn_df2$index <- 11:2054
afinn_df2 <- merge(x=afinn2, y=afinn_df2,  by = "index", all = TRUE)
afinn_df2[is.na(afinn_df2)] <- 0 

afinn_df2 <- subset(afinn_df2, select = c("sentiment.x"))
afinn_char2 <- as.character(afinn_df2[,1])
australis$afinncom <- afinn_char2

#converting character variables to factor or numeric type
australis$weekday <- as.factor(australis$weekday)
australis$afincomm <- as.factor(australis$afinncom)
australis$comlen <- as.numeric(australis$comlen)
australis$hour <- as.numeric(australis$hour)

#creating a subset of the data frame
australis_train = subset(australis, select=c("id", "likes_count", "created_time","comments_count", "shares_count", "comlen", "weekday", "hour", "afinncom"))

#dividing the data set into training & test datasets
set.seed(666)
split <- sample(nrow(australis_train), floor(0.7*nrow(australis_train)))
train <- australis_train[split,]
test <- australis_train[-split,] 

library(randomForest)

#setting the seed
set.seed(300)

#predicting number of likes
fit <- randomForest(likes_count ~ comments_count + shares_count + comlen + afinncom +
                     weekday + hour,
                    data=train, 
                    importance=TRUE, 
                    mtry = 3,
                    ntree=1000)

#printing fit of the model and variable importance
print(fit)
importance(fit)

#plottig variable importance
varImpPlot(fit)

#plottig the error rate
plot(fit)

#testing the model and predicting the amount of likes
test$outcome <- predict(fit, test)

#calculating Area Under the Curve
ROC1 <- multiclass.roc(test$likes_count, test$outcome)

AUC1 <- auc(ROC1)
AUC1

