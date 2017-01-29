oz = read.csv("australis.csv")

library(lubridate)

oz$weekday <- wday(oz$created_time, label = TRUE)
oz$hour <- lubridate::hour(oz$created_time)

library(tidytext)
library(dplyr)

#adding columns to data frame
comlen <- as.character(oz$comments)
comlen <- nchar(comlen)
oz$comlen <- paste(comlen)
oz$weekday <- paste(oz$weekday)
oz$hour <- paste(oz$hour)

#adding sentiment using afinn lexicon
library(tidytext)
oz$message <- as.character(oz$message)
post <- oz$message
post_df <- data_frame(line = 11:2054,  text = post)
posts_s <- post_df %>%
  unnest_tokens(word, text)

afinn <- posts_s %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

afinn_df = data.frame(matrix(NA, nrow=2044, ncol=2))
colnames(afinn_df) <- c("index", "sentiment")
afinn_df$index <- 11:2054
afinn_df <- merge(x=afinn, y=afinn_df,  by = "index", all = TRUE)
afinn_df[is.na(afinn_df)] <- 0 

afinn_df <- subset(afinn_df, select = c("sentiment.x"))
afinn_char <- as.character(afinn_df[,1])
oz$afinn <- afinn_char

#creating a subset of the data frame
oz = subset(oz, select=c("id", "likes_count", "message","created_time","comments_count", "shares_count", "comments", "comlen", "weekday", "hour", "afinn"))

#splitting the data frame into train and test sets
train <- oz[1:1500,]
test <- oz[1501:2044,]

#train <- write.csv(train,"train.csv")
#test <- write.csv(test,"test.csv")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

library(randomForest)

set.seed(500)
#predicting number of likes
fit <- randomForest(likes_count ~ comments_count + shares_count + comlen + afinn +
                      weekday + hour,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(id = test$id, likes_count = Prediction)

print(fit)
importance(fit)
plot(fit)

#predicting number of comments
fit2 <- randomForest(comments_count ~ likes_count + shares_count + comlen + afinn +
                       weekday + hour,
                     data=train, 
                     importance=TRUE, 
                     ntree=2000)

print(fit2)
importance(fit2)
plot(fit2)

#predicting number of shares
fit3 <- randomForest(shares_count ~ likes_count + comments_count + comlen + afinn +
                       weekday + hour,
                     data=train, 
                     importance=TRUE, 
                     ntree=2000)

print(fit3)
importance(fit3)
plot(fit3)

#predicting sentiment in comments
fit4<- randomForest(afinn ~ likes_count + comments_count + comlen + shares_count +
                      weekday + hour,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

print(fit4)
importance(fit4)
plot(fit4)
