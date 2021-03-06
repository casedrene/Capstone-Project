Introduction
------------

Facebook is the most popular social network. With almost 2 billion monthly active users, it contains a large amount of data about users and their behavior.

### User Data

Demographic data that Facebook Page owners can accessed through Facebook Insights provides information such as gender, age and location. Demographic data of users who like a particular Facebook Page used to be available but are not available anymore. Therefore I was not able to access user data for this project.

### Page Data

Rfacebook provides and interface to the Facebook API and after getting the User Access Token from Graph API Explorer, Facebook data can be accessed.

    #loading Rfacebook library
    library(Rfacebook)
    token <- 'XXXXX'

    #getting a data frame containing all posts from Australis Cosmetics Facebook Page and reactions for each post
    australis <- getPage(183675737042, token, n=5000, reactions = T)
    #setting id column as the first column in the data frame
    id <- australis$id[1]

Page data includes a list of posts, likes, reactions, comments and shares.

Data Set
--------

Important information in the data set are the text of the post (message), time when the post was created (created\_time), type of the post (type), counts of likes, comments and shares (likes\_count, comments\_count and shares\_count) and counts of reactions (love\_count, haha\_count,wow\_count, sad\_count and angry\_count).

Another important part of the data set is the text of comments associated with every post. Comment data can be accessed using getPost function and using id column to match a list of comments with the correct post.

    #getting a list of comments for every post
    com <- list()
    for (i in 1:length(australis$id)){
      com[[i]] <- getPost(post=australis$id[i], token, comments = TRUE, likes = FALSE)
    }

The list of comments is later attached to the main data frame.

    #merging australis and com into one data frame
    for(i in 1:length(com)){
      australis$comments[i] <- paste(com[[i]]$comments$message,collapse=" ")
    }

### Limitations of the Data Set

As previously mentioned, the data set does not include any user information due to privacy. Other information that can be accessed through Facebook Insights include the device/platform the likes came from, page views, check-ins, the reach of posts, unlikes or how many page fans are online at any given time of the day. These information are only accessible to page admin and therefore are also not contained in the data set. As a result, this data set cannot answer any user or demographics-related questions and questions regarding the reach of posts or page views.

### Data Cleaning

The data set did not require any major transformations. The time format in the created\_time column, however, needed to be edited. Time zone has been changed to Australian Eastern Daylight Time and three other columns were created that will be used in analysis. These columns are weekday - containing days of the week then the post was posted, month - containing the months and hour - a column containing the hour of when the post was posted.

    library(lubridate)

    australis$created_time <- as.POSIXct(australis$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz ="GMT")

    #changing the time zone 
    time <- attr(australis_raw$created_time, "tzone") <- "Australia/Sydney"

    australis$weekday <- wday(australis$created_time, label = TRUE)
    australis$month <- lubridate::month(australis$created_time, label = TRUE)
    australis$hour <- lubridate::hour(australis$created_time)

Preliminary Exploration Analysis
--------------------------------

Preliminary exploratory analysis showed the distribution of likes, comments and shares over time.

``` r
australis = read.csv("australis.csv")


library(ggplot2)
#lokes/shares/comments count and created time 
australis$created_time <- as.Date(australis$created_time)
ggplot(australis, aes(x=australis$created_time, y=australis$likes_count)) +
  geom_point(color = "blue", alpha = 0.3) +
  labs(x = "created time", y = "likes count") +
  scale_x_date(date_labels = "%Y") +
  ggtitle("The distribution of likes since the creation of the page") +
  theme_bw()

ggplot(australis, aes(x=australis$created_time, y=australis$comments_count)) +
  geom_point(color = "green", alpha = 0.3)  +
  labs(x = "created time", y = "comments count") +
  scale_x_date(date_labels = "%Y") +
  ggtitle("The distribution of comments since the creation of the page") +
  theme_bw()

ggplot(australis, aes(x=australis$created_time, y=australis$shares_count)) +
  geom_point(color = "red", alpha = 0.3)  +
  labs(x = "created time", y = "shares count") +
  scale_x_date(date_labels = "%Y") +
  ggtitle("The distribution of shares since the creation of the page") +
  theme_bw()
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-1-1.png)![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-1-2.png)![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-1-3.png)

Pie chart showing all reactions show that 'love' and 'wow' reactions are most common.

``` r
#pie chart showing proportion of reactions
#sums of reaction counts
love <- sum(australis$love_count, na.rm=TRUE)
haha <- sum(australis$haha_count, na.rm=TRUE)
wow <- sum(australis$wow_count, na.rm=TRUE)
sad <- sum(australis$sad_count, na.rm=TRUE)
angry <- sum(australis$angry_count, na.rm=TRUE)

#creating a data frame
df_reactions <- data.frame(
  reactions = c("love","haha","wow","sad","angry"),
  value = c(love, haha, wow, sad, angry)
)
#creating a pie chart
bp_reactions <- ggplot(df_reactions, aes(x="", y=value, fill=reactions))+
  geom_bar(width = 1, stat = "identity")
pie <- bp_reactions + coord_polar("y", start=0)
pie <- pie + theme_minimal() + ggtitle("The distribution of reactions the page received") 
pie
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-2-1.png)

Most of the posts on the page include a photo and therefore the most common type of post is 'photo'.

``` r
barplot(prop.table(table(australis$type)), col = c("palegreen2", "plum2", "skyblue2", "salmon2"), border = NA, space = 0.1, main = "Types of posts")
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-3-1.png)

Consequently, 'photo' as a type of posts received the highest number of likes, comments and shares.

``` r
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
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-4-1.png)![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-4-2.png)![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-4-3.png)

Most posts were posted in the afternoon, smaller number of posts were posted in the morning and in the evening and almost none at night.

``` r
#dividing days into sections
australis = read.csv("australis.csv")
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
australis$hour <- lubridate::hour(australis$created_time)
day_breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
day_labels <- c("Night", "Morning", "Afternoon", "Evening")
day_sections <- cut(x=australis$hour, breaks=day_breaks, labels=day_labels, include.lowest=TRUE)

#bar plot of posts by day section
barplot(prop.table(table(day_sections)), col = c("sky blue", "pink", "green", "magenta"), border = NA, space = 0.1, main = "The time of creating the posts divided into day sections")
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-5-1.png)

The amount of posts during the week is evenly distributed with slightly higher number of posts on Friday, and the amount of posts posted during the weekend is significantly lower.

``` r
library(lubridate)
australis$weekday <- wday(australis$created_time, label = TRUE)
barplot(prop.table(table(australis$weekday)), col = c("aquamarine3", "antiquewhite3", "coral1", "steelblue1", "brown1", "darkseagreen3", "goldenrod2"),  border = NA, space = 0.1, main = "The time of creating the posts divided into days of the week")
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-6-1.png)

More than a half of the total number of likes belong to posts that were posted in the afternoon, mornings have the second highest amount.

``` r
australis = read.csv("australis.csv")
library(lubridate)

australis$hour <- lubridate::hour(australis$created_time)

#dividing days into sections
day_breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
day_labels <- c("Night", "Morning", "Afternoon", "Evening")
day_sections <- cut(x=australis$hour, breaks=day_breaks, labels=day_labels, include.lowest=TRUE)

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
library(ggplot2)
bp_dayseclikes <- ggplot(df_dayseclikes, aes(x="", y=value, fill=dayseclikes))+
  geom_bar(width = 1, stat = "identity")
pie2 <- bp_dayseclikes + coord_polar("y", start=0)
pie2 <- pie2 + theme_minimal() + ggtitle("Total number of likes per day section")
pie2 <- pie2 + scale_fill_discrete(name = "day section") 
pie2
```

<img src="Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-7-1.png" style="float:left" />

Most comments were also added in the afternoon.

``` r
australis = read.csv("australis.csv")

library(lubridate)

australis$hour <- lubridate::hour(australis$created_time)

#dividing days into sections
day_breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
day_labels <- c("Night", "Morning", "Afternoon", "Evening")
day_sections <- cut(x=australis$hour, breaks=day_breaks, labels=day_labels, include.lowest=TRUE)

#creatimg sum of comments per day section
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
piecom
```

<img src="Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-8-1.png" style="float:left" />

The total number of likes per weekday is evenly distributed and the amount of likes per post posted during the weekend are lower.

``` r
library(lubridate)

australis$weekday <- wday(australis$created_time, label = TRUE)

#creatimg sum of likes per day of week
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
pie3
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-9-1.png)

Total number of comments per weekday has similar distribution as the total number of likes per weekday.

``` r
#creatimg sum of comments per day of week
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
pie4
```

![](Capstone_project_milestone_report_files/figure-markdown_github/unnamed-chunk-10-1.png)
