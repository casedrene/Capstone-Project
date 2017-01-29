australis = read.csv("australis.csv")


#transforming the date and time format in australis$created_time
library(lubridate)

#australis$created_time <- as.POSIXct(australis$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz ="GMT")

#changing the time zone 
#time <- attr(australis$created_time, "tzone") <- "Australia/Sydney"

australis$weekday <- wday(australis$created_time, label = TRUE)
australis$month <- lubridate::month(australis$created_time, label = TRUE)
australis$hour <- lubridate::hour(australis$created_time)


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

#bar plot of post types
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
day_breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
day_labels <- c("Night", "Morning", "Afternoon", "Evening")
day_sections <- cut(x=australis$hour, breaks=day_breaks, labels=day_labels, include.lowest=TRUE)

#bar plot of posts by day section
barplot(prop.table(table(day_sections)), col = c("sky blue", "pink", "green", "magenta"), border = NA, space = 0.1, main = "The time of creating the posts divided into day sections")
#bar plot of posts by day of week
barplot(prop.table(table(australis$weekday)), col = c("aquamarine3", "antiquewhite3", "coral1", "steelblue1", "brown1", "darkseagreen3", "goldenrod2"),  border = NA, space = 0.1, main = "The time of creating the posts divided into days of the week")

#creatimg sum of likes per day section
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
pie2


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



