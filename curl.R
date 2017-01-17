install.packages("RCurl")
install.packages("RJSONIO")
library(RCurl)
library(RJSONIO)
token <- 'EAACEdEose0cBAB2RmoAvleZCHeiyvLrqFW2ct930oiqMEDvC666tSvszlI6rRAAjLFsxZAc5TG1gRGPxMbfTb6FDZA5LnnHcggMzfqF3MqRJ2hMgTUwtbKKzea9z84JyCQywdPpDI0ZB7DfONo5RZCs4fhUoblGqq3107aqe4HgZDZD'
u <- paste("https://graph.facebook.com/", 183675737042, "/insights", "page_fans_country?", "&access_token=", token, sep = "")
df.list <- list()
json <- getURL(u)
json <- fromJSON(json, simplify = FALSE)
data <- json$data

df <- do.call("rbind", df.list)
return(df)
