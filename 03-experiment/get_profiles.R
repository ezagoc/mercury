rm(list = ls())

install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
library(tidyverse)

token <- 'AAAAAAAAAAAAAAAAAAAAAPdlnQEAAAAA1SQTuz2Q9mmQ70HKkq5iFSwMkLQ%3DZbAZ7JGYfT5G2j8zqyqJKgLdLsDd'


Sys.setenv(BEARER_TOKEN = token)

bearer_token <- Sys.getenv("$BEARER_TOKEN")
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

handle <- 'ZagoZaguinho'
url_handle <- sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)

 params <- list(`user.fields` = 'location')

response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers),
            query = params)
obj <- httr::content(response, as = "text")
print(obj)

