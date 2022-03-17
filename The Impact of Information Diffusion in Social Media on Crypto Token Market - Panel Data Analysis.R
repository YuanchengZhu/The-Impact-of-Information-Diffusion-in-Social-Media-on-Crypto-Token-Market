# This is part of a research project on evaluating how information diffusion in social media can impact the crypto token market. We collect tweet data of 500+ tokens 
# that are listed in the secondary market and token-related trading data. In this R script, the main task is data cleaning, joining trading data with tweet data to create 
# a complete panel data for crypto tokens. After that, I use fixed effect model and instrumental variables to evaluate how tweet intensity can impact the trading 
# performance of crypto tokens.

# Notes: The data are private and will not be posted here. Further analyses like VAR model analysis and VAR model analysis with topic modeling are done in Stata and the 
# code is not included in this R script.



# Load packages

library(tidyverse)
library(skimr)
library(readxl)
library(data.table) #fwrite()
library(sentimentr)
library(lubridate)
library(plm)
library(zoo)
library(stargazer)



# Merge all Twitter data (Each excel file cotains tweet data of one token)

file_name_list = list.files(path = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/concatdata")
n <- length(file_name_list)
a <- paste("C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/concatdata/", file_name_list, sep = "")
# Generate ico campaign name to match index in the future
ico_name <- str_replace_all(file_name_list, ".xlsx", "")

merge.data <- read_excel(a[1])
merge.data <- mutate(merge.data, ico_name = ico_name[1], .before = 1)
for (i in 2:n){
  new.data <- read_excel(a[i])
  new.data <- mutate(new.data, ico_name = ico_name[i], .before = 1)
  merge.data = rbind(merge.data, new.data)
}

df <- merge.data
skim_without_charts(df)

# fwrite(df, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Merged Data.csv")



# Sentiment analysis of tweet content

# Slice data into different data frames 
# (Due to memory limitation of the laptop used for this analysis, I divide the data into 50 pieces to analyze the sentiment of tweet content and then combine them again.)
for (i in 1:50) {
  assign(paste("df", i, sep = ""), df[((i-1)*132835+1):(i*132835),])
}

# Sentiment analysis of tweet content
for (i in 1:50) {
  assign(paste("sentiment_df", i, sep = ""), df[((i-1)*132835+1):(i*132835),] %>% 
           select(Tweet_Text) %>% 
           get_sentences() %>% 
           sentiment_by())
}
# summary(sentiment_df2)

# Add sentiment columns to each data frame
for (i in 1:50) {
  assign(paste("df_with_sentiment", i, sep = ""), 
         get(paste("sentiment_df", i, sep = "")) %>% 
           select(c("word_count","sd", "ave_sentiment")) %>% 
           bind_cols(get(paste("df", i, sep ="")), .)) 
}

# Merge all data frames with sentiments into one
df_with_sentiment <- df_with_sentiment1
for (i in 2:50) {
  df_with_sentiment <- 
    bind_rows(df_with_sentiment, get(paste("df_with_sentiment", i ,sep ="")))
}

# fwrite(df_with_sentiment, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/Sentiment Data.csv")



# Add the index of ICO project to match the trading data afterwards

df_basic <- read.csv("C:/Users/zyc71/Desktop/icodata_full/Trading Data/basicinfo.csv")
df_basic <- df_basic %>% 
  select(c("index", "icoName")) %>% 
  distinct(., icoName, .keep_all = TRUE)

df_with_index <- left_join(df_with_sentiment, df_basic, by = c("ico_name" = "icoName"))

# fwrite(df_with_index, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Sentiment Data with Index.csv")



# Extract tweet creation date
df_date <- mutate(df_with_index, tweet_crea_date = as.Date(Tweet_Crea_At))
df_date <- rename_with(df_date, tolower)

# fwrite(df_date, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Sentiment Data with Index and Date.csv")



# Further cleaning

# Separate tweet type
df_date$if_retweet <- ifelse(str_detect(df_date$tweet_ref_tweets, "retweeted"), 1, 0)
df_date$if_reply <- ifelse(str_detect(df_date$tweet_ref_tweets, "replied_to"), 1, 0)
df_date$if_tweet <- ifelse(df_date$tweet_ref_tweets == "-", 1, 0)

# Choose language of English
df_clean <- df_date[df_date$tweet_lang == "en"]

# fwrite(df_clean, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Clean Data.csv")



# Eliminate Hashtag Problem

# Import a file to get twitter full handle and from which to get twitter handle as a form of "@ico_twitter_handle"
# Through the twitter handle data, I find that there are 5 with "/" show in the last character of the account, which results in NA in the simplified twitter handle.
# In addition, I find that there are 5 with "@" in the account. 
# However, both these two situations won't influence the result since they are not in the twitter data from the programmer.
df_tw_handle <- read_excel("C:/Users/zyc71/Desktop/icodata_full/Selected ICO Projects.xlsx")
df_tw_handle <- df_tw_handle %>% 
  rename("tw_full_handle" = "TwitterAccount-for collection") %>% 
  select(., "ICOName", "tw_full_handle") %>% 
  mutate(., tw_handle = str_extract(tw_full_handle,"[^/]+$")) %>% 
  # Generate two columns of keywords used in the data collection
  mutate(., kw1 = paste("@", tw_handle, sep = ""),
         kw2 = paste("#", ICOName, sep = ""))

df_clean <- left_join(df_clean, df_tw_handle, by = c("ico_name" = "ICOName"))

# Remove tweet contents that are searching result of only hashtag keyword
df_clean <- df_clean %>% 
  filter(!(str_detect(.$tweet_text, regex(.$kw2, ignore_case = TRUE)) & str_detect(.$tweet_text, regex(.$kw1, ignore_case = TRUE), negate = TRUE) &
             !((str_detect(.$tweet_text, regex(.$ico_name, ignore_case = TRUE)) & str_detect(.$tweet_text, regex("ICO", ignore_case = TRUE))) | 
                 (str_detect(.$tweet_text, regex(.$ico_name, ignore_case = TRUE)) & str_detect(.$tweet_text, regex("crypto", ignore_case = TRUE))))))

# fwrite(df_clean, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Without Hashtag Problem/Clean Data without Hashtag Problem.csv")




# Group sentiment score by ICO and date

df_group <- df_clean %>% 
  group_by(index, ico_name, tweet_crea_date) %>% 
  summarize(avg_sent = mean(ave_sentiment), sd_sent = sd(ave_sentiment), total_sent = sum(ave_sentiment), 
            max_sent = max(ave_sentiment), min_sent = min(ave_sentiment),
            avg_wc = mean(word_count), sd_wc = sd(word_count), total_wc = sum(word_count),
            max_wc = max(word_count), min_wc = min(word_count),
            avg_retweet = mean(tweet_pm_retweet_c), total_retweet = sum(tweet_pm_retweet_c),
            avg_reply = mean(tweet_pm_reply_c), total_reply = sum(tweet_pm_reply_c),
            avg_quote = mean(tweet_pm_quote_c), total_quote = sum(tweet_pm_quote_c),
            tweet_volume = n(),
            type_rtweet_ratio = sum(if_retweet)/n(), type_reply_ratio = sum(if_reply)/n())
#tweet_crea_date is IDate type, need to be converted to Date type for future join.
df_group <- mutate(df_group, tweet_crea_date = as.Date(tweet_crea_date))

# fwrite(df_group, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Group Data without Hashtag Problem.csv")



# Import and match trading data

df_trading <- fread("C:/Users/zyc71/Desktop/icodata_full/Trading Data/tradeinfo.csv")
unique_index <- distinct(df_group, index)

# Change data type of date and volume
df_trade <- df_trading[df_trading$index %in% unique_index$index, ] %>% 
  mutate(., date = as.Date(.$date, format = "%b %d  %Y"),volume = as.numeric(str_replace_all(.$volume, " ", ""))) %>% 
  rename(., market_cap = "market Cap", trading_vol = volume)

# There exists situation that one coin in the same date that has two price rows. They may have the same price but different decimals.
# So need to remove duplicates in the trading date by the index and date.
df_trade <- df_trade[!duplicated(df_trade[ ,1:2]),]

# Convert character to numeric, calculate return and logreturn
df_trade <- mutate(df_trade, open = as.numeric(open), high = as.numeric(high), low = as.numeric(low), close = as.numeric(close),
                   return = (close - open)/open, logreturn = log(close/open))

# Sort the data by index in ascending order of date
df_trade <- df_trade %>% group_by(index) %>% 
  arrange(date) %>% arrange(index)

# Convert market cap into numeric
df_trade$market_cap <- as.numeric(str_replace_all(df_trade$market_cap, " ", ""))

# Add columns: return, logreturn and trading volume of previous day (lag) and next day (lead).
df_trade <- df_trade %>% group_by(index) %>% 
  mutate(lag_return = dplyr::lag(return, n = 1, default = NA), 
         lead_return = dplyr::lead(return, n = 1, default = NA),
         lag_logreturn = dplyr::lag(logreturn, n = 1, default = NA),
         lead_logreturn = dplyr::lead(logreturn, n = 1, default = NA),
         lag_trading_vol = dplyr::lag(trading_vol, n = 1, default = NA),
         lead_trading_vol = dplyr::lead(trading_vol, n = 1, default = NA),
         lag_market_cap = dplyr::lag(market_cap, n = 1, default = NA),
         lead_market_cap = dplyr::lead(market_cap, n = 1, default = NA))

# Use mapply to calculate the return volatility over the past 10 days and change the remaining -1 to NA
df_trade$volatility <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$volatility[11:length(df_trade[df_trade$index == id, ]$volatility)] <- 
    mapply(FUN = function(x,y) sd(df_trade[df_trade$index == id, ]$return[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$return) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$return) - 1))
}
df_trade <- mutate(df_trade, volatility = na_if(volatility, -1))

# Use mapply to calculate the average return over the past 10 days (pre_returns) and change the remaining -1 into NA.
df_trade$pre_returns <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$pre_returns[11:length(df_trade[df_trade$index == id, ]$pre_returns)] <- 
    mapply(FUN = function(x,y) mean(df_trade[df_trade$index == id, ]$return[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$return) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$return) - 1))
}
df_trade <- mutate(df_trade, pre_returns = na_if(pre_returns, -1))

# Use mapply to calculate the logreturn volatility over the past 10 days (logreturn_volatility) and change the remaining -1 into NA.
df_trade$logreturn_volatility <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$logreturn_volatility[11:length(df_trade[df_trade$index == id, ]$logreturn_volatility)] <- 
    mapply(FUN = function(x,y) sd(df_trade[df_trade$index == id, ]$logreturn[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$logreturn) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$logreturn) - 1))
}
df_trade <- mutate(df_trade, logreturn_volatility = na_if(logreturn_volatility, -1))

# Use mapply to calculate the average logreturn over the past 10 days (pre_logreturns) and change the remaining -1 into NA.
df_trade$pre_logreturns <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$pre_logreturns[11:length(df_trade[df_trade$index == id, ]$pre_logreturns)] <- 
    mapply(FUN = function(x,y) mean(df_trade[df_trade$index == id, ]$logreturn[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$logreturn) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$logreturn) - 1))
}
df_trade <- mutate(df_trade, pre_logreturns = na_if(pre_logreturns, -1))

# Use mapply to calculate the market cap volatility over the past 10 days (market_cap_volatility) and change the remaining -1 into NA.
df_trade$market_cap_volatility <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$market_cap_volatility[11:length(df_trade[df_trade$index == id, ]$market_cap_volatility)] <- 
    mapply(FUN = function(x,y) sd(df_trade[df_trade$index == id, ]$market_cap[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$market_cap) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$market_cap) - 1))
}
df_trade <- mutate(df_trade, market_cap_volatility = na_if(market_cap_volatility, -1))

# Use mapply to calculate the average market cap over the past 10 days (pre_market_caps) and change the remaining -1 into NA.
df_trade$pre_market_caps <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$pre_market_caps[11:length(df_trade[df_trade$index == id, ]$pre_market_caps)] <- 
    mapply(FUN = function(x,y) mean(df_trade[df_trade$index == id, ]$market_cap[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$market_cap) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$market_cap) - 1))
}
df_trade <- mutate(df_trade, pre_market_caps = na_if(pre_market_caps, -1))

# Use mapply to calculate the trading volume volatility over the past 10 days (trading_vol_volatility) and change the remaining -1 into NA.
df_trade$trading_vol_volatility <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$trading_vol_volatility[11:length(df_trade[df_trade$index == id, ]$trading_vol_volatility)] <- 
    mapply(FUN = function(x,y) sd(df_trade[df_trade$index == id, ]$trading_vol[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$trading_vol) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$trading_vol) - 1))
}
df_trade <- mutate(df_trade, trading_vol_volatility = na_if(trading_vol_volatility, -1))

# Use mapply to calculate the average trading volume over the past 10 days (pre_trading_vols) and change the remaining -1 into NA.
df_trade$pre_trading_vols <- -1
for (id in unique(df_trade$index)) {
  df_trade[df_trade$index == id, ]$pre_trading_vols[11:length(df_trade[df_trade$index == id, ]$pre_trading_vols)] <- 
    mapply(FUN = function(x,y) mean(df_trade[df_trade$index == id, ]$trading_vol[x:y]), 
           x = 1:(length(df_trade[df_trade$index == id, ]$trading_vol) - 10), y = 10:(length(df_trade[df_trade$index == id, ]$trading_vol) - 1))
}
df_trade <- mutate(df_trade, pre_trading_vols = na_if(pre_trading_vols, -1))

# add columns of lag (previous day) and lead (next day) for the moving average and moving volatility.
df_trade <- df_trade %>% group_by(index) %>% 
  mutate(lag_volatility = dplyr::lag(volatility, n = 1, default = NA), 
         lead_volatility = dplyr::lead(volatility, n = 1, default = NA),
         lag_pre_returns = dplyr::lag(pre_returns, n = 1, default = NA), 
         lead_pre_returns = dplyr::lead(pre_returns, n = 1, default = NA),
         lag_logreturn_volatility = dplyr::lag(logreturn_volatility, n = 1, default = NA), 
         lead_logreturn_volatility = dplyr::lead(logreturn_volatility, n = 1, default = NA),
         lag_pre_logreturns = dplyr::lag(pre_logreturns, n = 1, default = NA), 
         lead_pre_logreturns = dplyr::lead(pre_logreturns, n = 1, default = NA),
         lag_market_cap_volatility = dplyr::lag(market_cap_volatility, n = 1, default = NA), 
         lead_market_cap_volatility = dplyr::lead(market_cap_volatility, n = 1, default = NA),
         lag_pre_market_caps = dplyr::lag(pre_market_caps, n = 1, default = NA), 
         lead_pre_market_caps = dplyr::lead(pre_market_caps, n = 1, default = NA),
         lag_trading_vol_volatility = dplyr::lag(trading_vol_volatility, n = 1, default = NA), 
         lead_trading_vol_volatility = dplyr::lead(trading_vol_volatility, n = 1, default = NA),
         lag_pre_trading_vols = dplyr::lag(pre_trading_vols, n = 1, default = NA), 
         lead_pre_trading_vols = dplyr::lead(pre_trading_vols, n = 1, default = NA))

# fwrite(df_trade, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Cleaned Trade Data.csv")



# Merge group data and trade data

# Change data type before merging, otherwise will throw error.
df_group <- mutate(df_group, tweet_crea_date = as.Date(tweet_crea_date))
df_trade <-mutate(df_trade, date = as.Date(date))

date_limit <- df_trade %>% group_by(index) %>% 
  summarize(min_date = min(date))
  
df_trade <- df_trade %>% group_by(index) %>% 
  filter(date <= min(date) + 180)

df_trade <- mutate(df_trade, date = as.Date(date))

# Panel data analysis include all trading days
df_all <- left_join(df_trade, df_group, by = c("index", "date" = "tweet_crea_date"))

# There is NA in sd_sent, sd_wc.Remove these two columns.
df_all <- select(df_all, !c(sd_sent, sd_wc,ico_name))

df_all <-  mutate_at(df_all, c("avg_sent", "total_sent", "max_sent", "min_sent", "avg_wc", "total_wc", "max_wc", 
                              "min_wc", "avg_retweet", "total_retweet", "avg_reply", "total_reply", "avg_quote", 
                              "total_quote", "tweet_volume", "type_rtweet_ratio", "type_reply_ratio"), function(x) ifelse(is.na(x), 0, x))

df_all <- df_all %>% group_by(index) %>% 
  mutate(lag_tweet_volume = dplyr::lag(tweet_volume, n = 1, default = NA),
         lead_tweet_volume = dplyr::lead(tweet_volume, n = 1, default = NA))

df_all <- mutate(df_all, diff_price = close - open, avg_price = 0.5*(open + close), 
                 adjusted_trading_vol = log(trading_vol + 1), adjusted_lag_trading_vol = log(lag_trading_vol + 1),
                 adjusted_lead_trading_vol = log(lead_trading_vol + 1), adjusted_pre_trading_vols = log(pre_trading_vols + 1),
                 adjusted_tweet_volume = log(tweet_volume + 1), adjusted_lag_tweet_volume = log(lag_tweet_volume + 1),
                 adjusted_lead_tweet_volume = log(lead_tweet_volume + 1), 
                 adjusted_market_cap = log(market_cap + 1),  adjusted_lag_market_cap = log(lag_market_cap + 1),
                 adjusted_lead_market_cap = log(lead_market_cap + 1), adjusted_pre_market_caps = log(pre_market_caps + 1),
                 retweet_ratio = total_retweet/tweet_volume)

df_all <- df_all %>% ungroup()

# fwrite(df_all, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Without Hashtag Problem/Cleaned Panel Data without Hashtag Problem.csv")



# Panel data analysis with fixed effect of token and data

result_volume <- plm(adjusted_trading_vol ~ tweet_volume + volatility +
                       avg_sent + adjusted_lag_trading_vol,
                     data = df_all, index = c("index", "date"), model = "within")
summary(result_volume)


result_lead_volume <- plm(adjusted_lead_trading_vol ~ tweet_volume + lead_volatility + 
                            avg_sent + adjusted_trading_vol,
                          data = df_all, index = c("index", "date"), model = "within")
summary(result_lead_volume)


result_return <- plm(return ~ tweet_volume + volatility + 
                       avg_sent + lag_return,
                     data = df_all, index = c("index", "date"), model = "within")
summary(result_return)


result_lead_return <- plm(lead_return ~ tweet_volume + lead_volatility + 
                            avg_sent + return,
                          data = df_all, index = c("index", "date"), model = "within")
summary(result_lead_return)


result_logreturn <- plm(logreturn ~ tweet_volume + logreturn_volatility + 
                          avg_sent + lag_logreturn,
                        data = df_all, index = c("index", "date"), model = "within")
summary(result_logreturn)


result_lead_logreturn <- plm(lead_logreturn ~ tweet_volume + lead_logreturn_volatility +  
                               avg_sent + logreturn,
                             data = df_all, index = c("index", "date"), model = "within")
summary(result_lead_logreturn)



# Panel data analysis with fixed effect and instrumental variables
# Previous literature suggests that tweets with media and hashtags can spread more widely and hence we use these two as instruments for tweet volume. 

# df_iv: Detect whether tweet contents contain mentions, hashtags, urls, cashtags, annotations and at the same time it is not retweet
df_iv <- df_clean %>% 
  mutate(hashtags = ifelse((str_detect(tweet_enti, "'hashtags':") & str_detect(tweet_ref_tweets, "'type': 'retweeted'", negate = TRUE)),1,0),
         urls = ifelse((str_detect(tweet_enti, "'urls':")& str_detect(tweet_ref_tweets, "'type': 'retweeted'", negate = TRUE)),1,0),
         mentions = ifelse((str_detect(tweet_enti, "'mentions':") & str_detect(tweet_ref_tweets, "'type': 'retweeted'", negate = TRUE)),1,0),
         cashtags = ifelse((str_detect(tweet_enti, "'cashtags':") & str_detect(tweet_ref_tweets, "'type': 'retweeted'", negate = TRUE)),1,0),
         media = ifelse((str_detect(tweet_atta, "'media_keys':") & str_detect(tweet_ref_tweets, "'type': 'retweeted'", negate = TRUE)),1,0))

# Convert NA of sentiment score sd to 0
df_iv <- df_iv %>% 
  mutate_at(.,"sd", function(x) ifelse(is.na(x), 0, x))

# fwrite(df_iv, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Without Hashtag Problem/Cleaned Data with Instrumental Variables (Not Retweet) without Hashtag Problem.csv")



#df_iv_group: Group by ICO project and date

df_iv_group <- df_iv %>% 
  group_by(index, ico_name, tweet_crea_date) %>% 
  summarize(avg_sent = mean(ave_sentiment), sd_sent = sd(ave_sentiment), total_sent = sum(ave_sentiment), 
            max_sent = max(ave_sentiment), min_sent = min(ave_sentiment),
            avg_wc = mean(word_count), sd_wc = sd(word_count), total_wc = sum(word_count),
            max_wc = max(word_count), min_wc = min(word_count),
            avg_retweet = mean(tweet_pm_retweet_c), total_retweet = sum(tweet_pm_retweet_c),
            avg_reply = mean(tweet_pm_reply_c), total_reply = sum(tweet_pm_reply_c),
            avg_quote = mean(tweet_pm_quote_c), total_quote = sum(tweet_pm_quote_c),
            tweet_volume = n(),
            type_rtweet_ratio = sum(if_retweet)/n(), type_reply_ratio = sum(if_reply)/n(),
            # Below is the difference between df_group and df_iv_group  
            hashtags_vol = sum(hashtags), urls_vol = sum(urls), mentions_vol = sum(mentions),
            cashtags_vol = sum(cashtags), media_vol = sum(media))

# tweet_crea_date is IDate type, need to be converted to Date type for future join.
df_iv_group <- mutate(df_iv_group, tweet_crea_date = as.Date(tweet_crea_date))

# fwrite (df_iv_group, file = "C:/Users/zyc71/Desktop/icodata_full/ICO Twitter Data/ICO Twitter Data/Without Hashtag Problem/IV Group Data without Hashtag Problem.csv")



# Merge iv group data and trade data

# Change data type before merging, otherwise will throw error.
df_iv_group <- mutate(df_iv_group, tweet_crea_date = as.Date(tweet_crea_date))
df_trade <-mutate(df_trade, date = as.Date(date))



# Panel data analysis with fixed effects and instrumental variables
df_all_iv <- left_join(df_trade, df_iv_group, by = c("index", "date" = "tweet_crea_date"))

# There is NA in sd_sent, sd_wc.Remove these two columns.
df_all_iv <- select(df_all_iv, !c(sd_sent, sd_wc,ico_name))

df_all_iv <-  mutate_at(df_all_iv, c("avg_sent", "total_sent", "max_sent", "min_sent", "avg_wc", "total_wc", "max_wc", 
                                     "min_wc", "avg_retweet", "total_retweet", "avg_reply", "total_reply", "avg_quote", 
                                     "total_quote", "tweet_volume", "type_rtweet_ratio", "type_reply_ratio", 
                                     "hashtags_vol", "urls_vol", "mentions_vol", "cashtags_vol", "media_vol"), function(x) ifelse(is.na(x), 0, x))

df_all_iv <- df_all_iv %>% group_by(index) %>% 
  mutate(lag_tweet_volume = dplyr::lag(tweet_volume, n = 1, default = NA),
         lead_tweet_volume = dplyr::lead(tweet_volume, n = 1, default = NA))

df_all_iv <- mutate(df_all_iv, diff_price = close - open, avg_price = 0.5*(open + close), 
                    adjusted_trading_vol = log(trading_vol + 1), adjusted_lag_trading_vol = log(lag_trading_vol + 1),
                    adjusted_lead_trading_vol = log(lead_trading_vol + 1), adjusted_pre_trading_vols = log(pre_trading_vols + 1),
                    adjusted_tweet_volume = log(tweet_volume + 1), adjusted_lag_tweet_volume = log(lag_tweet_volume + 1),
                    adjusted_lead_tweet_volume = log(lead_tweet_volume + 1), 
                    adjusted_market_cap = log(market_cap + 1),  adjusted_lag_market_cap = log(lag_market_cap + 1),
                    adjusted_lead_market_cap = log(lead_market_cap + 1), adjusted_pre_market_caps = log(pre_market_caps + 1),
                    retweet_ratio = total_retweet/tweet_volume)

df_all_iv <- df_all_iv %>% ungroup()


result_volume_iv <- plm(adjusted_trading_vol ~ tweet_volume + volatility + 
                          avg_sent + adjusted_lag_trading_vol|.-tweet_volume + media_vol + hashtags_vol,
                        data = df_all_iv, index = c("index", "date"), model = "within")
summary(result_volume_iv)

result_lead_volume_iv <- plm(adjusted_lead_trading_vol ~ tweet_volume + lead_volatility + 
                               avg_sent + adjusted_trading_vol|.-tweet_volume + media_vol + hashtags_vol,
                             data = df_all_iv, index = c("index", "date"), model = "within")
summary(result_lead_volume_iv)


result_return_iv <- plm(return ~ tweet_volume + volatility + 
                          avg_sent + lag_return|.-tweet_volume + media_vol + hashtags_vol,
                        data = df_all_iv, index = c("index", "date"), model = "within")
summary(result_return_iv)


result_lead_return_iv <- plm(lead_return ~ tweet_volume + lead_volatility + 
                               avg_sent + return|.-tweet_volume + media_vol + hashtags_vol,
                             data = df_all_iv, index = c("index", "date"), model = "within")
summary(result_lead_return_iv)


result_logreturn_iv <- plm(logreturn ~ tweet_volume + logreturn_volatility + 
                             avg_sent + lag_logreturn|.-tweet_volume + media_vol + hashtags_vol,
                           data = df_all_iv, index = c("index", "date"), model = "within")
summary(result_logreturn_iv)


result_lead_logreturn_iv <- plm(lead_logreturn ~ tweet_volume + lead_logreturn_volatility + 
                                  avg_sent + logreturn|.-tweet_volume + media_vol + hashtags_vol,
                                data = df_all_iv, index = c("index", "date"), model = "within")
summary(result_lead_logreturn_iv)



# Use Stargazer to generate descriptive statistics and regression table

# Descriptive statistics of overall data
stargazer(data.frame(df_all)[ , c("open", "close", "return", "logreturn", "volatility", "logreturn_volatility", "adjusted_trading_vol", 
                                  "tweet_volume", "avg_sent")],
          covariate.labels = c("open price", "close price", "trading return", "logarithmic trading return", "return volatility", "logarithmic return volatility", 
                               "log(trading volume)", "tweet volume", "sentiment"),
          title = "Descriptive Statistics", type = "html", out = "C:/Users/zyc71/Desktop/Descriptive Statistics.html")

# Panel data analysis result 
stargazer(result_volume, result_lead_volume, result_return, result_lead_return, result_logreturn, result_lead_logreturn, 
          covariate.labels = c("tweet volume", "return volatility", "return volatility (t+1)","logarithmic return volatility",
                               "logarithmic return volatility (t+1)", "sentiment score", "log(trading volume) (t-1)", "log(trading volume)",
                               "trading return (t-1)", "trading return", "logarithmic trading return (t-1)", "logarithmic trading return"),
          dep.var.labels = c("log(trading volume)", "log(trading volume) (t+1)", "trading return", "trading return (t+1)",
                             "logarithmic trading return", "logarithmic trading return (t+1)"),
          title = "Panel Data Analysis Result", keep.stat = c("n", "rsq", "f"),
          type = "html", out = "C:/Users/zyc71/Desktop/Panel Data Analysis Result.html")


# Panel data analysis with instrumental variables result 
stargazer(result_volume_iv, result_lead_volume_iv, result_return_iv, result_lead_return_iv, result_logreturn_iv, result_lead_logreturn_iv,
          covariate.labels = c("tweet volume", "return volatility", "return volatility (t+1)","logarithmic return volatility",
                               "logarithmic return volatility (t+1)", "sentiment score", "log(trading volume) (t-1)", "log(trading volume)",
                               "trading return (t-1)", "trading return", "logarithmic trading return (t-1)", "logarithmic trading return"),
          dep.var.labels = c("log(trading volume)", "log(trading volume) (t+1)", "trading return", "trading return (t+1)",
                             "logarithmic trading return", "logarithmic trading return (t+1)"),
          title = "Panel Data Analysis with Instrumental Variables Result", keep.stat = c("n", "rsq", "f"),
          type = "html", out = "C:/Users/zyc71/Desktop/Panel Data Analysis with Instrumental Variables Result.html")
