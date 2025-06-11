"""Retrieve Content Shared By Influencers During Treatment

-- keywords: #FactsMatter, @AfricaCheck
-- start date: 13-03-23
-- end date:
"""

import pandas as pd
import glob
import time
import numpy as np

import sys
from tweetple import TweetPle
sys.path.insert(0, '../../src/utils')
from funcs import *
from datetime import date, timedelta
from tqdm import tqdm

sys.path.insert(0, '../../src/utils')
from general import *
from scrape import *


def start_date(days_ago):
    """Start date of scrape"""
    start = str(date.today() - timedelta(days_ago))
    start = start + "T00:00:00Z"

    return start


def end_date(days_ago):
    """Start date of scrape"""
    end = str(date.today() - timedelta(days_ago))
    end = end + "T23:59:59Z"

    return end


def scrape_tweets(accounts, path, bearer_token, days_ago):
    """Scrape Tweets"""
    end = end_date(days_ago - 6)
    TweetPle.TweetStreamer(
        accounts, bearer_token, path, start_date(days_ago), end
    ).main()

    return print("Content scraped")


def get_paths(days_ago, country):

    base = f"../../data/03-experiment/{country}/treatment/influencers/"
    rest = timedelta(days_ago - 6)
    datep = str(date.today() - rest)
    path_tw = base + "00-raw/twitter_b2/" + datep

    return path_tw, base, datep


def tweet_types(path_read):
    df = read_files(path_read)
    df.referenced_tweets = df.referenced_tweets.fillna({i: {} for i in df.index})
    m = pd.DataFrame(df["referenced_tweets"].tolist())
    m = m[0].apply(pd.Series)
    m.replace(
        {
            "retweeted": "retweet",
            "replied_to": "reply",
            "quoted": "quote",
            np.nan: "tweet",
        },
        inplace=True,
    )
    df["type"] = m["type"]
    df = df.reset_index(drop=True)
    df = df.drop("referenced_tweets", axis=1)
    return df


def content_twitter(path):
    """Find Tweets containing #FactsMatter"""
    pattern = '|'.join([f'(?i){word}' for word in words_tweets])
    pattern_url = '|'.join([f'(?i){url}' for url in urls_list])
    # df = tweet_types(path)
    df = read_files(path)
    df = expand_column(df, "entities.hashtags")
    df.rename({"tag": "hashtag"}, axis=1, inplace=1)
    df = df.drop(["end", "start"], axis=1)
    df = expand_column(df, "entities.urls")
    df0 = df.loc[df["text"].str.contains("#factsmatter", case=False)]
    df0['campaign_hashtag'] = 1
    df1 = df.loc[df["text"].str.contains("@africacheck", case=False)]
    df1['campaign_hashtag'] = 1
    df2 = df.loc[df["text"].str.contains(pattern, case=False)]
    df2 = df2[~df2['id'].isin(list(list(df0.id)+list(df1.id)))]
    df2['campaign_hashtag'] = 0
    df3 = df.loc[df["expanded_url"].str.contains(pattern_url, case=False, na=False)]
    df3 = df3[~df3['id'].isin(list(list(df0.id)+list(df1.id)))]
    df3['campaign_hashtag'] = 0
    df = df0.append(df1).append(df2).append(df3).drop_duplicates(subset="id").reset_index(drop=True)
    try:
        df["retweet"] = np.where(df.url.isna(), 1, 0)
    except:
        df["url"] = np.nan
        df["retweet"] = 1
    df = df[df["retweet"] != 1]
    df = df.drop("retweet", axis=1)
    df["tweet_url"] = "https://twitter.com/" + df.handle + "/status/" + df.id
    df.reset_index(drop=True,inplace=True)
    return df

def content_twitter_old(path):
    """Find Tweets containing #FactsMatter"""
    # df = tweet_types(path)
    df = read_files(path)
    df = expand_column(df, "entities.hashtags")
    df.rename({"tag": "hashtag"}, axis=1, inplace=1)
    df = df.drop(["end", "start"], axis=1)
    df0 = df.loc[df["text"].str.contains("#factsmatter", case=False)]
    df1 = df.loc[df["text"].str.contains("@africacheck", case=False)]
    df = df0.append(df1).drop_duplicates(subset="id").reset_index(drop=True)
    df = expand_column(df, "entities.urls")
    try:
        df["retweet"] = np.where(df.url.isna(), 1, 0)
    except:
        df["url"] = np.nan
        df["retweet"] = 1
    df = df[df["retweet"] != 1]
    df = df.drop("retweet", axis=1)
    df["tweet_url"] = "https://twitter.com/" + df.handle + "/status/" + df.id

    return df


def incremental_range(start, stop, step, inc):
    value = start
    while value < stop:
        yield value
        value += step
        step += inc


def create_list(df, start, step, inc):
    stop = len(df.columns)
    a_list = list(incremental_range(start, stop, step, inc))

    return a_list


def create_dictionaries(contents, n_contents, a_list, name1, name2):
    a_dict = dict(zip(contents, a_list))
    b_dict = dict(zip(n_contents, a_list))
    a_dict = {k: name1 + v for k, v in a_dict.items()}
    b_dict = {k: name2 + v for k, v in b_dict.items()}

    return a_dict, b_dict


def rename_columns(df, a_dict, b_dict):
    for key, value in a_dict.items():
        df.columns.values[key] = value
    for key, value in b_dict.items():
        df.columns.values[key] = value

    return df


def drop_duplicated_columns(df):
    df = df.loc[:, ~df.columns.duplicated()]

    return df


def problematic(df, t_col, n_col, str_find):
    prob = [col for col in df.columns if str_find in col]
    df["auxiliar"] = df[prob].sum(axis=1)
    df[n_col] = np.where((df["auxiliar"] == 0) & (df[t_col] == 1), 1, 0)
    df = df.drop(["auxiliar"], axis=1)

    return df


def africa_report(path_read, datep):
    """Generate report for AfricaCheck"""
    base = path_read.split("/*/")[0]
    df = pd.concat(map(pd.read_excel, glob.glob(f"{path_read}/twitter.xlsx")), axis=1)
    contents = create_list(df, 5, 7, 0)
    n_contents = create_list(df, 6, 7, 0)
    stop = len(df.columns)
    weeks = list(map(str, range(1, int(stop / 7) + 1)))
    n_contents_dict, contents_dict = create_dictionaries(
        contents, n_contents, weeks, "content_w", "n_content_w"
    )
    df = rename_columns(df, contents_dict, n_contents_dict)
    df = drop_duplicated_columns(df)
    df = problematic(df, "treatment", "potentially_problematic", "content_w")
    df.to_excel(f"{base}/{datep}/tracker_twitter.xlsx", index=False)


# def monitor_influencers(days_ago=1):
# --38, 31, 24, 17, 10
def monitor_influencers(days_ago=14, country='KE'):
    """Run process"""
    # Twitter
    _, _, _, _, bearer_token, _,_ = twitter_credentials(
        "../../conf/credentials.yaml"
    )
    participants_tw = get_participants_twitter(country)
    usernames_tw = list(participants_tw["username"])
    path_tw, base, datep = get_paths(days_ago, country)
    create_folder(path_tw)
    scrape_tweets(usernames_tw, f"{path_tw}/", bearer_token, days_ago)
    df_tw = content_twitter(f"{path_tw}")
    create_folder(f"{base}01-preprocessed/batch2/content/{datep}")
    df_tw.to_excel(f"{base}01-preprocessed/batch2/content/{datep}/tweets.xlsx", index=False)
    count_tw = df_tw.groupby("handle").count()
    count_tw_2 = df_tw.groupby("handle").sum()
    count_tw = count_tw.reset_index()[["handle", "id"]]
    count_tw_2 = count_tw_2.reset_index()[["handle",'public_metrics.impression_count',
                                       'public_metrics.like_count', 'public_metrics.quote_count',
                                       'public_metrics.reply_count', 'public_metrics.retweet_count']]
    count_tw = count_tw.rename({"id": "n_content", "handle": "username",
                                'public_metrics.impression_count': "n_impressions",
                                'public_metrics.like_count':'n_likes', 'public_metrics.quote_count':'n_quotes',
                                'public_metrics.reply_count':'n_replies', 'public_metrics.retweet_count':'n_retweets'}, axis=1)
    count_tw_2 = count_tw_2.rename({"handle": "username",
                                'public_metrics.impression_count': "n_impressions",
                                'public_metrics.like_count':'n_likes', 'public_metrics.quote_count':'n_quotes',
                                'public_metrics.reply_count':'n_replies', 'public_metrics.retweet_count':'n_retweets'}, axis=1)
    count_tw = count_tw.merge(count_tw_2, how="left", on="username")
    found = list(df_tw.handle.unique())
    participants_tw["content"] = np.where(participants_tw["username"].isin(found), 1, 0)
    create_folder(f"{base}01-preprocessed/batch2/report/{datep}")
    participants_tw = participants_tw.merge(count_tw, how="left", on="username")
    participants_tw[["n_content","n_impressions", 
                 "n_likes","n_quotes", "n_replies", 
                 "n_retweets"]] = participants_tw[["n_content","n_impressions", 
                                                  "n_likes","n_quotes",
                                                  "n_replies", "n_retweets"]].fillna(0)
    participants_tw.to_excel(
        f"{base}01-preprocessed/batch2/report/{datep}/twitter.xlsx", index=False
    )
    summ_tw = participants_tw.groupby(["treatment"]).sum().reset_index()
    summ_tw = summ_tw[["treatment", "content"]]
    summ_tw = summ_tw.rename({"content": "percent_share"}, axis=1)
    summ_tw["percent_share"] = (summ_tw["percent_share"] / 26) * 100
    summ_tw.to_excel(
        f"{base}01-preprocessed/batch2/report/{datep}/summary_twitter.xlsx", index=False
    )


if __name__ == "__main__":

    monitor_influencers()
