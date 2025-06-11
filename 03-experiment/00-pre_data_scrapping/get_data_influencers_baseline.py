"""Retrieve Content Shared By Influencers During Baseline

KE & SA
-- start date: '2022-08-21T00:00:00Z'
-- end date: '2023-02-21T00:00:00Z'
"""

import pandas as pd
import os
import glob
import yaml
import time
import numpy as np

from tweetple import TweetPle
import sys
sys.path.insert(0, '../../src/utils')
from funcs import *
from tqdm import tqdm
sys.path.insert(0, '../../src/utils')
from general import *


def scrape_tweets(accounts, bearer_token, path, start_date, end_date):
    """Scrape Tweets"""
    TweetPle.TweetStreamer(
        accounts,
        bearer_token,
        path,
        start_date,
        end_date
    ).main()

    return print('Content scraped')


def scrape_posts(path, accounts, start_date, end_date, token):
    """Scrape Posts"""
    for account in tqdm(accounts):
        time.sleep(10)
        try:
            posts = GetPosts(
                account,
                start_date,
                end_date,
                token).main()
            posts.to_parquet(f'{path}/{account}.parquet')
        except ValueError:
            print(f"Oops!  Not content for {account}.")

    return print('Content scraped')


def get_path(country):
    base = f'../../data/03-experiment/{country}/baseline/'
    path_tw = base + '00-raw/influencers/tweets/'
    path_fb = base + '00-raw/influencers/posts/'
    path_likes = f'{base}00-raw/influencers/likes/'
    path_followers = f'{base}00-raw/influencers/followers/'
    path_converstations = f'{base}00-raw/influencers/conversations/'
    path_retweets = f'{base}00-raw/influencers/retweets/'
    return path_tw, path_fb, base, path_likes, path_followers, path_converstations, path_retweets


def unique_tweets(path_read):
    """Get unique tweets"""
    df = read_files(path_read)
    df = df.drop_duplicates(subset='id').reset_index(drop=True)
    df = expand_column(df, 'entities.urls')

    return df


def unique_posts(path_read, path_save):
    """Get unique posts"""
    df = read_files(path_read)
    df = df.drop_duplicates(subset='postUrl')
    df = df.reset_index(drop=True)
    df.to_parquet(f'{path_save}posts.parquet')
    return df


def tweets_w_likes(path_read):
    """Get Tweets with at least one like"""
    df = read_files(path_read)
    df = df.drop_duplicates(subset='id').reset_index(drop=True)
    df['author_id'] = df['author_id'].astype(str) ### change
    #df = df[df['author_id']=='937779284'] ### change
    df = df[df['public_metrics.like_count'] > 0] 
    df = df.reset_index(drop=True)
    ids = df.sort_values(
        ['public_metrics.like_count'], ascending=False
    )['id']
    ids = list(ids)
    return ids


def tweets_w_retweets(path_read):
    """Get Tweets with at least one like"""
    df = read_files(path_read)
    df = df.drop_duplicates(subset='id').reset_index(drop=True)
    df = df[df['public_metrics.retweet_count'] > 0]
    df = df.reset_index(drop=True)
    ids = df.sort_values(
        ['public_metrics.retweet_count'], ascending=False
    )['id']
    ids = list(ids)
    return ids


def unique_likes(tweets, path_read, path_save):
    """Get unique users `liking users` of Tweets
    shared by influencers
    """
    col_a = 'liking_user_username'
    col_b = 'tweet_liked'
    col_c = 'influencer_id'
    col_d = 'liking_user_id'
    df = read_files(path_read)
    df = df.drop_duplicates(['id', col_b])
    df = df.rename(
        {'id': col_d, 'username': col_a},
        axis=1
    )
    df = df.reset_index(drop=True)
    tweets = tweets[['author_id', 'id']]
    tweets = tweets.rename(
        {'author_id': col_c, 'id': col_b},
        axis=1
    )
    df = df.merge(tweets, on=col_b, how='left')
    df = df[
        [col_d, col_a, col_b, col_c]
    ]
    df.to_parquet(f'{path_save}likes.parquet')

    return df


def unique_replies(tweets, path_read, path_save):
    """Get unique users replying to Tweets
    shared by influencers
    """
    col_a = 'replier_user_id'
    col_b = 'tweet_replied'
    col_c = 'influencer_id'
    df = read_files(path_read)
    df = df.drop_duplicates(['author_id', 'id', 'in_reply_to_user_id'])

    df = df.rename(
        {'author_id': col_a, 'id': col_b, 'in_reply_to_user_id': col_c},
        axis=1
    )
    df = df.reset_index(drop=True)
    df = df[[col_a, col_b, col_c]]
    df.to_parquet(f'{path_save}replies.parquet')

    return df


def scrape_followers(participants_tw, country, path_save):
    b_path = f'../../data/01-characterize/followers/{country}/00-raw/'
    file = 'integrate/followers.gzip'
    a_followers = pd.read_parquet(f'{b_path}{file}')
    a_followers = a_followers.rename(
        {'id': 'follower_id', 'author_id_following': 'influencer_id'},
        axis=1
    )
    participants_tw = participants_tw.rename(
        {'author_id': 'influencer_id'},
        axis=1
    )
    followers = a_followers[
        a_followers.influencer_id.isin(participants_tw.influencer_id.unique())
    ]
    followers = followers.reset_index(drop=True)
    followers.to_parquet(f'{path_save}followers.parquet')
    
    return followers


def tweet_types(df, path_save):
    df0 = df[~df.referenced_tweets.isna()]
    m = pd.DataFrame(df0['referenced_tweets'].tolist())
    m = m[0].apply(pd.Series)
    m.replace({'retweeted': 'retweet', 'replied_to': 'reply',
               'quoted': 'quote'}, inplace=True)
    df0 = pd.concat([df0, m['type']], axis=1)
    df1 = df[df.referenced_tweets.isna()]
    df1['type'] = 'tweet'
    df = df0.append(df1).reset_index(drop=True)
    df = df[~df.id.isna()].reset_index(drop=True)
    create_folder(path_save)
    df.to_parquet(f'{path_save}tweets.parquet')
    return df


def assign_followers_treatment(participants, followers):

    participants = participants[['author_id', 'treatment']]
    participants.rename({'author_id': 'influencer_id'}, axis=1, inplace=True)
    participants['influencer_id'] = participants['influencer_id'].astype(str)
    followers = followers.merge(participants, on='influencer_id', how='left')
    m = pd.DataFrame(followers.groupby('follower_id')[
        'treatment'].apply(list)).reset_index()
    m['set_col'] = np.where(m.treatment == '', '', m.treatment.map(set))
    m['count'] = m.set_col.str.len()
    z = m[m['count'] == 1]
    valid_followers = list(z['follower_id'].unique())
    df0 = followers[followers['follower_id'].isin(valid_followers)]
    df1 = followers[~followers['follower_id'].isin(valid_followers)].drop([
        'treatment'], axis=1)
    df = df0.append(df1).reset_index(drop=True)
    df.rename({'treatment': 'assignment'}, axis=1, inplace=True)
    df['assignment'].replace(
        {1: 'treatment', 0: 'control', np.nan: 'both'}, inplace=True)

    return df


def get_followers_quality(sample, followers, likes, replies, path_save):
    sample['author_id'] = sample['author_id'].astype(str)
    participants = list(sample['author_id'])
    followers['influencer_id'] = followers['influencer_id'].astype(str)

    quality_likes = pd.DataFrame()
    for participant in participants:
        followersp = followers[followers['influencer_id'] == participant]
        likesp = likes[likes['influencer_id'] == participant]
        list_to_look = list(followersp['follower_id'].unique())
        quality_likes = quality_likes.append(
            likesp[likesp['liking_user_id'].isin(list_to_look)]
        )
    quality_likes = quality_likes.rename(
        {'liking_user_username': 'follower_username'},
        axis=1
    )
    quality_likes = quality_likes[[
        'follower_username', 'liking_user_id', 'influencer_id']]
    quality_likes = quality_likes.drop_duplicates()
    quality_likes = quality_likes.rename(
        {'liking_user_id': 'follower_id'}, axis=1)
    quality_likes = quality_likes.reset_index(drop=True)
    quality_likes['liked'] = 1

    quality_replies = pd.DataFrame()
    for participant in participants:
        followersp = followers[followers['influencer_id'] == participant]
        repliesp = replies[replies['influencer_id'] == participant]
        list_to_look = list(followersp['follower_id'].unique())
        quality_replies = quality_replies.append(
            repliesp[repliesp['replier_user_id'].isin(list_to_look)]
        )
    quality_replies = quality_replies[[
        'replier_user_id', 'influencer_id']]
    quality_replies = quality_replies.drop_duplicates()
    quality_replies = quality_replies.rename(
        {'replier_user_id': 'follower_id'}, axis=1)
    quality_replies = quality_replies.reset_index(drop=True)
    quality_replies['replied'] = 1

    followers.rename({'username': 'follower_username'}, axis=1, inplace=True)
    cols_keep = ['follower_username', 'follower_id', 'influencer_id']
    followers = followers[cols_keep]
    followers['influencer_id'] = followers['influencer_id'].astype(str)
    followers = followers.merge(quality_likes, on=cols_keep, how='left')
    followers = followers.merge(
        quality_replies,
        on=['follower_id', 'influencer_id'],
        how='left'
    )
    followers[['replied', 'liked']] = followers[['replied', 'liked']].fillna(0)
    followers['strong'] = np.where(
        (followers.liked == 1) & (followers.replied == 1), 1, 0)
    followers['weak'] = np.where(
        (followers.liked == 1) | (followers.replied == 1), 1, 0)
    followers['weak'] = np.where(followers['strong']== 1, 0, df['weak'])
    #followers = assign_followers_treatment(sample, followers)
    followers.to_parquet(f'{path_save}followers.parquet')

    return followers


def get_baseline_content(start_date='2022-08-21T00:00:00Z', end_date='2023-02-21T00:00:00Z', country='SA',
quality_start_date='2022-08-21', quality_end_date='2022-02-21'):
    """Retrieve content from Social Media Influencers"""

    # Path
    path_tw, path_fb, base, path_likes, path_followers, path_converstations, path_retweets = get_path(country)
    path_save = f'{base}01-preprocess/influencers/'

    # Twitter
    _, _, bearer_token, _, _, _, _ = twitter_credentials(
        '../../conf/credentials.yaml')
    participants_tw = get_influencers(country)
    participants_tw['author_id'] = participants_tw['author_id'].astype(str)
    #participants_tw = participants_tw[participants_tw["username"].isin(['Wairimuwraps'])] ### change
    usernames_tw = list(participants_tw['username'])
    ids_tw = list(participants_tw['author_id'].astype(str))
    create_folder(path_tw)

    # Tweets
    #scrape_tweets(usernames_tw, bearer_token, path_tw, start_date, end_date)

    # Conversation threads
    #tweets = unique_tweets(path_tw)
    #tweets = tweet_types(tweets, path_save) 
    #tweets["author_id"] = tweets["author_id"].astype(str)
    #tweets = tweets[tweets["author_id"].isin(ids_tw)] ### change
    #df = tweets[tweets.type != 'reply'] 
    #df = df[df['public_metrics.reply_count'] > 0]
    #df = df.sort_values(by='public_metrics.reply_count', ascending=False)
    #conversation_ids = list(df['id'].unique())
    #TweetPle.get_threads(
    #    conversation_ids,
    #    bearer_token,
    #    path_save=path_converstations
    #)

    # Likers
    #ids_tweets = tweets_w_likes(path_tw)
    #TweetPle.TweepleStreamer(
    #    ids_tweets,
    #    bearer_token,
    #    path_save=path_likes
    #).likes_lookup()

    # Retweeters
    #ids_tweets = tweets_w_retweets(path_tw)
    # TweetPle.TweepleStreamer(
    #    ids_tweets,
    #    bearer_token,
    #    path_save=path_retweets
    # ).retweet_lookup()

    # Read Tweets
    tweets = pd.read_parquet(f"{base}01-preprocess/influencers/tweets.parquet")
    tweets_filtered = tweets[(tweets['created_at'] >= quality_start_date) & (tweets['created_at'] <= quality_end_date)]
  

    # Followers
    followers = scrape_followers(participants_tw, country, path_followers)
    
#
    # Quality Followers
    likes = unique_likes(tweets, path_likes, path_save)
#
    replies = unique_replies(tweets, path_converstations, path_save)
#
    get_followers_quality(
        participants_tw,
        followers,
        likes,
        replies,
        path_save
    )

    # Facebook
    # token = facebook_credentials('credentials.yaml')
    # participants_fb = get_participants_facebook()
    # usernames_fb = list(participants_fb['Platform Id'])
    # create_folder(path_fb)
    # scrape_posts(path_fb, usernames_fb, start_date, end_date, token)
    # unique_posts(path_fb, path_save)


if __name__ == "__main__":

    get_baseline_content()
