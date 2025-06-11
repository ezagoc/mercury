"""Retrieve Followers from Influencers

KE & SA
-- Retrieved on: 21-11-22

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
from funcs import  *
from tqdm import tqdm
sys.path.insert(0, '../../src/utils')
from general import *

faltan = ['301941865',
 '71592661',
 '367084378',
 '224695067',
 '333277490',
 '152967995',
 '117027524']


def get_path(country):
    base = f'../../data/01-characterize/followers/{country}/'
    path_save_c = base + '00-raw/collect/'
    path_save_i = base + '00-raw/integrate/'

    return path_save_c, path_save_i

def get_inf_followers(country='SA'):
    """Retrieve followers from Social Media Influencers"""

    # Path
    path_save_c, path_save_i = get_path(country)
    

    # Twitter
    _, _, _, _, _, bearer_token, _ = twitter_credentials(
        '../../conf/credentials.yaml')
    participants_tw = get_influencers(country)
    participants_tw['author_id'] = participants_tw['author_id'].astype(str)
    participants_tw = participants_tw[participants_tw['author_id'].isin(faltan)]
    ids_tw = list(participants_tw['author_id'].map(str))

    TweetPle.TweepleStreamer(
        ids_tw[2:3], 
        bearer_token, 
        path_save=path_save_c
    ).followers_lookup()
    
    # Concat all followers
    #all_files = glob.glob(os.path.join(path_save_c, "*.parquet"))
    #df = pd.concat((pd.read_parquet(f) for f in all_files), ignore_index=True)
    #df.to_parquet(f'{path_save_i}followers.gzip', compression='gzip')
if __name__ == "__main__":
    get_inf_followers()