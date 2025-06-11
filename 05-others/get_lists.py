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

# Change your paths for the actual experiment

def get_path(country = 'SA'):
    base = f'../../data/01-characterize/followers/{country}/'
    base2 = f'../../data/06-others/{country}/'
    path_save_c = base + '00-raw/collect/'
    path_save_i = base + '00-raw/integrate/'
    path_lists = base2 + 'twitter_ads/lists/'
    path_unmatched = base2 + 'twitter_ads/unmatched/'

    return path_save_c, path_save_i, path_lists, path_unmatched

def get_lists(country):
    
    path_c, path_i, path_l, path_un = get_path(country)
    df_i = pd.read_parquet(f'{path_i}followers.gzip')
    
    inf = df_i[['author_id_following']]
    inf = inf.drop_duplicates().reset_index(drop=True)
    inf = list(inf['author_id_following'])
    
    for influencer in inf:
        df = df_i[df_i['author_id_following'] == influencer]
        df = df[['id']]
        df.to_csv(f'{path_l}{influencer}.txt', header = None, index = None, sep = ',')
        
        
if __name__ == "__main__":
    get_inf_followers()