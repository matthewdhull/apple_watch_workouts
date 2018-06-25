import os
import glob
import pandas as pd
from fitparse_session import fitparse_session

"""
This is capable of handling 
"""

# get all fit files
os.chdir('/Users/matthewhull/Library/Mobile Documents/iCloud~com~altifondo~HealthFit/Documents/')
# only fit files
strava_fitfiles = glob.glob('*Strava*.fit')
nike_fitfiles = glob.glob('*Nike*.fit')
apple_fitfiles = glob.glob('*Apple*.fit')
print('%i Apple fitfiles' % len(apple_fitfiles))
print('%i Strava fitfiles' % len(strava_fitfiles))
print('%i Nike fitfiles' % len(nike_fitfiles))


def process_nike():
    df = pd.DataFrame()
    for f in nike_fitfiles:
        tdf = fitparse_session(f,'Nike+')
        df = pd.concat([df,tdf])
    return df

nike_df = process_nike()



def process_strava():
    df = pd.DataFrame()
    for f in strava_fitfiles:
        tdf = fitparse_session(f,'Strava')
        df = pd.concat([df,tdf])
    return df

strava_df = process_strava()



def process_appleactivity():
    df = pd.DataFrame()
    for f in apple_fitfiles:
        tdf = fitparse_session(f,'AppleActivity')
        df = pd.concat([df,tdf])
    return df

appleactivity_df = process_appleactivity()

df = pd.concat([nike_df,strava_df,appleactivity_df])
pd.DataFrame.to_csv(df,'/Users/matthewhull/r/apple_watch_workouts/fit.csv')
print('done')

# id,duration,distance,energy,source,hr,activity
