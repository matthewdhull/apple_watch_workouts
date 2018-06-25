"""
For Activity Files (Strava, Apple Activity, Nike +)
  looks like the activity is the top level event
    session is under activity
    event is under session



    message type 'event' captures start & stops
    'timestamp', 'event', 'timer-trigger', 'event-group'

    message type 'session' COULD capture: (combine the total list of these fields into a common list. )
    [total_distance, avg_speed, total_timer_time, total_elapsed_time, start_time, timestamp, sport, sub_sport, message_index, trigger, event_type]

    OR

    [total_distance, avg_speed, total_calories, total_timer_time, total_elapsed_time, avg_heart_rate, max_heart_rate, min_heart_rate, start_time,
    timestamp, sport, sub_sport, message_index, trigger, event_type]


    message type 'activity' captures:
    timestamp, local_timestamp, num_sessions, type, event, event-type, total_timer_time

    if contains message type 'record' it captures:
    timestamp, heart_rate


    id,duration,distance,energy,source,hr,activity
    equivalency
    timestamp, total_timer_time, total_distance, total_calories, avg_heart_rate, sport
"""

from fitparse import FitFile
import pandas as pd


def fitparse_session(fn,source=None):
    fitfile = FitFile(fn)

    fields = ['timestamp', 'total_timer_time', 'total_distance', 'total_calories', 'source', 'avg_heart_rate', 'sport']
    df = pd.DataFrame(data=None,columns=fields)

    # Get all data messages that are of type record
    for record in fitfile.get_messages('session'):

        tdf = pd.DataFrame(data=None, columns=fields)
        tdf.loc[0] = [None] * len(fields)

        # Go through all the data entries in this record
        for record_data in record:

            # Print the records name and value (and units if it has any)
            if record_data.units:
                tdf.loc[0][record_data.name] = record_data.value
                #print("%s: %s %s" % (record_data.name, record_data.value,record_data.units))
            else:
                #print(" * %s: %s" % (record_data.name, record_data.value))
                v = record_data.value
                if record_data.name == 'timestamp':
                    v = pd.Timestamp(record_data.value)
                    tdf.loc[0][record_data.name] = record_data.value
                elif record_data.name in fields:
                    tdf.loc[0][record_data.name] = record_data.value

        df = pd.concat([df,tdf])
        df = df.set_index(['timestamp'])
        df.loc[df.index[0]]['source'] = source

    return df