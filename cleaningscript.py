import pandas as pd
import numpy as np
import scipy
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error as mse


def clean(path):
    train = pd.read_csv(path)
    train = train.set_index('id').T
    train.reset_index(level=0, inplace=True)
    train = pd.merge(train, cal, left_on='index', right_on='d')
    train = train.rename(columns={'index':'id'})
    train.columns = [x[:13] if 'HOBBIES' in x else x for x in list(train.columns)]
    train['event_name_1'] = train['event_name_1'].fillna('None')
    train['event_type_1'] = train['event_type_1'].fillna('None')
    train['event_name_2'] = train['event_name_2'].fillna('None')
    train['event_type_2'] = train['event_type_2'].fillna('None')
    return train

cal = pd.read_csv('calendar_afcs2020.csv')
sample = pd.read_csv('sample_submission_afcs2020.csv')

train = clean('sales_train_validation_afcs2020.csv')
test = clean('sales_train_evaluation_afcs2020.csv')
sales = pd.read_csv('sell_prices_afcs2020.csv')

test.to_csv('testR2.csv')
