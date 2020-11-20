import warnings

from scipy import sparse as sp
from tqdm import tqdm
import pandas as pd

def KNNCF(A: sp.csc_matrix, model):
    '''
    :param A: Rating matrix nxm where m in the number of items. Must be a csc_matrix
    :param model: implicit.als.AlternatingLeastSquares object
    :return: model
    '''
    
    _ratings_dict = {'item': [], 'user': [], 'rating': []}
    for _user_i, _user_vec in enumerate(A):
        #for _item_i, _rating in enumerate(_user_vec.toarray()[0]):
        for _item_i in _user_vec.indices:
            _ratings_dict['item'].append(_item_i)
            _ratings_dict['user'].append(_user_i)
            _ratings_dict['rating'].append(1.0)
    
    _dataset = pd.DataFrame(_ratings_dict)

    model.fit(_dataset)
        
    
    return model