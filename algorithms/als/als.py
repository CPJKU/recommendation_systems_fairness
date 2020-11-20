import warnings

from scipy import sparse as sp
from tqdm import tqdm

import implicit

def ALS(A: sp.csc_matrix, model: implicit.als.AlternatingLeastSquares):
    '''
    :param A: Rating matrix nxm where m in the number of items. Must be a csc_matrix
    :param model: implicit.als.AlternatingLeastSquares object
    :return: model
    '''
    
    model.fit(A.T)
    
    return model