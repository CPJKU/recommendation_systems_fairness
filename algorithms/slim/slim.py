import warnings

from scipy import sparse as sp
from sklearn.linear_model import ElasticNet
from tqdm import tqdm


def SLIM(A: sp.csc_matrix, elanet: ElasticNet):
    '''
    SLIM: Sparse Linear Methods for Top-N Recommender Systems -  Xia Ning ; George Karypis
    https://ieeexplore.ieee.org/document/6137254
    Run Sparse Linear Models over the rating matrix A.
    It uses the ElasticNet object internally.
    (code from https://github.com/MaurizioFD/RecSys2019_DeepLearning_Evaluation/blob/master/SLIM_ElasticNet/SLIMElasticNetRecommender.py)

    :param A: Rating matrix nxm where m in the number of items. Must be a csc_matrix
    :param elanet: ElasticNet object
    :return: W weight matrix.
    '''
    warnings.simplefilter("ignore")
    n_users, n_items = A.shape

    # Predicting each column at time
    W_rows_idxs = []
    W_cols_idxs = []
    W_data = []

    for j in tqdm(range(n_items), leave=False):
        # Target column
        aj = A[:, j].toarray()

        # Removing the j-th item from all users
        # Need to zero the data entries related to the j-th column
        st_idx = A.indptr[j]
        en_idx = A.indptr[j + 1]

        copy = A.data[st_idx:en_idx].copy()
        A.data[st_idx:en_idx] = 0.0

        # Predicting the column
        elanet.fit(A, aj)

        # Fetching the coefficients (sparse)
        widx = elanet.sparse_coef_.indices
        wdata = elanet.sparse_coef_.data

        # Save information about position in the final matrix
        W_rows_idxs += list(widx)
        W_cols_idxs += [j] * len(widx)
        W_data += list(wdata)

        # reconstrucing the matrix
        A.data[st_idx:en_idx] = copy

    W = sp.csr_matrix((W_data, (W_rows_idxs, W_cols_idxs)), shape=(n_items, n_items))

    return W
