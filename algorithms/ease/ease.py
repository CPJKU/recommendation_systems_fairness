import numpy as np
from scipy import sparse as sp


def EASE(A: sp.spmatrix, lam: int):
    '''
    Run Embarassingly Shallow Autoencoders - Harald Steck
    https://arxiv.org/abs/1905.03375

    :param A: Rating matrix nxm where m in the number of items.
    :param lam: L2-norm regularization parameter
    :return: B weight matrix.
    '''

    # Computer Gram matrix
    G = A.transpose().dot(A).toarray()

    diagIndicies = np.diag_indices(G.shape[0])
    G[diagIndicies] += int(lam)

    P = np.linalg.inv(G)

    B = P / (-np.diag(P))
    B[diagIndicies] = 0

    return B
