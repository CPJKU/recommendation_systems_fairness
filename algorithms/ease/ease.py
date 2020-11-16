import numpy as np
from scipy import sparse as sp
from scipy.sparse.linalg import inv


def EASE(A: sp.spmatrix, lam: int):
    '''
    Run Embarassingly Shallow Autoencoders - Harald Steck
    https://arxiv.org/abs/1905.03375

    :param A: Rating matrix nxm where m in the number of items.
    :param lam: L2-norm regularization parameter
    :return: B weight matrix.
    '''

    def convert_to_64bit_indices(A):
        A.indptr = np.array(A.indptr, copy=False, dtype=np.int64)
        A.indices = np.array(A.indices, copy=False, dtype=np.int64)
        return A

    print('making a')
    a = convert_to_64bit_indices(A.transpose())
    print('making b')
    b = convert_to_64bit_indices(A)

    # Computer Gram matrix
    print('dot product')
    G = a @ b  # .toarray()
    print('end dot product')

    diagIndicies = np.diag_indices(G.shape[0])
    G[diagIndicies] += int(lam)

    # P = np.linalg.inv(G)
    print('Starting to invert')
    P = inv(G)
    print('Inverted!')

    B = P / (-np.diag(P))
    B[diagIndicies] = 0

    return B
