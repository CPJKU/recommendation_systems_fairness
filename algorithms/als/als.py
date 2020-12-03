import implicit
from scipy import sparse as sp


def ALS(A: sp.csr_matrix, factors: int, regularization: float, iter: int):
    '''
    :param A: Rating matrix nxm where m in the number of items. Must be a csr_matrix
    :param factors: embedding size
    :param regularization: regularization factor
    :param iter: number of iterations for ALS

    '''
    als = implicit.als.AlternatingLeastSquares(factors=factors,
                                               regularization=regularization,
                                               iterations=iter,
                                               use_gpu=True,
                                               num_threads=10)
    als.fit(A.T)
    item_factors = als.item_factors
    user_factors = als.user_factors

    return user_factors.dot(item_factors.T)
