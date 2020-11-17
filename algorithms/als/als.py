from implicit.als import AlternatingLeastSquares
from scipy import sparse as sp


def als(A: sp.spmatrix, factors: int):
    '''
    Run Alternating Least Squares - Hu et al.
    https://lkpy.readthedocs.io/en/stable/mf.html#hkv2008

    '''

    als = AlternatingLeastSquares(factors=factors)
    als.fit(A)
    item_factors = als.item_factors
    user_factors = als.user_factors

    return user_factors.dot(item_factors.T)
