from implicit.bpr import BayesianPersonalizedRanking
from scipy import sparse as sp


def bpr(A: sp.spmatrix, factors: int):
    '''
    Run BayesianPersonalizedRanking -

    '''
    # TODO: Change here
    als = BayesianPersonalizedRanking(factors=factors)
    als.fit(A)
    item_factors = als.item_factors
    user_factors = als.user_factors

    return user_factors.dot(item_factors.T)
