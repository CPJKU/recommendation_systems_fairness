from implicit.bpr import BayesianPersonalizedRanking
from scipy import sparse as sp


def ALS(A: sp.spmatrix, factors: int):
    '''
    Run ABayesianPersonalizedRanking -

    '''
    # TODO: Change here
    als = BayesianPersonalizedRanking(factors=factors)
    als.fit(A)
    item_factors = als.item_factors
    user_factors = als.user_factors

    return user_factors.dot(item_factors.T)
