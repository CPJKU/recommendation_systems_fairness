from implicit.bpr import BayesianPersonalizedRanking
from scipy import sparse as sp


def BPR(A: sp.coo_matrix, factors: int, lr: float, regularization: float, iterations: float):
    '''
    Run BayesianPersonalizedRanking - BPR: Bayesian Personalized Ranking from ImplicitFeedback
    :param A:
    :param factors:
    :param lr:
    :param regularization:
    :param iterations:
    '''
    bpr = BayesianPersonalizedRanking(factors=factors,
                                      learning_rate=lr,
                                      regularization=regularization,
                                      use_gpu=True,
                                      iterations=iterations,
                                      verify_negative_samples=True,
                                      num_threads=10
                                      )
    bpr.fit(A.T)

    # Last one is the bias term. However user_bias is 1 (not used) so a simple dot product works.
    item_factors = bpr.item_factors
    user_factors = bpr.user_factors
    return user_factors.dot(item_factors.T)
