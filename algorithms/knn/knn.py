from scipy import sparse as sp

from algorithms.knn.KNNCFRecommender import ItemKNNCF


def ItemKNN(A: sp.csc_matrix, maxk: int, shrink: int, similarity: str, normalize: bool):
    '''

    :param A: Rating matrix nxm where m in the number of items. Must be a csc_matrix
    :param maxk: number of neighbours to consider
    :param shrink: shrink factor from "Improved Neighborhood-based Collaborative Filtering" Bell and Koren.
    :param similarity: which similarity function to use (cosine,pearson,jaccard)
    :param normalize: if the denominator of the similarity function is used
    '''

    itemknncf = ItemKNNCF(user_num=A.shape[0], item_num=A.shape[1], maxk=maxk, shrink=shrink, similarity=similarity,
                          normalize=normalize)

    itemknncf.faster_fit(A)

    pred_mat = itemknncf.pred_mat

    return pred_mat
