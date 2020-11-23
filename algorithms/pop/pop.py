import bottleneck as bn
import numpy as np
from scipy import sparse as sp


def PopularItems(A: sp.csc_matrix, limit=50):
    """
    Returns the most popular items
    :param A: user-item matrix
    """

    n = A.shape[0]
    # used for indexing
    dummy_column = np.arange(n).reshape(n, 1)

    # Counting the number of interactions
    item_count = np.asarray(A.sum(axis=0)).reshape(-1)

    # Partially sorted indexes
    part_sort_indexes = bn.argpartition(-item_count, kth=limit)
    # Focusing on the tops
    unsorted_idx_tops = part_sort_indexes[:limit]
    unsorted_tops = item_count[unsorted_idx_tops]
    sorted_idx_tops_part = np.argsort(unsorted_tops)
    # Extracting the indexes of the tops respect of the original array
    sorted_idx_tops = part_sort_indexes[sorted_idx_tops_part]

    recommend = sp.lil_matrix(A.shape)
    # We assign real values between 0.5 and 1 to the tops so we can employ ranking metrics.
    recommend[dummy_column, sorted_idx_tops] = np.linspace(start=0.5, stop=1.0, num=limit)

    return recommend
