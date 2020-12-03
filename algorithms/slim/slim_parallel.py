import itertools
import multiprocessing
import warnings

from scipy import sparse as sp
from sklearn.linear_model import ElasticNet
from tqdm import trange


def SLIM_parallel(A: sp.csc_matrix, alpha: float, l1_ratio: float, max_iter: int):
    '''
    SLIM: Sparse Linear Methods for Top-N Recommender Systems -  Xia Ning ; George Karypis
    https://ieeexplore.ieee.org/document/6137254
    Run Sparse Linear Models over the rating matrix A.
    (code from https://github.com/ruhan/toyslim/blob/master/slim_parallel.py)

    :param A: Rating matrix nxm where m in the number of items. Must be a csc_matrix
    :param alpha: a + b (a and b are the multipliers of the L1 and L2 penalties, respectively)
    :param l1_ratio: a / (a + b) (a and b are the multipliers of the L1 and L2 penalties, respectively)
    :param max_iter: number of iterations to run max for each item
    :return: W weight matrix.
    '''
    warnings.simplefilter("ignore")

    n_items = A.shape[1]

    ranges = generate_slices(n_items)
    separated_tasks = []

    for from_j, to_j in ranges:
        separated_tasks.append([from_j, to_j, A.copy(), alpha, l1_ratio, max_iter])

    with multiprocessing.Pool() as pool:
        results = pool.map(work, separated_tasks)

    W_rows_idxs = list(itertools.chain(*[x[0] for x in results]))
    W_cols_idxs = list(itertools.chain(*[x[1] for x in results]))
    W_data = list(itertools.chain(*[x[2] for x in results]))

    W = sp.csr_matrix((W_data, (W_rows_idxs, W_cols_idxs)), shape=(n_items, n_items))

    return W


def work(params):
    from_j, to_j = params[0], params[1]
    A = params[2]
    alpha, l1_ratio, max_iter = params[3], params[4], params[5]

    elanet = ElasticNet(
        alpha=alpha,
        l1_ratio=l1_ratio,
        fit_intercept=False,  # Not considered by SLIM
        positive=True,  # Constraint in SLIM
        copy_X=False,  # efficiency reasons
        max_iter=max_iter,
        selection="random",  # efficiency reasons
        tol=1e-4  # assuming a good tolerance
    )

    W_rows_idxs = []
    W_cols_idxs = []
    W_data = []

    for j in trange(from_j, to_j, desc='{} -> {}'.format(from_j, to_j)):
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

        # reconstructing the matrix
        A.data[st_idx:en_idx] = copy

    return (W_rows_idxs, W_cols_idxs, W_data)


def generate_slices(total_columns):
    """
    Generate slices that will be processed based on the number of cores
    available on the machine.
    """
    from multiprocessing import cpu_count

    cores = cpu_count()
    print('Running on {} cores'.format(cores))
    segment_length = total_columns // cores

    ranges = []
    now = 0

    while now < total_columns:
        end = now + segment_length

        # The last part can be a little greater that others in some cases, but
        # we can't generate more than #cores ranges
        end = end if end + segment_length <= total_columns else total_columns
        ranges.append((now, end))
        now = end

    return ranges
