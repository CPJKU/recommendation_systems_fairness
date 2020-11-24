import bottleneck as bn
import numpy as np
from scipy import sparse as sp
from typing import List

from conf import LEVELS
from utils.data_splitter import UserGroup

import pandas as pd
import pdb


def NDCG_at_k_batch(logits, y_true, k=10):
    '''
    Computes the normalised discount cumulative gain at k.
    It sorts both the un-normalised predictions and the true predictions according to their score.
    It looks at the top-k results and checks how many items in the un-normalised predictions appear also in the
    true ranking. The resulting items are then sorted according to the un-normalised ranking and discounted according
    the definition of DCG. We assume relevance = 1 for each item. IDCG is computed considered having all true items
    in the first positions. The function then returns DCG/IDCG.

    (NOTE) if there are fewer than k elements in the true predictions, then k is adjusted according to the number
    of non-zero values in the true preditions.
    :param logits: the un-normalised predictions
    :param y_true: the true predictions
    :param k: cut-off value
    :return: NDCG at k
    '''

    n = logits.shape[0]
    # used for indexing
    dummy_column = np.arange(n).reshape(n, 1)

    # Finding the indexes related to the top-k
    idx_tops = bn.argpartition(-logits, k, axis=1)[:, :k]
    # Extract the (unsorted tops)
    tops = logits[dummy_column, idx_tops]
    # Sorting these and extracting the indices (respect to the subset)
    idx_sorted_tops_sub = np.argsort(-tops, axis=1)
    # Extracting the real indices of the sorted array from the real set
    idx_sorted_tops = idx_tops[dummy_column, idx_sorted_tops_sub]

    # Finding the indexes related to the top-k in the true predictions
    idx_tops_true = bn.argpartition(-y_true, k, axis=1)[:, :k]
    # Using a binary vector for saving the indices
    y_true_bin = np.zeros_like(y_true)
    # Setting True only to the top-k entries
    y_true_bin[dummy_column, idx_tops_true] = 1.
    # Setting 0 values to False (avoiding cases where 0 values are considered elements of top)
    # This takes care of the cases where the test set contains fewer than k elements
    y_true_bin[np.where(y_true == 0)] = 0.

    # discount value
    dv = 1. / np.log2(np.arange(2, k + 2))

    # discount cumulative gain
    # We extract the binary ranking
    logits_sorted_ones = y_true_bin[dummy_column, idx_sorted_tops]
    DCG = np.sum(logits_sorted_ones * dv, axis=1)

    # ideal discount cumulative gain
    # we take the top-k (bunch of ones)
    y_true_ones = y_true_bin[dummy_column, idx_tops_true]
    # naively sort them (to have the 1 at the beginning)
    idx_true_ones = np.argsort(-y_true_ones, axis=1)
    y_true_sorted_ones = y_true_ones[dummy_column, idx_true_ones]
    IDCG = np.sum(y_true_sorted_ones * dv, axis=1)

    # sanity check
    assert (IDCG - DCG >= 0).all()
    return DCG / IDCG


def Recall_at_k_batch(logits, y_true, k=10):
    '''
    Computes the normalised recall at k.
    It sorts both the un-normalised predictions and the true predictions according to their score.
    It looks at the top-k results and checks how many items in the un-normalised predictions appear also in the
    true ranking. The result is then divided by a normalisation value (see NOTE) in order to have a range in [0,1].

    (NOTE) if there are fewer than k elements in the true predictions, then k is adjusted according to the number
    of non-zero values in the true preditions.
    :param logits: the un-normalised predictions
    :param y_true: the true predictions
    :param k: cut-off value
    :return: normalised recall at k
    '''

    n = logits.shape[0]
    # used for indexing
    dummy_column = np.arange(n).reshape(n, 1)

    # Normalisation value min between k and the number of elements in true predictions
    non_zeros = np.count_nonzero(y_true, axis=1)
    norms = np.where(non_zeros < k, non_zeros, k)

    # Finding the indexes related to the top-k
    idx_tops = bn.argpartition(-logits, k, axis=1)[:, :k]
    # Using a binary vector for saving the indices
    logits_bin = np.zeros_like(logits, dtype=bool)
    # Setting True only to the top-k entries
    logits_bin[dummy_column, idx_tops] = True

    # Finding the indexes related to the top-k in the true predictions
    idx_tops_true = bn.argpartition(-y_true, k, axis=1)[:, :k]
    y_true_bin = np.zeros_like(y_true, dtype=bool)
    y_true_bin[dummy_column, idx_tops_true] = True
    # Setting 0 values to False (avoids cases where intersection with 0 are counted as positives)
    y_true_bin[np.where(y_true == 0)] = False

    # counting how many values where in the top-k predictions
    div = (np.logical_and(logits_bin, y_true_bin).sum(axis=1)).astype("float32")

    recall = div / norms
    assert (recall >= 0).all() and (recall <= 1).all()

    return recall


def NDCG_binary_at_k_batch(logits, y_true, k=10):
    """
    Function taken from Variational Autoencoders for Collaborative Filtering.
    :param logits: the un-normalised predictions
    :param y_true: the true predictions (binary)
    :param k: cut-off value
    :return: NDCG at k
    """
    n = logits.shape[0]
    dummy_column = np.arange(n).reshape(n, 1)

    idx_topk_part = bn.argpartition(-logits, k, axis=1)[:, :k]
    topk_part = logits[dummy_column, idx_topk_part]
    idx_part = np.argsort(-topk_part, axis=1)
    idx_topk = idx_topk_part[dummy_column, idx_part]
    # build the discount template
    tp = 1. / np.log2(np.arange(2, k + 2))

    DCG = (y_true[dummy_column, idx_topk].toarray() * tp).sum(axis=1)
    IDCG = np.array([(tp[:min(n, k)]).sum() for n in y_true.getnnz(axis=1)])

    # TODO: issue with the precision here, assertion fails
    DCG = np.round(DCG, 13)
    IDCG = np.round(IDCG, 13)
    # sanity check
    assert (IDCG - DCG >= 0).all()
    return DCG / IDCG


def Recall_binary_at_k_batch(logits, y_true, k=10):
    """
    Function taken from Variational Autoencoders for Collaborative Filtering
    :param logits: the un-normalised predictions
    :param y_true: the true predictions (binary)
    :param k: cut-off value
    :return: normalised recall at k
    """
    n = logits.shape[0]
    dummy_column = np.arange(n).reshape(n, 1)

    idx_topk_part = bn.argpartition(-logits, k, axis=1)[:, :k]
    X_pred_binary = np.zeros_like(logits, dtype=bool)
    X_pred_binary[dummy_column, idx_topk_part] = True

    X_true_binary = (y_true > 0).toarray()
    tmp = (np.logical_and(X_true_binary, X_pred_binary).sum(axis=1)).astype(
        np.float32)
    recall = tmp / np.minimum(k, X_true_binary.sum(axis=1))

    assert (recall >= 0).all() and (recall <= 1).all()

    return recall

def Diversity_Shannon_at_k_batch(logits,
                                 tids_path,
                                 normalized=True,
                                 k=10,
                                 tracklist_path="/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_regexp_since_2016_pc_gt_1_user_gte_5_song_gte_5/song_ids.txt"):
    
    """
    :param logits: the un-normalised predictions
    :param y_true: the true predictions (binary)
    :param k: cut-off value
    :param tids_path: path to the mapping to original track ids
    :param tracklist_path: path to (id -- track name) index. ("song_ids.txt")
    :return: Diversity based on Shannon entropy at k
    """
    # TODO: create general (track_id -- artist_id) index for speed and beauty
    
    n = logits.shape[0]
    m = logits.shape[1]
    dummy_column = np.arange(n).reshape(n, 1)
    
    # getting top k indicies
    idx_topk_part = bn.argpartition(-logits, k, axis=1)[:, :k]
    topk_part = logits[dummy_column, idx_topk_part]
    idx_part = np.argsort(-topk_part, axis=1)
    idx_topk = idx_topk_part[dummy_column, idx_part]
    
    # mapping back to original indicies
    tids = pd.read_csv(tids_path)
    
    orig_idx_topk = np.empty((0,k), int)
    for user in idx_topk:
        orig_idx_topk = np.append(orig_idx_topk, np.array([tids.loc[user]['track_id']]), axis=0)
    
    # retrieving track names
    track_names = pd.read_csv(tracklist_path, sep='\t', header=None)
    
    # # creating batch-local (track -- artist) index
    # # hoping to save a bit of time while calculating histograms for each user
    batch_recommended = set(np.concatenate(orig_idx_topk))
    artist_idx = track_names.loc[batch_recommended][0]
    
    res = np.array([])
    for user in orig_idx_topk:
        user_histogram = artist_idx.loc[user].value_counts() / k # WE EVALUATE TOP K RECOMMENDED
        user_entropy = -np.sum(user_histogram * np.log2(user_histogram))
        artist_range = len(user_histogram)
        if normalized and artist_range > 1:
            user_entropy = user_entropy/np.log2(artist_range)
        res = np.append(res, user_entropy)
    # pdb.set_trace()
    return res

def eval_proced(preds: np.ndarray, true: np.ndarray, tag: str, user_groups: List[UserGroup]):
    '''
    Performs the evaluation procedure.
    :param preds: predictions
    :param true: true values
    :param tag: should be either val or test
    :param user_groups: array of UserGroup objects. It is used to extract the results from preds and true.
    :return: eval_metric, value of the metric considered for validation purposes
            metrics, dictionary of the average metrics for all users, high group, and low group
            metrics_raw, dictionary of the metrics (not averaged) for high group, and low group
    '''

    assert tag in ['val', 'test'], "Tag can only be 'val' or 'test'!"
    #
    # Diversity_Shannon_at_k_batch(logits=preds, k=3,
    #                              tids_path="/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_regexp_since_2016_pc_gt_1_user_gte_5_song_gte_5/data/fold_n/sampled_1000_items_inter/0/new_tids.csv")
    #
    true = sp.csr_matrix(true)  # temporary #TODO: to remove
    
    metrics = dict()
    metrics_raw = dict()
    trait = user_groups[0].type
    for lev in LEVELS:
        for metric_name, metric in zip(['ndcg', 'recall'], [NDCG_binary_at_k_batch, Recall_binary_at_k_batch]):

            # Compute metrics for all users
            res = metric(preds, true, lev)
            metrics['{}/{}_at_{}'.format(tag, metric_name, lev)] = np.mean(res)
            metrics_raw['{}/{}_at_{}'.format(tag, metric_name, lev)] = res

            # Split the metrics on user group basis
            for user_group in user_groups:
                user_group_res = res[user_group.vd_idxs if tag == 'val' else user_group.te_idxs]
                metrics['{}/{}_{}/{}_at_{}'.format(tag, trait, user_group.name, metric_name, lev)] = np.mean(
                    user_group_res)
                metrics_raw['{}/{}_{}/{}_at_{}'.format(tag, trait, user_group.name, metric_name, lev)] = user_group_res

    eval_metric = metrics['{}/ndcg_at_50'.format(tag)]
    return eval_metric, metrics, metrics_raw


def eval_metric(preds: np.ndarray, true: np.ndarray, aggregated=True):
    '''
    Shorter version of the previous function. It only computes the evaluation on NDCG@50
    :param preds:
    :param true:
    :param aggregated: whether to compute the mean or not
    :return:
    '''
    true = sp.csr_matrix(true)  # temporary
    eval_metric_raw = NDCG_binary_at_k_batch(preds, true, 50)

    return np.mean(eval_metric_raw) if aggregated else eval_metric_raw


def top_k(arr, k):
    ## not used
    n = arr.shape[0]
    dummy_column = np.arange(n).reshape(n, 1)

    # Finding the indexes related to the top-k
    idx_tops = bn.argpartition(-arr, k, axis=1)[:, :k]
    # Extracting the unsorted top-k
    tops = arr[dummy_column, idx_tops]
    # Sorting these and extracting the indices (respect to the subset)
    idx_sorted_tops_sub = np.argsort(-tops, axis=1)
    # 1. Extracting the real indices of the sorted array from the real set
    idx_sorted_tops = idx_tops[dummy_column, idx_sorted_tops_sub]
    # 2. Extracting the real values associated to the real indicies
    sorted_tops = arr[dummy_column, idx_sorted_tops]
    return sorted_tops
