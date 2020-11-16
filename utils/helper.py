import os
import random

import numpy as np
import pandas as pd
import pathlib
import torch
from scipy import sparse as sp


def permute(arr: np.ndarray):
    idx_perm = np.random.permutation(arr.size)
    return arr[idx_perm]


def idx_split(arr: np.ndarray, n_train: int, n_heldout: int):
    '''
    Splits an array in training, validation, and test set with n_train, n_heldout, and n_heldout entries respectively.
    (negligible 1 or 2 entries differences may exist)

    :param arr: array to be split in three parts
    :param n_train: number of training entries
    :param n_heldout: number of validation entries (= number of test entries)
    :return: tr, vd, te split arrays
    '''

    # Ensure that the total amount of entries is more or less equal to the size of the array
    assert abs((n_train + 2 * n_heldout) - arr.size) <= 2
    tr = arr[: n_train]
    vd = arr[n_train: n_train + n_heldout]
    te = arr[n_train + n_heldout:]
    return tr, vd, te


def filt(df: pd.DataFrame, tids: np.ndarray):
    '''
    Filters validation and test data by discarding track ids (tids) not present in the training data.
    It also discards users with fewer than 5 interactions (since they cannot be split in a 80-20% fashion)
    :param df: pd DataFrame of validation data or test data
    :param tids: list of track ids present in the training data
    :return: the filtered dataframe + temp information for logging
    '''
    num_, usr_, trk_ = len(df), df.user_id.nunique(), df.track_id.nunique()
    df = df[df.track_id.isin(set(tids))]
    df = df.groupby("user_id").filter(lambda x: len(x.drop_duplicates()) >= 5)
    return df, num_, usr_, trk_


def playcounts(df: pd.DataFrame):
    '''
    Generates the playcount column
    :param df: either the train,val,test dataframe containing the interaction data
    :return: the original dataframe with the playcount column appended
    '''
    df["play_count"] = 1

    # Aggregating
    df = df.groupby(["user_id", "new_track_id"]).count().reset_index()
    return df


def random_item_splitter(df: pd.DataFrame):
    '''
    Performs the 80-20% item split. For a specific user (either in the validation or test set), it randomly
    picks 80% of the listened items as training and places the remaining 20% in the test set
    :param df: either validation of test dataframe containing the interaction data
    :return: two dataframes containing the interaction data for training and test data (e.g. vdalidation training and validation testing)
    '''
    grouped = df.groupby("user_id")

    tr_idxs, te_idxs = list(), list()

    for i, (_, group) in enumerate(grouped):
        # Fixed at 80-20 split
        if len(group) >= 5:
            # Sampling test items
            test = group.sample(frac=0.2, replace=False).index

            # Training is the leftover data
            train = group[~group.index.isin(set(test))].index

            tr_idxs += train.tolist()
            te_idxs += test.tolist()
        else:
            raise Exception("A user has fewer than 5 different tracks")

    tr_data = df.loc[tr_idxs]
    te_data = df.loc[te_idxs]

    return tr_data, te_data


def sparsify(df: pd.DataFrame, n_tracks: int):
    '''
    Creates a sparse representation of the data. To do so, it also assigns a new user_id depending on the position.

    :param df: pandas DataFrame containing the interaction data
    :return: sparse csr matrix of the data and the original dataframe with the new_user_id column
    '''
    # Assign new user id for position within the matrix
    df = df.merge(
        df.user_id.drop_duplicates().reset_index(drop=True).reset_index().rename(columns={"index": "new_user_id"}))
    sp_df = sp.csr_matrix((df.play_count, (df.new_user_id, df.new_track_id)),
                          shape=(df.new_user_id.max() + 1, n_tracks))
    return sp_df, df


def save_data(dir_path: str, pandas_data: dict, scipy_data: dict, new_tids: pd.DataFrame):
    pandas_dir_path = os.path.join(dir_path, "pandas/")
    scipy_dir_path = os.path.join(dir_path, "scipy/")

    pathlib.Path(pandas_dir_path).mkdir(parents=True, exist_ok=True)
    pathlib.Path(scipy_dir_path).mkdir(parents=True, exist_ok=True)

    # Saving pandas data
    for file_name, df in pandas_data.items():
        file_path = os.path.join(pandas_dir_path, file_name + '.csv')
        df.to_csv(file_path, index=False)

    # Saving scipy data
    for file_name, sps in scipy_data.items():
        file_path = os.path.join(scipy_dir_path, file_name + '.npz')
        sp.save_npz(file_path, sps)

    # Saving new_tids mapping
    tids_path = os.path.join(dir_path, 'new_tids.csv')
    new_tids.to_csv(tids_path, index=False)

    return pandas_dir_path, scipy_dir_path, tids_path


def reproducible(seed: int):
    random.seed(seed)
    torch.manual_seed(seed)
    np.random.seed(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False
