import os
import pickle

import numpy as np
import pandas as pd

from utils.helper import idx_split, filt, random_item_splitter, playcounts, permute, sparsify, save_data


class UserGroup:
    '''
    Holds information of a specific user group such as name of the group and the user ids
    '''

    def __init__(self, type: str, name: str, uids: np.ndarray):
        self.type = type
        self.name = name
        self.uids = uids
        self.te_indxs = None
        self.vd_indxs = None


class DataSplitter:
    '''
    Class used to generated the dataset in the following steps:
    - Splits the users in training (80%), validation (10%), and test (10%)
    - Filters the data (tracks not in the training data are discarded, users with fewer than 5 LEs are also removed)
    - Generates the playcount for each user
    - For validation and test users, the tracks listened are partitioned in training items (80%) and test items (20%)

    It can also generated a controlled splitting where the ratio of high vs low trait classes is equal in train, val,
    test sets.
    '''
    INF_STR = "{:10d} entries {:7d} users {:7d} items for {} data"
    LST_STR = "{:10d} entries {:7d} users {:7d} items (lost)"
    DIR_NAME = "{}/{}"
    DIR_TR_NAME = "{}/{}/{}"

    def __init__(self, data_path, demo_path=None, out_dir='../../data/', perc_train=80):
        '''
        :param data_path: path to the file containing interactions user_id, track_id
        :param demo_path: path to the personality file containing user_id,ope,con,ext,agr,neu
        :param out_dir: path where the generated dataset is saved
        :param perc_train: % of training users
        '''
        self.data_path = data_path
        self.demo_path = demo_path

        self.inter = pd.read_csv(self.data_path, sep='\t', names=['user_id', 'track_id', 'play_count'])[
            ['user_id', 'track_id']]

        if self.demo_path:
            self.demo = pd.read_csv(self.demo_path, sep='\t',
                                    names=['user_name', 'country', 'age', 'gender', 'timestamp'])

        self.out_dir = out_dir

        self.n_users = self.inter.user_id.nunique()
        self.n_items = self.inter.track_id.nunique()

        self.n_train = (self.n_users * perc_train) // 100
        self.n_heldout = (self.n_users - self.n_train) // 2

    def _split(self, tr_uids: np.ndarray, vd_uids: np.ndarray, te_uids: np.ndarray):
        '''
        Internal method for the splitting procedure
        '''

        # Extract data
        tr_data = self.inter[self.inter.user_id.isin(tr_uids)]
        print(DataSplitter.INF_STR.format(len(tr_data), tr_data.user_id.nunique(), tr_data.track_id.nunique(),
                                          "Training"))
        # Only tracks in the training data are considered
        tids = tr_data.track_id.drop_duplicates().values
        self.n_items = len(tids)

        vd_data = self.inter[self.inter.user_id.isin(vd_uids)]
        vd_data, num_, usr_, its_ = filt(vd_data, tids)
        print(DataSplitter.INF_STR.format(len(vd_data), vd_data.user_id.nunique(), vd_data.track_id.nunique(),
                                          "Validation"))
        print(DataSplitter.LST_STR.format(num_ - len(vd_data), usr_ - vd_data.user_id.nunique(),
                                          its_ - vd_data.track_id.nunique()))

        te_data = self.inter[self.inter.user_id.isin(te_uids)]
        te_data, num_, usr_, its_ = filt(te_data, tids)
        print(DataSplitter.INF_STR.format(len(te_data), te_data.user_id.nunique(), te_data.track_id.nunique(),
                                          "Test"))
        print(DataSplitter.LST_STR.format(num_ - len(te_data), usr_ - te_data.user_id.nunique(),
                                          its_ - te_data.track_id.nunique()))

        # Re-indexing for the track_id
        new_tids = tr_data.track_id.drop_duplicates().sort_values().reset_index(drop=True).reset_index().rename(
            columns={"index": "new_track_id"})

        tr_data = tr_data.merge(new_tids).drop(columns="track_id")
        vd_data = vd_data.merge(new_tids).drop(columns="track_id")
        te_data = te_data.merge(new_tids).drop(columns="track_id")

        # Generates playcounts
        tr_data = playcounts(tr_data)
        vd_data = playcounts(vd_data)
        te_data = playcounts(te_data)

        # Splitting item data
        vd_tr_data, vd_te_data = random_item_splitter(vd_data)
        te_tr_data, te_te_data = random_item_splitter(te_data)

        sp_tr_data, tr_data = sparsify(tr_data, self.n_items)
        sp_vd_data, vd_data = sparsify(vd_data, self.n_items)
        sp_te_data, te_data = sparsify(te_data, self.n_items)
        sp_vd_tr_data, vd_tr_data = sparsify(vd_tr_data, self.n_items)
        sp_vd_te_data, vd_te_data = sparsify(vd_te_data, self.n_items)
        sp_te_tr_data, te_tr_data = sparsify(te_tr_data, self.n_items)
        sp_te_te_data, te_te_data = sparsify(te_te_data, self.n_items)

        pandas_data = {
            'tr_data': tr_data,
            'vd_data': vd_data,
            'vd_tr_data': vd_tr_data,
            'vd_te_data': vd_te_data,
            'te_data': te_data,
            'te_tr_data': te_tr_data,
            'te_te_data': te_te_data
        }

        scipy_data = {
            'sp_tr_data': sp_tr_data,
            'sp_vd_data': sp_vd_data,
            'sp_vd_tr_data': sp_vd_tr_data,
            'sp_vd_te_data': sp_vd_te_data,
            'sp_te_data': sp_te_data,
            'sp_te_tr_data': sp_te_tr_data,
            'sp_te_te_data': sp_te_te_data,
        }

        return pandas_data, scipy_data, new_tids

    def split(self, seed: int):
        '''
        Main splitting procedure. Users are sampled at random.
        '''

        np.random.seed(seed)

        # Extract user_ids
        uids = self.inter.user_id.drop_duplicates().values

        # Permute array
        uids = permute(uids)

        # Split user ids
        tr_uids, vd_uids, te_uids = idx_split(uids, self.n_train, self.n_heldout)

        pandas_data, scipy_data, new_tids = self._split(tr_uids, vd_uids, te_uids)

        # Saving data
        dir_name = DataSplitter.DIR_NAME.format(os.path.basename(self.data_path).split('.')[0], seed)
        dir_path = os.path.join(self.out_dir, dir_name)
        pandas_dir_path, scipy_dir_path, tids_path = save_data(dir_path, pandas_data, scipy_data, new_tids)

        # Saving uids
        uids_dic = {
            'tr_uids': tr_uids,
            'vd_uids': vd_uids,
            'te_uids': te_uids
        }
        uids_dic_path = os.path.join(dir_path, 'uids_dic.pkl')
        pickle.dump(uids_dic, open(uids_dic_path, 'wb'))

        return pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path

    def trait_split(self, seed: int, demo_trait: str):
        '''
        Users are sampled equally within the specified category
        TODO: NOT YET FINISHED
        '''
        np.random.seed(seed)

        # Extract low and high user_ids

        low_uids, high_uids, = self.get_low_high_uids(demo_trait)

        # Permute arrays
        low_uids = permute(low_uids)
        high_uids = permute(high_uids)

        # Divide the number of train and heldout users by two (will be merged later)
        red_n_train = self.n_train // 2
        red_n_heldout = self.n_heldout // 2

        # Split user ids within the group
        tr_low_uids, vd_low_uids, te_low_uids = idx_split(low_uids, red_n_train, red_n_heldout)
        tr_high_uids, vd_high_uids, te_high_uids = idx_split(high_uids, red_n_train, red_n_heldout)

        # Putting back together
        tr_uids = np.concatenate((tr_low_uids, tr_high_uids))
        vd_uids = np.concatenate((vd_low_uids, vd_high_uids))
        te_uids = np.concatenate((te_low_uids, te_high_uids))

        pandas_data, scipy_data, new_tids = self._split(tr_uids, vd_uids, te_uids)

        # Saving data
        dir_name = DataSplitter.DIR_TR_NAME.format(os.path.basename(self.data_path).split('.')[0], demo_trait, seed)
        dir_path = os.path.join(self.out_dir, dir_name)
        pandas_dir_path, scipy_dir_path, tids_path = save_data(dir_path, pandas_data, scipy_data, new_tids)

        # Saving uids
        uids_dic = {
            'tr_low_uids': tr_low_uids,
            'tr_high_uids': tr_high_uids,
            'vd_low_uids': vd_low_uids,
            'vd_high_uids': vd_high_uids,
            'te_low_uids': te_low_uids,
            'te_high_uids': te_high_uids
        }
        uids_dic_path = os.path.join(dir_path, 'uids_dic.pkl')
        pickle.dump(uids_dic, open(uids_dic_path, 'wb'))

        return pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path

    def get_paths(self, seed: int, trait: str = None):
        '''
        Returns the dataset given a seed. If trait is specified, then it creates balanced splits where there will be an
        equal ratio of high vs low users (for that trait) in each set (train, validation, and test).

        :param seed: random seed for the splits
        :param trait: one in ['ope','con','ext','agr','neu']
        :return: paths to the data
        '''

        if trait:
            dir_name = DataSplitter.DIR_TR_NAME.format(os.path.basename(self.data_path).split('.')[0], trait, seed)
        else:
            dir_name = DataSplitter.DIR_NAME.format(os.path.basename(self.data_path).split('.')[0], seed)

        dir_path = os.path.join(self.out_dir, dir_name)

        # Assuming that if the main dir exists, all files will be present
        if os.path.isdir(dir_path):
            pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = os.path.join(dir_path, "pandas/"), os.path.join(
                dir_path, "scipy/"), os.path.join(dir_path, 'uids_dic.pkl'), os.path.join(dir_path, 'new_tids.csv')
        else:
            print("Data not found, generating new split")
            if trait:
                print("Seed: {:10d} Trait: {}".format(seed, trait))
                pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = self.trait_split(seed, trait)
            else:
                print("Seed: {:10d}".format(seed))
                pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = self.split(seed)

        # Reads new_tids.csv in order to update the number of items
        self.n_items = len(pd.read_csv(tids_path)['new_track_id'])

        return pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path

    def get_user_groups(self, demo_trait: str):

        if not hasattr(self, "demo"):
            raise Exception("No path to user demographic file!")

        # Split by gender
        if demo_trait == 'gender':
            m_uids = self.demo[self.demo.gender == 'm'].index.values  # TODO: Are these the same user ids?
            f_uids = self.demo[self.demo.gender == 'f'].index.values
            return [UserGroup('gender', 'm', m_uids), UserGroup('gender', 'f', f_uids)]
        else:
            raise ValueError('Demographic trait not yet implemented')

    def get_user_groups_indxs(self, pandas_dir_path: str, uids_dic_path: str, demo_trait=None):
        '''
        :param pandas_dir_path:
        :param uids_dic_path:
        :param demo_trait:
        :return:
        '''

        vd_data = pd.read_csv(os.path.join(pandas_dir_path, 'vd_data.csv'))[
            ['user_id', 'new_user_id']].drop_duplicates()
        te_data = pd.read_csv(os.path.join(pandas_dir_path, 'te_data.csv'))[
            ['user_id', 'new_user_id']].drop_duplicates()

        # uids_dic = pickle.load(open(uids_dic_path, 'rb'))

        if demo_trait:
            user_groups = self.get_user_groups(demo_trait)
            for user_group in user_groups:
                user_group.vd_indxs = vd_data[
                    vd_data.user_id.isin(set(vd_data.user_id).intersection(user_group.uids))].new_user_id.values
                user_group.te_indxs = te_data[
                    te_data.user_id.isin(set(te_data.user_id).intersection(user_group.uids))].new_user_id.values
        else:
            raise ValueError('demo_trait has to be non-null. Not yet implemented!')

        return user_groups
