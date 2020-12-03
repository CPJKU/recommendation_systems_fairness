import argparse
import os

from datetime import datetime
from scipy import sparse as sp
from sklearn.model_selection import ParameterGrid
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm, trange

from algorithms.slim.slim_parallel import SLIM_parallel
from conf import LOG_VAL_STR, LOG_TE_STR, DATA_PATH, DEMO_PATH, OUT_DIR, DEMO_TRAITS, DOWN_DATA_PATH, \
    DOWN_DEMO_PATH
from utils.data_splitter import DataSplitter
from utils.eval import eval_metric
from utils.helper import pickle_dump

grid = {
    "alpha": [5e-1, 1e-1, 1e-2, 1e-3],
    "l1_ratio": [1e-1, 1e-2],
    "max_iter": [500]
}
pg = ParameterGrid(grid)

now = datetime.now()

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("--experiment_type", type=str, required=True, choices=['standard', 'up_sample', 'down_sample'])
    args = parser.parse_args()

    experiment_type = args.experiment_type

    print('STARTING EXPERIMENTS <{}> WITH SLIM'.format(experiment_type))
    data_path = DOWN_DATA_PATH if experiment_type == 'down_sample' else DATA_PATH
    demo_path = DOWN_DEMO_PATH if experiment_type == 'down_sample' else DEMO_PATH

    for fold_n in trange(5, desc='folds'):

        log_val_str = LOG_VAL_STR.format('slim', experiment_type, now, fold_n)
        log_te_str = LOG_TE_STR.format('slim', experiment_type, now, fold_n)

        ds = DataSplitter(data_path, demo_path, out_dir=OUT_DIR)
        pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(fold_n=fold_n)

        if experiment_type == 'up_sample':
            up_tr_data_path, up_sp_tr_data_path = ds.up_sample_train_data_path(pandas_dir_path, scipy_dir_path,
                                                                               'gender')

        # --- Data --- #
        sp_tr_data = sp.load_npz(
            up_sp_tr_data_path if experiment_type == 'up_sample' else os.path.join(scipy_dir_path, 'sp_tr_data.npz'))
        sp_vd_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_tr_data.npz'))
        sp_vd_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_te_data.npz'))
        sp_te_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_tr_data.npz'))
        sp_te_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_te_data.npz'))

        user_groups_all_traits = dict()
        for trait in DEMO_TRAITS:
            # This creates UserGroup object that store the index of the users belonging to the group in te and vd data.
            user_groups_all_traits[trait] = ds.get_user_groups_indxs(pandas_dir_path, trait)
        print("Data Loaded")

        # Stacking training data and validation training data
        A = sp.csc_matrix(sp.vstack((sp_vd_tr_data, sp_tr_data)))

        best_value = 0
        # Running Hyperparameter search
        for config in tqdm(pg, desc='configs'):

            summ = SummaryWriter(os.path.join(log_val_str, str(config)))

            W = SLIM_parallel(A, config['alpha'], config['l1_ratio'], config['max_iter'])

            Atild = sp.csr_matrix(A.dot(W))

            # Only focusing on validation data
            Atild = Atild[:sp_vd_tr_data.shape[0]]
            # Removing entries from training data
            Atild[sp_vd_tr_data.nonzero()] = .0

            preds = Atild.toarray()
            true = sp_vd_te_data.toarray()

            curr_value = eval_metric(preds, true)

            # Logging hyperparams and metrics
            summ.add_hparams({**config, 'fold_n': fold_n}, {'val/ndcg_50': curr_value})
            summ.flush()

            if curr_value > best_value:
                print('New best model found')
                best_value = curr_value

                pickle_dump(config, os.path.join(log_val_str, 'best_config.pkl'))

            # Logging hyperparams and metrics
            summ.add_hparams({**config, 'fold_n': fold_n}, {'val/ndcg_50': best_value})
            summ.flush()
