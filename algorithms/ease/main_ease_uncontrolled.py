import os
import pickle

from datetime import datetime
from scipy import sparse as sp
from sklearn.model_selection import ParameterGrid
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm

from algorithms.ease.ease import EASE
from conf import UN_SEEDS, UN_LOG_VAL_STR, UN_LOG_TE_STR, DATA_PATH, DEMO_PATH, UN_OUT_DIR, DEMO_TRAITS
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced

print('STARTING UNCONTROLLED EXPERIMENTS WITH EASE')
print('SEEDS ARE: {}'.format(UN_SEEDS))

grid = {
    'lam': [1, 1e1, 1e2, 5e2, 1e3, 1e4, 1e5, 1e6, 1e7]
}
pg = ParameterGrid(grid)

now = datetime.now()

for seed in tqdm(UN_SEEDS, desc='seeds'):

    log_val_str = UN_LOG_VAL_STR.format('ease', now, seed)
    log_te_str = UN_LOG_TE_STR.format('ease', now, seed)

    ds = DataSplitter(DATA_PATH, DEMO_PATH, out_dir=UN_OUT_DIR)
    pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(seed)

    # Load data
    sp_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_tr_data.npz'))
    sp_vd_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_tr_data.npz'))
    sp_vd_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_te_data.npz'))
    sp_te_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_tr_data.npz'))
    sp_te_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_te_data.npz'))

    user_groups_all_traits = dict()
    for trait in DEMO_TRAITS:
        # This creates UserGroup object that store the index of the users belonging to the group in te and vd data.
        user_groups_all_traits[trait] = ds.get_user_groups_indxs(pandas_dir_path, uids_dic_path, trait)
    print("Data Loaded")

    # Stacking training data and validation training data
    A = sp.csr_matrix(sp.vstack((sp_tr_data, sp_vd_tr_data)))

    best_value = 0
    best_config = None
    # Running Hyperparameter search
    for config in tqdm(pg, desc='configs'):
        B = EASE(A, config['lam'])

        Atild = sp.csr_matrix(A.dot(B))

        # Only focusing on validation data
        Atild = Atild[sp_tr_data.shape[0]:, :]
        # Removing entries from training data
        Atild[sp_vd_tr_data.nonzero()] = .0

        preds = Atild.toarray()
        true = sp_vd_te_data.toarray()

        summ = SummaryWriter(log_val_str)

        full_metrics = dict()
        val_metric = None
        for trait in DEMO_TRAITS:
            user_groups = user_groups_all_traits[trait]
            eval_metric, metrics, _ = eval_proced(preds, true, 'val', user_groups)

            val_metric = eval_metric  # ndcg@50 is used, regardless of the UserGroup.
            full_metrics.update(metrics)

        if val_metric > best_value:
            print('New best model found')
            best_value = val_metric
            best_config = config

        # Logging hyperparams and metrics
        hparams = {**config, 'seed': seed}
        summ.add_hparams(hparams, full_metrics)
        summ.flush()

    # Testing the best configuration
    A_test = sp.csr_matrix(sp.vstack((sp_tr_data, sp_te_tr_data)))
    B_test = EASE(A_test, best_config['lam'])

    Atild_test = sp.csr_matrix(A_test.dot(B_test))
    Atild_test = Atild_test[sp_tr_data.shape[0]:, :]
    # Removing entries from training data
    Atild_test[sp_te_tr_data.nonzero()] = .0

    preds = Atild_test.toarray()
    true = sp_te_te_data.toarray()

    summ = SummaryWriter(log_te_str)

    full_metrics = dict()
    full_raw_metrics = dict()
    for trait in DEMO_TRAITS:
        user_groups = user_groups_all_traits[trait]
        _, metrics, metrics_raw = eval_proced(preds, true, 'test', user_groups)
        full_metrics.update(metrics)
        full_raw_metrics.update(metrics_raw)

    # Logging hyperparams and metrics
    hparams = {**best_config, 'seed': seed}

    summ.add_hparams(hparams, full_metrics)
    summ.flush()

    # Saving the best_config and results
    pickle.dump(hparams, open(os.path.join(log_te_str, 'best_config.pkl'), 'wb'))
    pickle.dump(full_raw_metrics, open(os.path.join(log_te_str, 'metrics_raw.pkl'), 'wb'))
