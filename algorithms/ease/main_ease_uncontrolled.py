import os
import pickle
import sys
from datetime import datetime

from scipy import sparse as sp
from sklearn.model_selection import ParameterGrid
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm

sys.path.append(os.path.abspath('../../'))
from algorithms.ease.ease import EASE
from conf import UN_SEEDS, TRAITS, UN_LOG_VAL_STR, UN_LOG_TE_STR, DATA_PATH, PERS_PATH, UN_OUT_DIR
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

    ds = DataSplitter(DATA_PATH, PERS_PATH, out_dir=UN_OUT_DIR)
    pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(seed)

    # Load data
    sp_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_tr_data.npz'))
    sp_vd_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_tr_data.npz'))
    sp_vd_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_te_data.npz'))
    sp_te_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_tr_data.npz'))
    sp_te_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_te_data.npz'))

    low_high_indxs = dict()
    for tr in TRAITS:
        # This assigns a 4-entry tuple (vd_low_idxs, vd_high_idxs, te_low_idxs, te_high_idxs)
        low_high_indxs[tr] = ds.get_low_high_indxs(pandas_dir_path, uids_dic_path, tr)

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

        # Compute metrics at different thresholds
        full_metrics = dict()
        val_metric = None
        for trait in TRAITS:
            vd_low_idxs, vd_high_idxs, _, _ = low_high_indxs[trait]
            val_metric, metrics, _ = eval_proced(preds, true, vd_high_idxs, vd_low_idxs, 'val')
            # Changing the tag for some metrics
            metrics = {k if ('high' not in k and 'low' not in k) else k[:4] + trait + '_' + k[4:]: v for k, v in
                       metrics.items()}
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
    for trait in TRAITS:
        _, _, te_low_idxs, te_high_idxs = low_high_indxs[trait]
        _, metrics, metrics_raw = eval_proced(preds, true, te_high_idxs, te_low_idxs, 'test')
        # Changing the tag for some metrics
        metrics = {k if ('high' not in k and 'low' not in k) else k[:5] + trait + '_' + k[5:]: v for k, v in
                   metrics.items()}
        metrics_raw = {k if ('high' not in k and 'low' not in k) else trait + '_' + k: v for k, v in
                       metrics_raw.items()}
        full_metrics.update(metrics)
        full_raw_metrics.update(metrics_raw)

    # Logging hyperparams and metrics
    hparams = {**best_config, 'seed': seed}

    summ.add_hparams(hparams, full_metrics)
    summ.flush()

    # Saving the best_config and results
    pickle.dump(hparams, open(os.path.join(log_te_str, 'best_config.pkl'), 'wb'))
    pickle.dump(full_raw_metrics, open(os.path.join(log_te_str, 'metrics_raw.pkl'), 'wb'))
