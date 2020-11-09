import os
import pickle
from datetime import datetime

from scipy import sparse as sp
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import ParameterGrid
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm

from algorithms.slim.slim import SLIM
from conf import SEEDS, TRAITS, LOG_VAL_STR, LOG_TE_STR, DATA_PATH, PERS_PATH, OUT_DIR
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced

print('STARTING CONTROLLED EXPERIMENTS WITH SLIM')
print('SEEDS ARE: {}'.format(SEEDS))

grid = {
    "alpha": [5e-1, 1e-1, 1e-2, 1e-3],
    "l1_ratio": [1, 1e-1, 1e-2],
    "max_iter": [500]
}
pg = ParameterGrid(grid)

now = datetime.now()

for trait in tqdm(TRAITS, desc='traits'):
    print('WORKING ON TRAIT: ' + trait)
    for seed in tqdm(SEEDS, desc='seeds'):

        log_val_str = LOG_VAL_STR.format('slim', now, trait, seed)
        log_te_str = LOG_TE_STR.format('slim', now, trait, seed)

        ds = DataSplitter(DATA_PATH, PERS_PATH, out_dir=OUT_DIR)
        pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(seed, trait)

        # Load data
        sp_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_tr_data.npz'))
        sp_vd_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_tr_data.npz'))
        sp_vd_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_vd_te_data.npz'))
        sp_te_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_tr_data.npz'))
        sp_te_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_te_data.npz'))

        vd_low_idxs, vd_high_idxs, te_low_idxs, te_high_idxs = ds.get_low_high_indxs(pandas_dir_path, uids_dic_path)
        print("Data Loaded")

        # Stacking training data and validation training data
        A = sp.csc_matrix(sp.vstack((sp_tr_data, sp_vd_tr_data)))

        best_value = 0
        best_config = None
        # Running Hyperparameter search
        for config in tqdm(pg, desc='configs'):
            elanet = ElasticNet(
                alpha=config["alpha"],
                l1_ratio=config["l1_ratio"],
                fit_intercept=False,  # Not considered by SLIM
                positive=True,  # Constraint in SLIM
                copy_X=False,  # efficiency reasons
                max_iter=config["max_iter"],
                selection="random",  # efficiency reasons
                tol=1e-4  # assuming a good tolerance
            )
            W = SLIM(A, elanet)

            Atild = sp.csr_matrix(A.dot(W))

            # Only focusing on validation data
            Atild = Atild[sp_tr_data.shape[0]:, :]
            # Removing entries from training data
            Atild[sp_vd_tr_data.nonzero()] = .0

            preds = Atild.toarray()
            true = sp_vd_te_data.toarray()

            summ = SummaryWriter(log_val_str)

            # Compute metrics at different thresholds
            val_metric, metrics, _ = eval_proced(preds, true, vd_high_idxs, vd_low_idxs, 'val')

            if val_metric > best_value:
                print('New best model found')
                best_value = val_metric
                best_config = config

            # Logging hyperparams and metrics
            hparams = {**config, 'trait': trait, 'seed': seed}
            summ.add_hparams(hparams, metrics)
            summ.flush()

        # Testing the best configuration
        A_test = sp.csc_matrix(sp.vstack((sp_tr_data, sp_te_tr_data)))
        elanet = ElasticNet(
            alpha=best_config["alpha"],
            l1_ratio=best_config["l1_ratio"],
            fit_intercept=False,  # Not considered by SLIM
            positive=True,  # Constraints in SLIM
            copy_X=False,  # efficiency reasons
            max_iter=best_config["max_iter"],
            selection="random",  # efficiency reasons
            tol=1e-4  # assuming a good tolerance
        )
        W_test = SLIM(A_test, elanet)

        Atild_test = sp.csr_matrix(A_test.dot(W_test))
        Atild_test = Atild_test[sp_tr_data.shape[0]:, :]
        # Removing entries from training data
        Atild_test[sp_te_tr_data.nonzero()] = .0

        preds = Atild_test.toarray()
        true = sp_te_te_data.toarray()

        summ = SummaryWriter(log_te_str)

        _, metrics, metrics_raw = eval_proced(preds, true, te_high_idxs, te_low_idxs, 'test')

        # Logging hyperparams and metrics
        hparams = {**best_config, 'trait': trait, 'seed': seed}

        summ.add_hparams(hparams, metrics)
        summ.flush()

        # Saving the best_config and results
        pickle.dump(hparams, open(os.path.join(log_te_str, 'best_config.pkl'), 'wb'))
        pickle.dump(metrics_raw, open(os.path.join(log_te_str, 'metrics_raw.pkl'), 'wb'))
