import os

from datetime import datetime
from scipy import sparse as sp
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import ParameterGrid
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm, trange

from algorithms.slim.slim import SLIM
from conf import UN_LOG_VAL_STR, UN_LOG_TE_STR, DATA_PATH, DEMO_PATH, UN_OUT_DIR, DEMO_TRAITS
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced, eval_metric
from utils.helper import pickle_dump, pickle_load

print('STARTING UNCONTROLLED EXPERIMENTS WITH SLIM')

grid = {
    "alpha": [5e-1, 1e-1, 1e-2, 1e-3],
    "l1_ratio": [1e-1, 1e-2],
    "max_iter": [500]
}
pg = ParameterGrid(grid)

now = datetime.now()

for fold_n in trange(5, desc='folds'):

    log_val_str = UN_LOG_VAL_STR.format('slim', now, fold_n)
    log_te_str = UN_LOG_TE_STR.format('slim', now, fold_n)

    ds = DataSplitter(DATA_PATH, DEMO_PATH, out_dir=UN_OUT_DIR)
    pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(fold_n=fold_n)

    # --- Data --- #
    sp_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_tr_data.npz'))
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
    A = sp.csc_matrix(sp.vstack((sp_tr_data, sp_vd_tr_data)))

    best_value = 0
    # Running Hyperparameter search
    for config in tqdm(pg, desc='configs'):

        summ = SummaryWriter(os.path.join(log_val_str, str(config)))

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

        curr_value = eval_metric(preds, true)

        if curr_value > best_value:
            print('New best model found')
            best_value = curr_value

            pickle_dump(config, os.path.join(log_val_str, 'best_config.pkl'))

        # Logging hyperparams and metrics
        summ.add_hparams({**config, 'fold_n': fold_n}, {'val/ndcg_50': best_value})
        summ.flush()

    # --- Test --- #

    summ = SummaryWriter(log_te_str)

    best_config = pickle_load(os.path.join(log_val_str, 'best_config.pkl'))

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

    full_metrics = dict()
    full_raw_metrics = dict()
    for trait in DEMO_TRAITS:
        user_groups = user_groups_all_traits[trait]
        _, metrics, metrics_raw = eval_proced(preds, true, 'test', user_groups)
        full_metrics.update(metrics)
        full_raw_metrics.update(metrics_raw)

    # Logging hyperparams and metrics
    summ.add_hparams({**best_config, 'fold_n': fold_n}, full_metrics)
    summ.flush()

    # Saving results
    pickle_dump(full_metrics, os.path.join(log_te_str, 'full_metrics.pkl'))
    pickle_dump(full_raw_metrics, os.path.join(log_te_str, 'full_raw_metrics.pkl'))
