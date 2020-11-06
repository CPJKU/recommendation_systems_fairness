import os
import pickle

import pandas as pd
from scipy import sparse as sp
from sklearn.linear_model import ElasticNet
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm

from algorithms.slim.slim import SLIM
from conf import LOG_TE_STR, DATA_PATH, PERS_PATH, OUT_DIR
from utils.data_splitter import DataSplitter, get_high_low_indxs
from utils.eval import eval_proced

best_configs = pd.read_csv("./hparams_table(SLIM).csv", usecols=range(5), dtype={'seed': 'int64'})

experiment_now = '2020-05-19 17:55:54.113112'

for _, best_config in tqdm(best_configs.iterrows()):
    trait = best_config.trait
    seed = best_config.seed

    log_te_str = LOG_TE_STR.format('slim', experiment_now, trait, seed)

    ds = DataSplitter(DATA_PATH, PERS_PATH, out_dir=OUT_DIR)
    pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths_trait(seed, trait)

    # Load data
    sp_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_tr_data.npz'))
    sp_te_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_tr_data.npz'))
    sp_te_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_te_data.npz'))

    _, _, te_low_idxs, te_high_idxs = get_high_low_indxs(pandas_dir_path, uids_dic_path)

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

    # Saving the results
    pickle.dump(metrics_raw, open(os.path.join(log_te_str, 'metrics_raw.pkl'), 'wb'))
