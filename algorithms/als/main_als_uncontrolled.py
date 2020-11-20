import os, pdb
from datetime import datetime

from scipy import sparse as sp
import numpy as np
from sklearn.linear_model import ElasticNet
from sklearn.model_selection import ParameterGrid
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm, trange

from algorithms.als.als import ALS
import implicit

from conf import UN_LOG_VAL_STR, UN_LOG_TE_STR, DATA_PATH, DEMO_PATH, UN_OUT_DIR, DEMO_TRAITS
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced, eval_metric
from utils.helper import pickle_dump, pickle_load


print('STARTING UNCONTROLLED EXPERIMENTS WITH ALS')

grid = {
    "factors": [64, 128, 256],
    "iterations": [15, 30, 60],
    "regularization": [0.01, 0.05, 0.001]
}
pg = ParameterGrid(grid)

now = datetime.now()

for fold_n in trange(5, desc='folds'):

    log_val_str = UN_LOG_VAL_STR.format('als', now, fold_n)
    log_te_str = UN_LOG_TE_STR.format('als', now, fold_n)

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

    A = sp.csc_matrix(sp.vstack((sp_vd_tr_data, sp_tr_data)))
    
    best_value = 0
    # Running Hyperparameter search
    for config in pg:#tqdm(pg, desc='configs'):

        summ = SummaryWriter(os.path.join(log_val_str, str(config)))


        model = implicit.als.AlternatingLeastSquares(factors=config["factors"], 
                                                     regularization=config["regularization"],
                                                     iterations=config["iterations"])
    
        model = ALS(A, model)
        
        _predict_user_ids = list(range(sp_vd_tr_data.shape[0]))
        _user_factors_set = model.user_factors[_predict_user_ids]
        preds = _user_factors_set.dot(model.item_factors.T)
    
        true = sp_vd_te_data.toarray()

        curr_value = eval_metric(preds, true)

        if curr_value > best_value:
            print('New best model found')
            best_value = curr_value

            pickle_dump(config, os.path.join(log_val_str, 'best_config_fold.pkl'))

        # Logging hyperparams and metrics
        summ.add_hparams({**config, 'fold_n': fold_n}, {'val/ndcg_50': best_value})
        summ.flush()

    # --- Test --- #
    print ("*** Test running ... ***")
    
    summ = SummaryWriter(log_te_str)

    
    best_config = pickle_load(os.path.join(log_val_str, 'best_config_fold.pkl'))

    model = implicit.als.AlternatingLeastSquares(factors=best_config["factors"], 
                                                 regularization=best_config["regularization"],
                                                 iterations=best_config["iterations"])
    A_test = sp.csc_matrix(sp.vstack((sp_te_tr_data, sp_tr_data)))
    
    model = ALS(A_test, model)
        
    _predict_user_ids = list(range(sp_te_tr_data.shape[0]))
    _user_factors_set = model.user_factors[_predict_user_ids]
    preds = _user_factors_set.dot(model.item_factors.T)

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

    print ("*** Test finished! ***")
    
