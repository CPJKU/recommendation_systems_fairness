import os, pdb
from datetime import datetime

from scipy import sparse as sp
import numpy as np
from sklearn.model_selection import ParameterGrid
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm, trange
import pandas as pd


from conf import UN_LOG_VAL_STR, UN_LOG_TE_STR, DATA_PATH, DEMO_PATH, UN_OUT_DIR, DEMO_TRAITS
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced, eval_metric
from utils.helper import pickle_dump, pickle_load

from algorithms.knnCF.knn import KNNCF
from algorithms.knnCF.KNNCFRecommender import ItemKNNCF


def predict(_model, _user_items):
    print ("Prediction running ...")
    
    scores = []
    _user_cnt = _user_items.shape[0]
    _item_cnt = _user_items.shape[1]
    for _user_i in range(_user_cnt):
        _u_res_vec = np.zeros(_item_cnt)
        for _item_i in range(_item_cnt):
            _u_res_vec[_item_i] = _model.predict(_user_i, _item_i)
        scores.append(_u_res_vec)
    scores = np.array(scores)
    
    print ("Prediction done!")
    
    return scores


print('STARTING UNCONTROLLED EXPERIMENTS WITH ALS')

grid = {
    "maxk": [10, 20, 40, 80]
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

        model = ItemKNNCF(user_num=A.shape[0], item_num=A.shape[1], 
                          maxk=config["maxk"], shrink=100, similarity='cosine', normalize=True)
    
        model = KNNCF(A, model)

        preds = predict(model, sp_vd_tr_data)
        
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

    A_test = sp.csc_matrix(sp.vstack((sp_te_tr_data, sp_tr_data)))
    
    model = ItemKNNCF(user_num=A_test.shape[0], item_num=A_test.shape[1], 
                      maxk=best_config["maxk"], shrink=100, similarity='cosine', normalize=True)
    
    model = KNNCF(A_test, model)
        
    preds = predict(model, sp_te_tr_data)

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
    
