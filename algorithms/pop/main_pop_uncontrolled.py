import os

from datetime import datetime
from scipy import sparse as sp
from torch.utils.tensorboard import SummaryWriter
from tqdm import trange

from algorithms.pop.pop import PopularItems
from conf import UN_LOG_VAL_STR, UN_LOG_TE_STR, DATA_PATH, DEMO_PATH, UN_OUT_DIR, DEMO_TRAITS
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced, eval_metric
from utils.helper import pickle_dump

print('STARTING UNCONTROLLED EXPERIMENTS WITH POP')

now = datetime.now()

for fold_n in trange(5, desc='folds'):

    log_val_str = UN_LOG_VAL_STR.format('pop', now, os.path.basename(DATA_PATH), fold_n)
    log_te_str = UN_LOG_TE_STR.format('pop', now, os.path.basename(DATA_PATH), fold_n)

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
    A = sp.csc_matrix(sp.vstack((sp_vd_tr_data, sp_tr_data)))

    summ = SummaryWriter(os.path.join(log_val_str))

    Atild = PopularItems(A)

    # Only focusing on validation data
    Atild = Atild[:sp_vd_tr_data.shape[0]]
    # Removing entries from training data
    Atild[sp_vd_tr_data.nonzero()] = .0

    preds = Atild.toarray()
    true = sp_vd_te_data.toarray()

    # There is no validation here
    best_value = eval_metric(preds, true)

    # Logging hyperparams and metrics
    summ.add_hparams({'fold_n': fold_n}, {'val/ndcg_50': best_value})
    summ.flush()

    # --- Test --- #

    summ = SummaryWriter(log_te_str)

    A_test = sp.csc_matrix(sp.vstack((sp_te_tr_data, sp_tr_data)))

    Atild_test = PopularItems(A)

    # Only focusing on test data
    Atild_test = Atild_test[:sp_te_tr_data.shape[0]]
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
    summ.add_hparams({'fold_n': fold_n}, full_metrics)
    summ.flush()

    # Saving results and predictions
    pickle_dump(full_metrics, os.path.join(log_te_str, 'full_metrics.pkl'))
    pickle_dump(full_raw_metrics, os.path.join(log_te_str, 'full_raw_metrics.pkl'))
