import argparse
import os

from scipy import sparse as sp
from torch.utils.tensorboard import SummaryWriter
from tqdm import trange

from algorithms.knn.knn import ItemKNN
from conf import LOG_VAL_STR, LOG_TE_STR, DATA_PATH, DEMO_PATH, OUT_DIR, DEMO_TRAITS, DOWN_DEMO_PATH, DOWN_DATA_PATH
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced
from utils.helper import pickle_dump, pickle_load

best_configs = {
    'standard': '2020-11-25 10:21:49.480511',
    'up_sample': '2020-11-25 10:30:33.106171',
    'down_sample': '2020-11-25 10:30:21.244073'
}

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("--experiment_type", type=str, required=True, choices=['standard', 'up_sample', 'down_sample'])
    args = parser.parse_args()

    experiment_type = args.experiment_type

    print('STARTING EVALUATION <{}> WITH ItemKNN'.format(experiment_type))
    data_path = DOWN_DATA_PATH if experiment_type == 'down_sample' else DATA_PATH
    demo_path = DOWN_DEMO_PATH if experiment_type == 'down_sample' else DEMO_PATH

    experiment_datetime = best_configs[experiment_type]
    if not experiment_datetime:
        raise ValueError('Configuration <{}> for <{}> not found!'.format(experiment_datetime, experiment_type))

    for fold_n in trange(5, desc='folds'):

        log_val_str = LOG_VAL_STR.format('itemknn', experiment_type, experiment_datetime, fold_n)
        log_te_str = LOG_TE_STR.format('itemknn', experiment_type, experiment_datetime, fold_n)

        ds = DataSplitter(data_path, demo_path, out_dir=OUT_DIR)
        pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(fold_n=fold_n)

        if experiment_type == 'up_sample':
            up_tr_data_path, up_sp_tr_data_path = ds.up_sample_train_data_path(pandas_dir_path, scipy_dir_path,
                                                                               'gender')

        # --- Data --- #
        sp_tr_data = sp.load_npz(
            up_sp_tr_data_path if experiment_type == 'up_sample' else os.path.join(scipy_dir_path, 'sp_tr_data.npz'))
        sp_te_tr_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_tr_data.npz'))
        sp_te_te_data = sp.load_npz(os.path.join(scipy_dir_path, 'sp_te_te_data.npz'))

        user_groups_all_traits = dict()
        for trait in DEMO_TRAITS:
            # This creates UserGroup object that store the index of the users belonging to the group in te and vd data.
            user_groups_all_traits[trait] = ds.get_user_groups_indxs(pandas_dir_path, trait)
        print("Data Loaded")

        # --- Test --- #

        summ = SummaryWriter(log_te_str)

        best_config = pickle_load(os.path.join(log_val_str, 'best_config.pkl'))

        A_test = sp.csc_matrix(sp.vstack((sp_te_tr_data, sp_tr_data)))

        Atild_test = ItemKNN(A_test, best_config['maxk'], best_config['shrink'], best_config['similarity'],
                             best_config['normalize'])

        Atild_test = Atild_test[:sp_te_tr_data.shape[0]]
        # Removing entries from training data
        Atild_test[sp_te_tr_data.nonzero()] = .0

        preds = Atild_test.toarray()
        true = sp_te_te_data.toarray()

        full_metrics = dict()
        full_raw_metrics = dict()
        for trait in DEMO_TRAITS:
            user_groups = user_groups_all_traits[trait]

            _, metrics, metrics_raw = eval_proced(preds=preds,
                                                  true=true,
                                                  tag='test',
                                                  user_groups=user_groups,
                                                  tids_path=tids_path,
                                                  entropy_norm=True)

            full_metrics.update(metrics)
            full_raw_metrics.update(metrics_raw)

        # Logging hyperparams and metrics
        summ.add_hparams({**best_config, 'fold_n': fold_n}, full_metrics)
        summ.flush()

        # Saving results and predictions
        pickle_dump(full_metrics, os.path.join(log_te_str, 'full_metrics.pkl'))
        pickle_dump(full_raw_metrics, os.path.join(log_te_str, 'full_raw_metrics.pkl'))
