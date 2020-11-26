import argparse
import os

import numpy as np
import torch
from torch.utils.data import DataLoader
from torch.utils.tensorboard import SummaryWriter
from tqdm import trange

from algorithms.vae.LFM2bDataset import LFM2bDataset
from algorithms.vae.model.multi_vae import MultiVAE
from conf import EXP_SEED, LOG_TE_STR, LOG_VAL_STR, DATA_PATH, DEMO_PATH, OUT_DIR, DEMO_TRAITS, DOWN_DATA_PATH, \
    DOWN_DEMO_PATH
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced2_beyond_accuracy
from utils.helper import reproducible, pickle_dump, pickle_load

best_configs = {
    'standard': '2020-11-24 19:43:47.801230',
    'up_sample': '2020-11-24 19:44:32.188924',
    'down_sample': '2020-11-24 19:45:15.744244'
}

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("--experiment_type", type=str, required=True, choices=['standard', 'up_sample', 'down_sample'])
    parser.add_argument("--gpu", type=str, required=True)
    args = parser.parse_args()

    experiment_type = args.experiment_type
    gpu = args.gpu

    print('STARTING EVALUATION <{}> WITH VAE'.format(experiment_type))
    device = 'cuda:{}'.format(gpu) if torch.cuda.is_available() else 'cpu'
    data_path = DOWN_DATA_PATH if experiment_type == 'down_sample' else DATA_PATH
    demo_path = DOWN_DEMO_PATH if experiment_type == 'down_sample' else DEMO_PATH

    experiment_datetime = best_configs[experiment_type]
    if not experiment_datetime:
        raise ValueError('Configuration <{}> for <{}> not found!'.format(experiment_datetime, experiment_type))

    for fold_n in trange(5, desc='folds'):

        log_val_str = LOG_VAL_STR.format('vae', experiment_type, experiment_datetime, fold_n)
        log_te_str = LOG_TE_STR.format('vae', experiment_type, experiment_datetime, fold_n)

        ds = DataSplitter(data_path, demo_path, out_dir=OUT_DIR)
        pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(fold_n=fold_n)

        if experiment_type == 'up_sample':
            ds.up_sample_train_data_path(pandas_dir_path, scipy_dir_path, 'gender')

        # Setting seed for reproducibility
        reproducible(EXP_SEED)

        # --- Data --- #
        tr_loader = DataLoader(LFM2bDataset(scipy_dir_path, which='train', up_sample=experiment_type == 'up_sample'),
                               batch_size=64, shuffle=True, num_workers=10)
        te_loader = DataLoader(LFM2bDataset(scipy_dir_path, pandas_dir_path, uids_dic_path, which='test'),
                               batch_size=128,
                               num_workers=10, shuffle=False)

        user_groups_all_traits = dict()
        for trait in DEMO_TRAITS:
            # This creates UserGroup object that store the index of the users belonging to the group in te and vd data.
            user_groups_all_traits[trait] = ds.get_user_groups_indxs(pandas_dir_path, trait)
        print("Data Loaded")

        # --- Test --- #

        summ = SummaryWriter(log_te_str)

        best_config = pickle_load(os.path.join(log_val_str, 'best_config.pkl'))
        model = MultiVAE(best_config['p_dims'], '', 0.5, best_config['betacap'],
                         best_config['betasteps'])
        model.load_state_dict(torch.load(os.path.join(log_val_str, 'best_model.pth')))
        model = model.to(device)

        model.eval()
        all_y = []
        all_logits = []
        for x, y in te_loader:
            x = x.to(device)
            logits, _ = model(x)

            # Removing items from training data
            logits[x.nonzero(as_tuple=True)] = .0

            # Fetching all predictions and ground_truth labels
            all_logits.append(logits.detach().cpu().numpy())
            all_y.append(y.detach().cpu().numpy())

        preds = np.concatenate(all_logits)
        true = np.concatenate(all_y)

        full_metrics = dict()
        full_raw_metrics = dict()
        for trait in DEMO_TRAITS:
            user_groups = user_groups_all_traits[trait]
            # _, metrics, metrics_raw = eval_proced(preds, true, 'test', user_groups)
            _, metrics, metrics_raw = eval_proced2_beyond_accuracy(preds=preds,
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
        pickle_dump(full_metrics, os.path.join(log_te_str, 'full_metrics_beyond_accuracy.pkl'))
        pickle_dump(full_raw_metrics, os.path.join(log_te_str, 'full_raw_metrics_beyond_accuracy.pkl'))
