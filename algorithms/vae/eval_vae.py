import os

import numpy as np
import torch
from torch.utils.data import DataLoader
from torch.utils.tensorboard import SummaryWriter
from tqdm import trange

from algorithms.vae.LFM2bDataset import LFM2bDataset
from algorithms.vae.model.multi_vae import MultiVAE
from conf import EXP_SEED, UN_LOG_TE_STR, UN_LOG_VAL_STR, DATA_PATH, DEMO_PATH, UN_OUT_DIR, DEMO_TRAITS
from utils.data_splitter import DataSplitter
from utils.eval import eval_proced
from utils.helper import reproducible, pickle_dump, pickle_load

print('STARTING EVALUATION WITH VAE')

device = 'cuda:0' if torch.cuda.is_available() else 'cpu'

experiment_datetime = '2020-11-19 16:32:40.811648'

for fold_n in trange(5, desc='folds'):

    log_val_str = UN_LOG_VAL_STR.format('vae', experiment_datetime, os.path.basename(DATA_PATH), fold_n)
    log_te_str = UN_LOG_TE_STR.format('vae', experiment_datetime, os.path.basename(DATA_PATH), fold_n)

    ds = DataSplitter(DATA_PATH, DEMO_PATH, out_dir=UN_OUT_DIR)
    pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(fold_n=fold_n)

    # Setting seed for reproducibility
    reproducible(EXP_SEED)

    # --- Data --- #
    tr_loader = DataLoader(LFM2bDataset(scipy_dir_path, which='train'), batch_size=64, shuffle=True, num_workers=10)
    te_loader = DataLoader(LFM2bDataset(scipy_dir_path, pandas_dir_path, uids_dic_path, which='test'), batch_size=128,
                           num_workers=10, shuffle=False)

    user_groups_all_traits = dict()
    for trait in DEMO_TRAITS:
        # This creates UserGroup object that store the index of the users belonging to the group in te and vd data.
        user_groups_all_traits[trait] = ds.get_user_groups_indxs(pandas_dir_path, trait)
    print("Data Loaded")

    # --- Test --- #

    summ = SummaryWriter(log_te_str)

    best_config = pickle_load(os.path.join(log_val_str, 'best_config.pkl'))
    model = MultiVAE(best_config['p_dims'], best_config['q_dims'], 0.5, best_config['betacap'],
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

    all_y = np.concatenate(all_y)
    all_logits = np.concatenate(all_logits)

    full_metrics = dict()
    full_raw_metrics = dict()
    for trait in DEMO_TRAITS:
        user_groups = user_groups_all_traits[trait]
        _, metrics, metrics_raw = eval_proced(all_logits, all_y, 'test', user_groups)
        full_metrics.update(metrics)
        full_raw_metrics.update(metrics_raw)

    # Logging hyperparams and metrics
    summ.add_hparams({**best_config, 'fold_n': fold_n}, full_metrics)
    summ.flush()

    # Saving results
    pickle_dump(full_metrics, os.path.join(log_te_str, 'full_metrics.pkl'))
    pickle_dump(full_raw_metrics, os.path.join(log_te_str, 'full_raw_metrics.pkl'))
