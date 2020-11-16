import os
import pickle
from argparse import Namespace

import numpy as np
import torch
from datetime import datetime
from sklearn.model_selection import ParameterGrid
from torch.utils.data import DataLoader
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm, trange

from algorithms.vae.LFM2bDataset import LFM2bDataset
from algorithms.vae.model.multi_vae import MultiVAE
from conf import VAE_SEED, VAE_MAX_EPOCHS, \
    UN_SEEDS, UN_LOG_TE_STR, UN_LOG_VAL_STR, DATA_PATH, DEMO_PATH, UN_OUT_DIR, DEMO_TRAITS, VAE_LOG_VAL_EVERY
from utils.data_splitter import DataSplitter
from utils.eval import eval_metric, eval_proced
from utils.helper import reproducible

print('STARTING UNCONTROLLED EXPERIMENTS WITH VAE')
print('SEEDS ARE: {}'.format(UN_SEEDS))

device = 'cuda:0' if torch.cuda.is_available() else 'cpu'

grid = {
    "p_dims": ["100,{}"],  # "500,{}", "50,200,{}", "100,200,{}", "200,500,{}"],
    'q_dims': [''],
    "lr": [1e-3],  # Changing the learning rate does not improve results
    "betacap": [0.5, 1],
    "betasteps": [10000, 20000],
    "wd": [.0],
    "dp": [0.5],

}
pg = ParameterGrid(grid)

now = datetime.now()

for seed in tqdm(UN_SEEDS, desc='seeds'):

    log_val_str = UN_LOG_VAL_STR.format('vae', now, seed)
    log_te_str = UN_LOG_TE_STR.format('vae', now, seed)

    if not os.path.isdir(log_val_str):
        os.makedirs(log_val_str)  # TODO: why?

    ds = DataSplitter(DATA_PATH, DEMO_PATH, out_dir=UN_OUT_DIR)
    pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(seed)

    # Setting seed for reproducibility
    reproducible(VAE_SEED)

    # Creating data loaders
    tr_loader = DataLoader(LFM2bDataset(scipy_dir_path, which='train'), batch_size=128, shuffle=True, num_workers=10)
    vd_loader = DataLoader(LFM2bDataset(scipy_dir_path, pandas_dir_path, uids_dic_path, which='val'), batch_size=128,
                           num_workers=10, shuffle=False)
    te_loader = DataLoader(LFM2bDataset(scipy_dir_path, pandas_dir_path, uids_dic_path, which='test'), batch_size=128,
                           num_workers=10, shuffle=False)

    user_groups_all_traits = dict()
    for trait in DEMO_TRAITS:
        # This creates UserGroup object that store the index of the users belonging to the group in te and vd data.
        user_groups_all_traits[trait] = ds.get_user_groups_indxs(pandas_dir_path, uids_dic_path, trait)
    print("Data Loaded")

    best_value = 0
    best_config = None
    # Running Hyperparameter search
    for config in tqdm(pg, desc='configs'):
        config['p_dims'] = config['p_dims'].format(ds.n_items)

        # Adding seed just for the sake of logging
        config['seed'] = seed

        config = Namespace(**config)

        # Model definition
        model = MultiVAE(config.p_dims, config.q_dims, config.dp, config.betacap, config.betasteps)
        model = model.to(device)
        opt = torch.optim.Adam(model.parameters(), lr=config.lr)

        summ = SummaryWriter(log_val_str)

        for epoch in trange(VAE_MAX_EPOCHS):
            # --- Training --- #
            model.train()
            losses, neg_lls, weighted_KLs = [], [], []
            for x, y in tr_loader:
                x, y = x.to(device), y.to(device)

                logits, KL = model(x)
                loss, neg_ll, weighted_KL = model.vae_loss(logits, KL, y)

                loss.backward()
                opt.step()
                opt.zero_grad()

                losses.append(loss)
                neg_lls.append(neg_ll)
                weighted_KLs.append(weighted_KL)

            summ.add_scalars('train', {
                'avg_loss': np.mean(losses),
                'avg_neg_lss': np.mean(neg_lls),
                'avg_weighted_KL': np.mean(weighted_KLs)
            }, epoch)

            # --- Validation --- #
            if epoch % VAE_LOG_VAL_EVERY == 0:
                model.eval()
                val_metrics = []
                for x, y in vd_loader:
                    x = x.to(device)
                    logits, _ = model(x)

                    # Removing items from training data
                    logits[x.nonzero(as_tuple=True)] = .0

                    logits = logits.detach().cpu().numpy()
                    val_metrics.append(eval_metric(logits, y, aggregated=False))

                curr_value = np.mean(val_metrics)
                summ.add_scalar('val/ndcg_50', curr_value, epoch)
                if curr_value > best_value:
                    best_value = curr_value
                    print('New best model found')
                    best_config = config
                    torch.save(model.state_dict(), os.path.join(log_val_str, 'best_model.pth'))  # TODO: PATH?

    # Test
    model = MultiVAE(best_config.p_dims, best_config.q_dims, best_config.dp, best_config.betacap,
                     best_config.betasteps)
    model.load_state_dict(torch.load(os.path.join(log_val_str, 'best_model.pth')))

    summ = SummaryWriter(log_te_str)

    model.eval()
    all_y = []
    all_logits = []
    for x, y in te_loader:
        x = x.to(device)
        logits, _ = model(x)

        # Removing items from training data
        logits[x.nonzero(as_tuple=True)] = .0

        logits = logits.detach().cpu().numpy()

        # Fetching all predictions and ground_truth labels
        all_y.append(y)
        all_logits.append(logits)

    all_y = np.array(all_y).flatten()
    all_logits = np.array(all_logits).flatten()

    full_metrics = dict()
    full_raw_metrics = dict()
    for trait in DEMO_TRAITS:
        user_groups = user_groups_all_traits[trait]
        _, metrics, metrics_raw = eval_proced(all_logits, all_logits, 'test', user_groups)
        full_metrics.update(metrics)
        full_raw_metrics.update(metrics_raw)

    summ.add_hparams({**vars(best_config), 'seed': seed}, full_metrics)
    # Saving the best_config
    pickle.dump(vars(best_config), open(os.path.join(log_te_str, 'best_config.pkl'), 'wb'))
