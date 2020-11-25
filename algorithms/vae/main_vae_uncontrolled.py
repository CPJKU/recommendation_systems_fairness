import argparse
import os

import numpy as np
import torch
from datetime import datetime
from sklearn.model_selection import ParameterGrid
from torch.utils.data import DataLoader
from torch.utils.tensorboard import SummaryWriter
from tqdm import tqdm, trange

from algorithms.vae.LFM2bDataset import LFM2bDataset
from algorithms.vae.model.multi_vae import MultiVAE
from conf import EXP_SEED, VAE_MAX_EPOCHS, \
    LOG_TE_STR, LOG_VAL_STR, DATA_PATH, DEMO_PATH, OUT_DIR, DEMO_TRAITS, VAE_LOG_VAL_EVERY, DOWN_DATA_PATH, \
    DOWN_DEMO_PATH
from utils.data_splitter import DataSplitter
from utils.eval import eval_metric
from utils.helper import reproducible, pickle_dump

grid = {
    "p_dims": ["500,{}", "1000,{}", "500,1000,{}"],
    # 'q_dims': [''],
    # "lr": [1e-3, 1e-4],  # Changing the learning rate does not improve results
    "betacap": [1],  # betcap does not influence a lot the results
    "betasteps": [5000, 10000],  # with batch_size 64 and 100 epochs, there are 18.800 updates steps
}
pg = ParameterGrid(grid)

now = datetime.now()

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument("--experiment_type", type=str, required=True, choices=['standard', 'up_sample', 'down_sample'])
    parser.add_argument("--gpu", type=str, required=True)
    args = parser.parse_args()

    experiment_type = args.experiment_type
    gpu = args.gpu

    print('STARTING EXPERIMENTS <{}> WITH VAE'.format(experiment_type))

    device = 'cuda:{}'.format(gpu) if torch.cuda.is_available() else 'cpu'
    data_path = DOWN_DATA_PATH if experiment_type == 'down_sample' else DATA_PATH
    demo_path = DOWN_DEMO_PATH if experiment_type == 'down_sample' else DEMO_PATH

    for fold_n in trange(5, desc='folds'):

        log_val_str = LOG_VAL_STR.format('vae', experiment_type, now, fold_n)
        log_te_str = LOG_TE_STR.format('vae', experiment_type, now, fold_n)

        ds = DataSplitter(data_path, demo_path, out_dir=OUT_DIR)
        pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(fold_n=fold_n)

        if experiment_type == 'up_sample':
            ds.up_sample_train_data_path(pandas_dir_path, scipy_dir_path, 'gender')

        # Setting seed for reproducibility
        reproducible(EXP_SEED)

        # --- Data --- #
        tr_loader = DataLoader(LFM2bDataset(scipy_dir_path, which='train', up_sample=experiment_type == 'up_sample'),
                               batch_size=64, shuffle=True, num_workers=10)
        vd_loader = DataLoader(LFM2bDataset(scipy_dir_path, pandas_dir_path, uids_dic_path, which='val'),
                               batch_size=128,
                               num_workers=10, shuffle=False)
        te_loader = DataLoader(LFM2bDataset(scipy_dir_path, pandas_dir_path, uids_dic_path, which='test'),
                               batch_size=128,
                               num_workers=10, shuffle=False)

        user_groups_all_traits = dict()
        for trait in DEMO_TRAITS:
            # This creates UserGroup object that store the index of the users belonging to the group in te and vd data.
            user_groups_all_traits[trait] = ds.get_user_groups_indxs(pandas_dir_path, trait)
        print("Data Loaded")

        best_value = 0
        # Running Hyperparameter search
        for config in tqdm(pg, desc='configs'):
            config['p_dims'] = config['p_dims'].format(ds.n_items)

            summ = SummaryWriter(os.path.join(log_val_str, str(config)))

            # Model definition
            model = MultiVAE(config['p_dims'], "", 0.5, config['betacap'], config['betasteps'])
            model = model.to(device)
            opt = torch.optim.Adam(model.parameters(), lr=1e-3, weight_decay=0.0)

            curr_value = 0
            for epoch in trange(VAE_MAX_EPOCHS, desc='epochs'):
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

                    losses.append(loss.item())
                    neg_lls.append(neg_ll.item())
                    weighted_KLs.append(weighted_KL.item())

                summ.add_scalar('train/avg_loss', np.mean(losses), epoch)
                summ.add_scalar('train/avg_neg_lss', np.mean(neg_lls), epoch)
                summ.add_scalar('train/avg_weighted_KL', np.mean(weighted_KLs), epoch)

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
                        val_metrics += list(eval_metric(logits, y, aggregated=False))

                    curr_value = np.mean(val_metrics)
                    summ.add_scalar('val/ndcg_50', curr_value, epoch)
                    summ.flush()

                    if curr_value > best_value:
                        print('New best model found')
                        best_value = curr_value

                        pickle_dump(config, os.path.join(log_val_str, 'best_config.pkl'))
                        torch.save(model.state_dict(), os.path.join(log_val_str, 'best_model.pth'))
            # Logging hyperparams and metrics
            summ.add_hparams({**config, 'fold_n': fold_n}, {'val/ndcg_50': curr_value})
            summ.flush()
