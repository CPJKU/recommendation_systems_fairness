import os
import pickle
import sys
from argparse import Namespace
from datetime import datetime

sys.path.append(os.path.abspath('../../'))
import numpy as np
import torch
from pytorch_lightning import Trainer
from pytorch_lightning.loggers import TensorBoardLogger
from sklearn.model_selection import ParameterGrid
from torch.utils.data import DataLoader
from tqdm import tqdm

from algorithms.vae.TwtPerDataset import TwitPers
from algorithms.vae.model.multi_vae import MultiVAE
from algorithms.vae.utils.utils import LogHparams
from conf import VAE_SEED, SEEDS, TRAITS, LOG_VAL_STR, LOG_TE_STR, DATA_PATH, PERS_PATH, OUT_DIR, VAE_MAX_EPOCHS
from utils.data_splitter import DataSplitter

print('STARTING CONTROLLED EXPERIMENTS WITH VAE')
print('SEEDS ARE: {}'.format(SEEDS))

grid = {
    "p_dims": ["100,{}", "500,{}", "50,200,{}", "100,200,{}", "200,500,{}"],
    'q_dims': [''],
    "act": ["tanh"],  # ReLu does not make a real difference
    "lr": [1e-3],  # Changing the learning rate does not improve results
    "doanneal": [True],
    "betacap": [0.5, 1],
    "betasteps": [10000, 20000],
    "beta": [1e-2],
    "wd": [.0],
    "dp": [0.5],

}
pg = ParameterGrid(grid)

now = datetime.now()

for trait in tqdm(TRAITS, desc='traits'):
    print('WORKING ON TRAIT: ' + trait)
    for seed in tqdm(SEEDS, desc='seeds'):

        log_val_str = LOG_VAL_STR.format('vae', now, trait, seed)
        log_te_str = LOG_TE_STR.format('vae', now, trait, seed)

        ds = DataSplitter(DATA_PATH, PERS_PATH, out_dir=OUT_DIR)
        pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(seed, trait)

        # Setting seed for reproducibility
        torch.manual_seed(VAE_SEED)
        np.random.seed(VAE_SEED)

        low_high_indxs = dict()
        for tr in TRAITS:
            # This assigns a 4-entry tuple (vd_low_idxs, vd_high_idxs, te_low_idxs, te_high_idxs)
            low_high_indxs[tr] = ds.get_low_high_indxs(pandas_dir_path, uids_dic_path, tr)

        # Creating data loaders
        # Do not batch val and test data! Evaluation will differ from the other algorithms!
        # (mean of the means instead of a simple mean)
        tr_loader = DataLoader(TwitPers(scipy_dir_path, which='train'), batch_size=64, shuffle=True, num_workers=10)
        vd_loader = DataLoader(TwitPers(scipy_dir_path, pandas_dir_path, uids_dic_path, which='val'), batch_size=5000,
                               num_workers=10)
        te_loader = DataLoader(TwitPers(scipy_dir_path, pandas_dir_path, uids_dic_path, which='test'), batch_size=5000,
                               num_workers=10)

        best_value = 0
        best_config = None
        best_path = None
        # Running Hyperparameter search
        for config in tqdm(pg, desc='configs'):
            config['p_dims'] = config['p_dims'].format(ds.n_items)

            # Adding trait and seed just for the sake of logging
            config['seed'] = seed
            config['trait'] = trait

            config = Namespace(**config)

            model = MultiVAE(config)
            # eval procedure uses the indexes to split high group and low group
            model.set_low_high_indxs(low_high_indxs)
            vd_logger = TensorBoardLogger(os.path.dirname(log_val_str), name=os.path.basename(log_val_str))
            trainer = Trainer(max_epochs=VAE_MAX_EPOCHS, logger=vd_logger, callbacks=[LogHparams()], gpus=[0],
                              check_val_every_n_epoch=5)
            trainer.fit(model, train_dataloader=tr_loader, val_dataloaders=vd_loader)

            curr_best_path = trainer.checkpoint_callback.kth_best_model
            # Lightning defaults to val_loss
            curr_value = trainer.checkpoint_callback.kth_value

            if curr_value < best_value:
                best_path = curr_best_path
                best_value = curr_value
                best_config = config

        # Test
        te_logger = TensorBoardLogger(os.path.dirname(log_te_str), name=os.path.basename(log_te_str))
        best_model = MultiVAE.load_from_checkpoint(best_path)
        best_model.set_low_high_indxs(low_high_indxs)
        trainer = Trainer(logger=te_logger)
        trainer.test(best_model, te_loader)

        # Saving the best_config
        pickle.dump(vars(best_config), open(os.path.join(log_te_str, 'best_config.pkl'), 'wb'))
