from argparse import Namespace
from datetime import datetime

import numpy as np
import torch
from sklearn.model_selection import ParameterGrid
from torch.utils.data import DataLoader
from tqdm import tqdm

from algorithms.vae.DummyDataset import DummyDataset
from algorithms.vae.model.multi_vae import MultiVAEN
from conf import VAE_SEED, VAE_MAX_EPOCHS, \
    UN_SEEDS, UN_LOG_TE_STR, UN_LOG_VAL_STR
from utils.eval import eval_metric

print('STARTING EXPERIMENTS WITH VAE')
print('SEEDS ARE: {}'.format(UN_SEEDS))

device = 'cuda:0' if torch.cuda.is_available() else 'cpu'

grid = {
    "p_dims": ["100,{}", "500,{}", "50,200,{}", "100,200,{}", "200,500,{}"],
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

    # TODO: Split Data
    # ds = DataSplitter(DATA_PATH, PERS_PATH, out_dir=UN_OUT_DIR)
    # pandas_dir_path, scipy_dir_path, uids_dic_path, tids_path = ds.get_paths(seed)

    # Setting seed for reproducibility # TODO: check this (don't move)
    torch.manual_seed(VAE_SEED)
    np.random.seed(VAE_SEED)

    # Creating data loaders
    # Do not batch val and test data! Evaluation will differ from the other algorithms!
    # (mean of the means instead of a simple mean)
    tr_loader = DataLoader(DummyDataset(), batch_size=64, shuffle=True, num_workers=10)
    vd_loader = DataLoader(DummyDataset(), batch_size=5000, num_workers=10)
    te_loader = DataLoader(DummyDataset(), batch_size=5000, num_workers=10)

    best_value = 0
    best_config = None
    # Running Hyperparameter search
    for config in tqdm(pg, desc='configs'):
        config['p_dims'] = config['p_dims'].format(100)  # TODO: CHANGE THIS

        # Adding seed just for the sake of logging
        config['seed'] = seed

        config = Namespace(**config)

        # Model definition
        model = MultiVAEN(config.p_dims, config.q_dims, config.dp, config.betacap, config.betasteps)
        opt = torch.optim.Adam(model.parameters(), lr=config.lr,
                               weight_decay=0)  # TODO: check the effect of weight decay
        # TODO: Declare SacredLogger
        # Training

        for epoch in range(VAE_MAX_EPOCHS):  # TODO: maybe change this
            print(epoch)
            model.train()
            for x, y in tr_loader:
                x, y = x.to(device), y.to(device)
                logits, KL = model(x)
                loss, neg_ll, weighted_KL = model.vae_loss(logits, KL, y)
                loss.backward()
                opt.step()
                opt.zero_grad()

                # TODO: Log losses

            # End of the epcoh, do validation (maybe every five?)
            if epoch % 5 == 0 and epoch!= 0:  # TODO: Check every parameter

                model.eval()
                for x, y in vd_loader:
                    x, _ = x.to(device), y.to(device)
                    logits, _ = model(x)

                    logits[x.nonzero(as_tuple=True)] = .0

                    logits = logits.detach().cpu().numpy()
                    y = y.detach().cpu().numpy()
                    val_metric = eval_metric(logits, y)  # TODO: What is the returning type?

                curr_value = np.mean(val_metric)
                print(curr_value)
                if curr_value > best_value:

                    best_value = curr_value
                    print('New best model found')

                    torch.save(model.state_dict(),'.') # TODO: PATH?

    # TODO: Load on the best configuration!

    # Test
    # te_logger = TensorBoardLogger(os.path.dirname(log_te_str), name=os.path.basename(log_te_str))
    # best_model = MultiVAE.load_from_checkpoint(best_path)
    # best_model.set_low_high_indxs(low_high_indxs)
    # trainer = Trainer(logger=te_logger)
    # trainer.test(best_model, te_loader)

    # Saving the best_config
    # pickle.dump(vars(best_config), open(os.path.join(log_te_str, 'best_config.pkl'), 'wb'))
