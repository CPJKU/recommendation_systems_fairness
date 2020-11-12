import os
import pickle
from collections import OrderedDict

import torch
from torch import nn
from torch.nn import functional

from algorithms.vae.model.multi_dae import MultiDAE, MultiDAEN
from utils.eval import eval_proced, eval_metric


class MultiVAE(MultiDAE):
    """
    Variational Autoencoders for Collaborative Filtering - Dawen Liang, Rahul G. Krishnan, Matthew D. Hoffman, Tony Jebara
    https://arxiv.org/abs/1802.05814
    Attributes
    ---------
    p_dims  : list
        list of values that defines the structure of the network on the decoder side
    q_dims : list
        list of values that defines the structure of the network on the encoder side (Optional)
    dp: float
        dropout value
    doanneal: bool
        True for annealing, False for fixed beta
    beta: float
        beta value multiplied to the KL (used for doanneal=False)
    betacap: float
        maximum value beta can reach (used for doanneal=True)
    betastep: int
        number of iteration on which to do the annealing  (used for doanneal=True)
    """

    def __init__(self, hparams):
        super().__init__(hparams)

        self.doanneal = hparams.doanneal
        self.beta = hparams.beta
        self.betacap = hparams.betacap
        self.betasteps = hparams.betasteps

        self.curr_beta = 0.0 if self.doanneal else self.beta

        self.low_high_indxs = None

        # Overwrite the encoder
        # Encoder #
        enc = OrderedDict()
        for i, (d_in, d_out) in enumerate(zip(self.q_dims[:-1], self.q_dims[1:])):
            if i == len(self.q_dims) - 2:
                d_out *= 2
            enc["layer_{}".format(i)] = nn.Linear(d_in, d_out)

            if i != len(self.q_dims) - 2:
                enc["{}_{}".format(self.act_name, i)] = self.act

        self.encoder = nn.Sequential(enc)

    def set_low_high_indxs(self, low_high_indxs):
        '''
        Sets the dictionary containing the indexes for the high and low group for val and test data and across each trait
        Format is:
        low_high_indxs['<trait>'] = (vd_low_idxs, vd_high_idxs, te_low_idxs, te_high_idxs)
        '''
        self.low_high_indxs = low_high_indxs

    def forward(self, x):
        # Encoder #
        x = functional.normalize(x, 2, 1)
        x = self.dropout(x)
        x = self.encoder(x)
        mu, logvar = x[:, :self.latent], x[:, self.latent:]

        # Sampling #
        z, KL = self._sampling(mu, logvar)

        # Decoder #
        z = self.decoder(z)

        return z, KL

    def _sampling(self, mu, logvar):
        # KL loss
        KL = torch.mean(torch.sum(0.5 * (-logvar + torch.exp(logvar) + mu ** 2 - 1), dim=1))

        # Sampling #
        std = torch.exp(0.5 * logvar)
        epsilon = torch.randn_like(std)

        z = mu + epsilon * std * self.training
        return z, KL

    def training_step(self, batch, batch_num):
        x, y = batch
        logits, KL = self(x)

        prob = functional.log_softmax(logits, dim=1)

        neg_ll = - torch.mean(torch.sum(prob * y, dim=1))
        weighted_KL = self.curr_beta * KL
        loss = neg_ll + weighted_KL

        #  Updating beta if required
        if self.doanneal:
            self.curr_beta = min(self.betacap, self.global_step / self.betasteps)

        logs = {"train/loss": loss, "train/neg_ll": neg_ll, "train/KL": KL, "train/weighted_KL": weighted_KL}
        return {"loss": loss, "log": logs}

    def validation_step(self, batch, batch_idx):
        x, y = batch

        logits, _ = self(x)

        logits[x.nonzero(as_tuple=True)] = .0

        logits = logits.cpu().numpy()
        y = y.cpu().numpy()

        val_metric = eval_metric(logits, y)
        # This is too slow, cannot run evaluation on everything
        # val_metric, metrics, metrics_raw = eval_proced(logits, y, high_idxs, low_idxs, 'val')
        # return {"val_metric": val_metric, "metrics": metrics, 'metrics_raw': metrics_raw}

        return {'val_metric': val_metric}

    def test_step(self, batch, batch_idx):

        x, y = batch

        logits, _ = self(x)

        logits[x.nonzero(as_tuple=True)] = .0

        logits = logits.cpu().numpy()
        y = y.cpu().numpy()

        full_metrics = dict()
        full_raw_metrics = dict()


        return {"full_metrics": full_metrics, 'full_raw_metrics': full_raw_metrics}

    def validation_epoch_end(self, outputs):
        # It considers only 1 batch!
        assert len(outputs) == 1
        val_results = outputs[0]
        val_metric = val_results['val_metric']
        # Save best metrics if needed
        best_value = self.trainer.checkpoint_callback.kth_value

        if -val_metric < best_value:
            self.best_metrics = val_results

        return {'val_loss': -torch.tensor(val_metric), 'log': val_results}

    def test_epoch_end(self, outputs):
        # It considers only 1 batch!
        assert len(outputs) == 1
        test_results = outputs[0]
        metrics = test_results['full_metrics']
        metrics_raw = test_results['full_raw_metrics']

        metrics_raw_path = os.path.join((os.path.dirname(self.trainer.ckpt_path)), 'metrics_raw.pkl')
        pickle.dump(metrics_raw, open(metrics_raw_path, 'wb'))

        # Lightning needs to remove lines from the pre_train_routine in order to correctly log this (hparams)
        # (see https://github.com/PyTorchLightning/pytorch-lightning/issues/1778)
        self.logger.experiment.add_hparams(vars(self.hparams), metrics)

        return metrics


class VAE_loss:

    def __init__(self, betacap=0.5, betasteps=2000):
        super().__init__()

        self.betacap = betacap
        self.betasteps = betasteps

        self.curr_beta = 0.0
        self.global_step = 0  # TODO:CHECK THIS

    def __call__(self, logits, KL, y):
        prob = functional.log_softmax(logits, dim=1)

        neg_ll = - torch.mean(torch.sum(prob * y, dim=1))
        weighted_KL = self.curr_beta * KL
        loss = neg_ll + weighted_KL

        #  Updating beta
        self.curr_beta = min(self.betacap, self.global_step / self.betasteps)
        return loss, neg_ll, weighted_KL


class MultiVAEN(MultiDAEN):
    """
    Variational Autoencoders for Collaborative Filtering - Dawen Liang, Rahul G. Krishnan, Matthew D. Hoffman, Tony Jebara
    https://arxiv.org/abs/1802.05814
    Attributes
    ---------
    p_dims  : list
        list of values that defines the structure of the network on the decoder side
    q_dims : list
        list of values that defines the structure of the network on the encoder side (Optional)
    dp: float
        dropout value
    betacap: float
        maximum value beta can reach (used for doanneal=True)
    betastep: int
        number of iteration on which to do the annealing  (used for doanneal=True)
    """

    def __init__(self, p_dims, q_dims=None, dp=0.5, betacap=0.5, betasteps=2000):
        super().__init__(p_dims, q_dims, dp)

        self.vae_loss = VAE_loss(betacap, betasteps)

        # Overwrite the encoder
        # Encoder #
        enc = OrderedDict()
        for i, (d_in, d_out) in enumerate(zip(self.q_dims[:-1], self.q_dims[1:])):
            if i == len(self.q_dims) - 2:
                d_out *= 2
            enc["layer_{}".format(i)] = nn.Linear(d_in, d_out)

            if i != len(self.q_dims) - 2:
                enc["tanh_{}".format(i)] = nn.Tanh()

        self.encoder = nn.Sequential(enc)

    def forward(self, x):
        # Encoder #
        x = functional.normalize(x, 2, 1)
        x = self.dropout(x)
        x = self.encoder(x)
        mu, logvar = x[:, :self.latent], x[:, self.latent:]

        # Sampling #
        z, KL = self._sampling(mu, logvar)

        # Decoder #
        z = self.decoder(z)

        return z, KL

    def _sampling(self, mu, logvar):
        # KL loss
        KL = torch.mean(torch.sum(0.5 * (-logvar + torch.exp(logvar) + mu ** 2 - 1), dim=1))

        # Sampling #
        std = torch.exp(0.5 * logvar)
        epsilon = torch.randn_like(std)

        z = mu + epsilon * std * self.training
        return z, KL
