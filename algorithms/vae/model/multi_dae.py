from collections import OrderedDict

import pytorch_lightning as pl
import torch
from torch import nn
from torch.nn import functional

from algorithms.vae.utils.utils import return_act, return_dims


class MultiDAE(pl.LightningModule):
    """

    Attributes
    ---------
    p_dims  : list
        list of values that defines the structure of the network on the decoder side
    q_dims : list
        list of values that defines the structure of the network on the encoder side (Optional)
    act : nn.Layer (activation Function)
        activation function to use in the network.
    dp: float
        dropout value
    wd: float
        weight decay as regularization (Optional)

    """

    def __init__(self, hparams):
        super().__init__()

        self.hparams = hparams

        ## Reading Parameters ##
        self.p_dims = return_dims(hparams.p_dims)

        if not hparams.q_dims:
            self.q_dims = self.p_dims[::-1]
        else:
            q_dims = return_dims(hparams.q_dims)
            assert q_dims[-1] == self.p_dims[0], "Latent dimension for p- and q-network mismatches."
            self.q_dims = q_dims

        self.latent = self.p_dims[0]

        self.act = return_act(hparams.act)
        self.act_name = hparams.act

        self.dropout = nn.Dropout(hparams.dp)
        self.lr = hparams.lr
        self.wd = hparams.wd

        self.best_metrics = {}

        # Encoder #
        enc = OrderedDict()
        for i, (d_in, d_out) in enumerate(zip(self.q_dims[:-1], self.q_dims[1:])):
            enc["layer_{}".format(i)] = nn.Linear(d_in, d_out)

            if i != len(self.q_dims) - 2:
                enc["{}_{}".format(self.act_name, i)] = self.act

        self.encoder = nn.Sequential(enc)

        # Decoder #
        dec = OrderedDict()
        for i, (d_in, d_out) in enumerate(zip(self.p_dims[:-1], self.p_dims[1:])):
            dec["layer_{}".format(i)] = nn.Linear(d_in, d_out)

            if i != len(self.p_dims) - 2:
                dec["{}_{}".format(self.act_name, i)] = self.act

        self.decoder = nn.Sequential(dec)

        self.apply(self._init_weights)

    def _init_weights(self, m: nn.Module):
        gain = nn.init.calculate_gain(self.act_name)
        if type(m) == nn.Linear:
            torch.nn.init.xavier_normal_(m.weight, gain)
            torch.nn.init.constant_(m.bias, 0.01)

    def forward(self, x):
        x = functional.normalize(x, 2, 1)
        x = self.dropout(x)
        x = self.encoder(x)
        x = self.decoder(x)

        return x

    def configure_optimizers(self):
        return torch.optim.Adam(self.parameters(),
                                lr=self.lr,
                                weight_decay=self.wd)

    def training_step(self, batch, batch_idx):
        raise Exception("This is not configured to run with the current version of the code!")
        x, y = batch
        logits = self(x)
        y = functional.normalize(y, 2, 1)
        prob = functional.log_softmax(logits, dim=1)
        neg_ll = - torch.mean(torch.sum(prob * y, dim=1))
        logs = {"neg_ll": neg_ll}
        return {"neg_ll": neg_ll, "log": logs}

    def train_dataloader(self):
        # Ignored
        return None

    def val_dataloader(self):
        # Ignored
        return None


class MultiDAEN(nn.Module):
    """

    Attributes
    ---------
    p_dims  : str
        list of values that defines the structure of the network on the decoder side
    q_dims : str
        list of values that defines the structure of the network on the encoder side (Optional)
    dp: float
        dropout value
    wd: float
        weight decay as regularization (Optional)

    """

    def __init__(self, p_dims, q_dims=None, dp=0.5):
        super().__init__()

        ## Reading Parameters ##
        self.p_dims = return_dims(p_dims)

        if q_dims:
            q_dims = return_dims(q_dims)
            assert q_dims[-1] == self.p_dims[0], "Latent dimension for p- and q-network mismatches."
        else:
            self.q_dims = self.p_dims[::-1]

        self.latent = self.p_dims[0]
        self.dropout = nn.Dropout(dp)

        # Encoder #
        enc = OrderedDict()
        for i, (d_in, d_out) in enumerate(zip(self.q_dims[:-1], self.q_dims[1:])):
            enc["layer_{}".format(i)] = nn.Linear(d_in, d_out)

            if i != len(self.q_dims) - 2:
                enc["tanh_{}".format(i)] = nn.Tanh()

        self.encoder = nn.Sequential(enc)

        # Decoder #
        dec = OrderedDict()
        for i, (d_in, d_out) in enumerate(zip(self.p_dims[:-1], self.p_dims[1:])):
            dec["layer_{}".format(i)] = nn.Linear(d_in, d_out)

            if i != len(self.p_dims) - 2:
                dec["tanh_{}".format(i)] = nn.Tanh()

        self.decoder = nn.Sequential(dec)

        self.apply(self._init_weights)

    def _init_weights(self, m: nn.Module):
        gain = nn.init.calculate_gain('tanh')
        if type(m) == nn.Linear:
            torch.nn.init.xavier_normal_(m.weight, gain)
            torch.nn.init.constant_(m.bias, 0.01)

    def forward(self, x):
        x = functional.normalize(x, 2, 1)
        x = self.dropout(x)
        x = self.encoder(x)
        x = self.decoder(x)

        return x
