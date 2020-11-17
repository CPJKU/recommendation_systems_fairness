from collections import OrderedDict

import torch
from torch import nn
from torch.nn import functional

from algorithms.vae.model.multi_dae import MultiDAE


class VAE_loss:

    def __init__(self, betacap=0.5, betasteps=2000):
        super().__init__()

        self.betacap = betacap
        self.betasteps = betasteps

        self.curr_beta = 0.0
        self.global_step = 0

    def __call__(self, logits, KL, y):
        prob = functional.log_softmax(logits, dim=1)

        neg_ll = - torch.mean(torch.sum(prob * y, dim=1))
        weighted_KL = self.curr_beta * KL
        loss = neg_ll + weighted_KL

        #  Updating beta
        self.curr_beta = min(self.betacap, self.global_step / self.betasteps)
        self.global_step += 1
        return loss, neg_ll, weighted_KL


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
