from collections import OrderedDict

import torch
from torch import nn
from torch.nn import functional


class MultiDAE(nn.Module):
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


def return_dims(dims: str):
    return [int(x) for x in dims.strip().split(",")]
