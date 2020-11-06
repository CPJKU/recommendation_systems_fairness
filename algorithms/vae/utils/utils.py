from pytorch_lightning import Callback
from torch import nn


def return_dims(dims: str):
    return [int(x) for x in dims.strip().split(",")]


def return_act(act: str):
    '''
    Return the loss function associated to the string passed as input. Valid values are "tanh" and "relu"
    :param act: name of the activation fucntion
    :return:
    '''
    if act == "tanh":
        return nn.Tanh()
    elif act == "relu":
        return nn.ReLU()
    else:
        raise ValueError("Activation function not defined")


class LogHparams(Callback):
    # This is used in order to hopefully override the (buggy) hparams logging mechanism
    # (see https://github.com/PyTorchLightning/pytorch-lightning/issues/1778)
    def on_train_end(self, trainer, model):
        trainer.logger.log_hyperparams(vars(model.hparams), model.best_metrics)
