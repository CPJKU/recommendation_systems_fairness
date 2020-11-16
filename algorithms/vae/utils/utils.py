from abc import ABC, abstractmethod

from pathlib import Path


def return_dims(dims: str):
    return [int(x) for x in dims.strip().split(",")]


class Logger(ABC):
    """ Extracts and/or persists tracker information. """

    def __init__(self, path: str = None):
        """
        Parameters
        ----------
        path : str or Path, optional
            Path to where data will be logged.
        """
        path = Path() if path is None else Path(path)
        self.path = path.expanduser().absolute()

    @abstractmethod
    def log_scalar(self, tag: str, value: float, step: int) -> None:
        """
        Log a scalar

        Parameters
        ----------
        tag : str
            Name of the parameter.
        value: float
            Scalar to log.
        step:
            Position in the log
        """
        pass
