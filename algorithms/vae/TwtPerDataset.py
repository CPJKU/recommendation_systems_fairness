import os

from scipy import sparse
from torch.utils.data import Dataset


class TwitPers(Dataset):
    '''
    Twitter Personality Dataset
    '''

    def __init__(self, scipy_dir_path, pandas_dir_path=None, uids_dic_path=None, which='train', transform=None):

        self.scipy_dir_path = scipy_dir_path
        self.pandas_dir_path = pandas_dir_path
        self.uids_dic_path = uids_dic_path
        self.which = which

        if self.which == "train":
            self.data = sparse.load_npz(os.path.join(self.scipy_dir_path + "sp_tr_data.npz"))
            self.targets = self.data
        elif self.which == "val":
            self.data = sparse.load_npz(os.path.join(self.scipy_dir_path + "sp_vd_tr_data.npz"))
            self.targets = sparse.load_npz(os.path.join(self.scipy_dir_path + "sp_vd_te_data.npz"))
        elif self.which == "test":
            self.data = sparse.load_npz(os.path.join(self.scipy_dir_path + "sp_te_tr_data.npz"))
            self.targets = sparse.load_npz(os.path.join(self.scipy_dir_path + "sp_te_te_data.npz"))
        else:
            raise Exception("Dataset string entered is not valid! Please choose from [train,val,test]")

        self.data = self.data.astype("float32")
        self.targets = self.targets.astype("float32")
        self.transform = transform

    def __len__(self):
        return self.data.shape[0]

    def __getitem__(self, idx):
        x_sample = self.data[idx, :].toarray().squeeze()
        if self.transform:
            x_sample = self.transform(x_sample)

        y_sample = self.targets[idx, :].toarray().squeeze()

        return x_sample, y_sample
