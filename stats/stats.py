from conf import DEMO_PATH, DATA_PATH, UN_OUT_DIR, DEMO_TRAITS
from utils.data_splitter import DataSplitter

for fold_n in range(5):
    ds = DataSplitter(DATA_PATH, DEMO_PATH, out_dir=UN_OUT_DIR)
    pandas_dir_path, _, _, _ = ds.get_paths(fold_n=fold_n)

    user_groups_all_traits = dict()
    for trait in DEMO_TRAITS:
        user_groups = ds.get_user_groups_indxs(pandas_dir_path, trait)
        print('Statistics for fold number <{}> and demographic trait <{}>'.format(fold_n, user_groups[0].type))
        tr_num = sum([len(x.tr_idxs) for x in user_groups])
        vd_num = sum([len(x.vd_idxs) for x in user_groups])
        te_num = sum([len(x.te_idxs) for x in user_groups])

        print('Training data:')
        for user_group in user_groups:
            print('{} - {:3d} users ({:.2f}%)'.format(user_group.name, len(user_group.tr_idxs),
                                                      len(user_group.tr_idxs) * 100 / tr_num))
        print('Validation data:')
        for user_group in user_groups:
            print('{} - {:3d} users ({:.2f}%)'.format(user_group.name, len(user_group.vd_idxs),
                                                      len(user_group.vd_idxs) * 100 / vd_num))
        print('Test data:')
        for user_group in user_groups:
            print('{} - {:3d} users ({:.2f}%)'.format(user_group.name, len(user_group.te_idxs),
                                                      len(user_group.te_idxs) * 100 / te_num))
