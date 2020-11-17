LEVELS = [
    1,
    3,
    5,
    10,
    20,
    50,
]

VAE_SEED = 101315
VAE_MAX_EPOCHS = 500
VAE_LOG_VAL_EVERY = 5

# Uncontrolled experiments
# Structure is res/un/{ algorithm name }/{ date and time }/{ seed or fold_n }
UN_LOG_VAL_STR = '../../res/un/{}/{}/val/{}'
UN_LOG_TE_STR = '../../res/un/{}/{}/test/{}'

DATA_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_since_2016_pc_gt_1_user_gte_5_song_gte_50/sampled_inter.txt'
DEMO_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_since_2016_pc_gt_1_user_gte_5_song_gte_50/sampled_demo.txt'
UN_OUT_DIR = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_since_2016_pc_gt_1_user_gte_5_song_gte_50/data/{}/'

DEMO_TRAITS = ['gender']
