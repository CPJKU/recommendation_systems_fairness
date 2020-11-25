LEVELS = [
    1,
    3,
    5,
    10,
    20,
    50,
]

DEMO_TRAITS = [
    'gender'
]

EXP_SEED = 101315
VAE_MAX_EPOCHS = 100
VAE_LOG_VAL_EVERY = 5

# Uncontrolled experiments
# Structure is res/{ algorithm name }/{ experiment type }--{ date and time }/(val-test)/{ seed or fold_n }
LOG_VAL_STR = '../../res/{}/{}--{}/val/{}'
LOG_TE_STR = '../../res/{}/{}--{}/test/{}'

DOWN_DATA_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_regexp_since_2016_pc_gt_1_user_gte_5_song_gte_5/down_sampled_inter.txt'
DOWN_DEMO_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_regexp_since_2016_pc_gt_1_user_gte_5_song_gte_5/down_sampled_demo.txt'
DATA_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_regexp_since_2016_pc_gt_1_user_gte_5_song_gte_5/sampled_100000_items_inter.txt'
DEMO_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_regexp_since_2016_pc_gt_1_user_gte_5_song_gte_5/sampled_100000_items_demo.txt'
OUT_DIR = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_regexp_since_2016_pc_gt_1_user_gte_5_song_gte_5/data/{}/'
