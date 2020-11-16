
UN_SEEDS = [
    6547893,
    #2034976,
    #2345303,
    #7887871,
    #1023468,
    #8812394,
    #2132395,
    #4444637,
    #7192837,
    #6574836,
]


LEVELS = [
    1,
    3,
    5,
    10,
    20,
    50,
]

VAE_SEED = 101315
VAE_MAX_EPOCHS = 100
VAE_LOG_VAL_EVERY =5

# Uncontrolled experiments
# Structure is res/un/{ algorithm name }/{ date and time }/{ seed }
UN_LOG_VAL_STR = '../../res/un/{}/{}/val/{}'
UN_LOG_TE_STR = '../../res/un/{}/{}/test/{}'

DATA_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_since_2016_pc_gt_1_user_gte_5_song_gte_50/user_song_since_2016_pc_gt_1_user_gte_5_song_gte_50.txt'
DEMO_PATH = '/share/cp/datasets/LFM/LFM-2b/IPM/datasets/user_song_since_2016_pc_gt_1_user_gte_5_song_gte_50/user_demographics.txt'
UN_OUT_DIR = '../../data/seed/'

DEMO_TRAITS = ['gender']