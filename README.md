# Investiating Gender Fairness of Recommendation Algorithms in the Music Domain

This repository accompanies the paper "Investigating Gender Fairness of Recommendation Algorithms in the Music Domain" by Alessandro B. Melchiorre, Navid Rekabsaz, Emilia Parada-Cabaleiro, Stefan Brandl, Oleg Lesota, and Markus Schedl.

## Installation

Create the environment with

~~~
conda env create -f gender_fair.yml
conda activate gender_fair
~~~

and then setup the project with
~~~
python3 setup.py develop
~~~

## Setup the data
Data paths must be set in the `conf.py` file. In particular:
- `DATA_PATH`: points to a .tsv file with columns [`user_id`,`track_id`,`play_count`].
- `DEMO_PATH`: points to a .tsv file with columns [`user_name`,`country`,`age`,`gender`,`timestamp`].
- `TRACKS_PATH`: points to a .tsv file with columns [`track_artist`,`track_name`].
- `DOWN_DATA_PATH`: similar to `DATA_PATH` but contains the balanced dataset.
- `DOWN_DEMO_PATH`: similar to `DEMO_PATH` but contains the balanced dataset.
- `OUT_DIR`: where the processed datasets (e.g train/val/test data for fold 0).
## Run an experiment (Train + Validation)
`algorithms` contains all the algorithms used in the project. Move to the folder of the algorithm of your choice (e.g. `slim`)
~~~
cd algorithms/slim
~~~
and with the active env, run the experiment
~~~
python3 main_slim.py --experiment_type <experiment_type>
~~~
`experiment_type` can be:
- `standard`: original data
- `up_sample`: debiasing method where the minority group in the training data is upsampled (as specified in the paper)
- `down_sample`: balanced data is used instead where users from the majority groups have been discarded

for `vae` there is also the parameter `gpu` which specifies which gpu to use.
## Check the experiment status
Experiment status and results will be saved in the `res` folder (created when the first experiment is run).
 
To check the status of your experiment, move in the folder of the algorithm you are running (e.g. `slim`):
~~~
cd res/slim/<experiment_type>-<timestamp_start_experiment>/val
~~~

For a broad overview of the experiments, you can just use `tensorboard` in the folder.
~~~
tensorboard --logdir=./
~~~
You can also jump to a specific test fold by selecting one of the folders there (numbered from 0 to 4 for 5-folds cross validation).
E.g. monitor how the experiment is going on fold 0.
~~~
cd 0
tensorboard --logdir=./
~~~

## Running evaluation
Experiment status and results will be saved in the `res` folder (created when the first experiment is run).

To run the evaluation when an experiment has ended, move in the folder of the algorithm you run (e.g. `slim`)
~~~
cd algorithms/slim
~~~
and the run the evaluation with:
~~~
python3 eval_slim.py --experiment_type <experiment_type>
~~~
where `experiment_type` parameter is defined as above in "Check the experiment status".

**NOTE:** Evaluation requires loading the trained model (except for `pop`). The code will look in the `res` folder for the best configuration automatically.
It is needed to update the path to the trained model by providing the `timestamp_start_experiment` in the evaluation code.
E.g. for `eval_slim.py` you can find this at the top of the code:
~~~python
best_configs = {
    'standard': '2020-11-24 19:43:47.801230',
    'up_sample': '2020-11-24 19:44:32.188924',
    'down_sample': '2020-11-24 19:45:15.744244'
}
~~~
Set the timestamp of your experiment in the dictionary (in the correct `experiment_type` field) for running the evaluation.

E.G. `slim` is started at `2020-11-24 19:43:47.801230 with
~~~
python3 main_slim.py --experiment_type standard
~~~
in `res` there will be a folder `slim/standard-2020-11-24 19:43:47.801230`.
After the training and validation, place `2020-11-24 19:43:47.801230` in the filed `standard` of the `best_configs` dictionary in `eval_slim.py`

## Results and best configurations
Experiment status and results will be saved in the `res` folder (created when the first experiment is run).

### Results
To check the results of your experiment, move in the folder of your experiment (e.g. `slim`):
~~~
cd res/slim/<experiment_type>-<timestamp_start_experiment>/test
~~~
For a broad overview of the experiments, you can just use `tensorboard` in the folder.
~~~
tensorboard --logdir=./
~~~

The results are saved separately for each fold in the relative folder.
In each fold folder you will find:
~~~
full_metrics.pkl
full_raw_metrics.pkl
~~~
Both files contain a dictionary where the key is the evaluated metric function and the value is the result of the metric function on the test data.
- `full_raw_metrics.pkl` contains vectors as values of the dictionary (results are **not** aggregated).
- `full_metrics.pkl` contains floats as values of the dictionary (results are aggregated with mean)

### Best configurations
The best configuration for each algorithm and for each fold is located in:
~~~
cd res/slim/<experiment_type>-<timestamp_start_experiment>/val/<fold-number>/best_config.pkl
~~~