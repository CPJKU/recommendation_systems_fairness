# Demographic Bias in Music Recommendation

## Installation

Create the environment with

~~~
conda env create -f demo_bias.yml
conda activate demo_bias
~~~

and then setup the project with
~~~
python3 setup.py develop
~~~

## Run an experiment
`algorithms` contains all the algorithms used in the project. Move to the folder of the algorithm of your choice (e.g. `slim`)
~~~
cd algorithms/slim
~~~
and with the active env, run the experiment
~~~
python3 main_slim_uncontrolled.py
~~~

## Check the experiment status
Experiment status and results will be saved in the `res` folder (created when the first experiment is run).
 
To check the status of your experiment, move in the folder of your experiment (e.g. `slim`):
~~~
cd res/un/slim/<timestamp-start-experiment>-<dataset-used-(inter.txt)>/val
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

## Results and best configurations
Experiment status and results will be saved in the `res` folder (created when the first experiment is run).

### Results
To check the results of your experiment, move in the folder of your experiment (e.g. `slim`):
~~~
cd res/un/slim/<timestamp-start-experiment>-<dataset-used-(inter.txt)>/test
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
Both files contain a dictionary where the key is the metric function evaluated and the value is the result of the metric function on the test data.
- `full_raw_metrics.pkl` contains vectors as values of the dictionary (results are **not** aggregated).
- `full_metrics.pkl` contains floats as values of the dictionary (results are aggregated with mean)

### Best configurations
The best configuration for each algorithm and for each fold is located in:
~~~
cd res/un/slim/<timestamp-start-experiment>-<dataset-used-(inter.txt)>/val/<fold-number>/best_config.pkl
~~~