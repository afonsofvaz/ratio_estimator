# ratio_estimator

## Description
This repository contains the codes used in the experiments related to paper named “Quantification under prior probability shift: the ratio
estimator and its extensions” (https://arxiv.org/abs/1807.03929) by Afonso Fernandes Vaz, Rafael Izbicki and Rafael Bassi Stern. We perform three experiments, aiming at:

1. Comparing different quantification methods (including combined and multiclass estimators) under prior shift scenario as well as evaluate the confidence interval properties;
2. Evaluating perfomance of our goodness-of-fit test;
3. Comparing the ratio regression estimator against the classify and count approach.

We organize each experiment in its own folder of the repository as discussed bellow.

## Usage
The are three folders in this repository:

- ratio;
- goodness_of_fit;
- ratio_regression.

Each of these folders is related to an experiment mentioned in the decription. Moreover, there is a file named `ratio_estimator.R` which you can use to replicate our results more easily. If you don't use the RStudio environment, you might set your working directory into the root folder of this repository before runnig any script. 

The packages used in our experiments can be found in the file `dependencies.R`. Make sure that you have all these packages, otherwise you should install them.

Next, we present the steps to perform each experiment.

### ratio
1. Run `sample_generation.R`. It loads all dataset and generates samples from them by simulating the various prior shift settings.
2. Run `quantification.R`. It applies the discussed methods over the samples generated in the previous step.
3. Run `analysis.R`. It generates the tables (latex code) and plots related to the Section 2 of our work (except for the multiclass experiment). These plots will be saved in the folder `ratio_estimator/ratio/outputs/plots`. 
4. Finally, to perform the multiclass experiment, you should run `experiment_multiclass.R`. It also will save the plots in the plots folder.

### ratio_regression
1. Run `simulation.R`.It generates artificial samples and computes the ratio regression and classify and count estimators.   
2. Run `analysis.R`. It loads the results and plots the figures from Section 3. These plots are saved in the folder `ratio_estimator/ratio_regression/outputs/plots`.

### goodness-of-fit
1. The `goodnessOfFit_*.R` files generate data and compute the power of the goodness-of-fit hypothesis test for each setting. 
They should be run first.
2. `Analysis_power.R` generates the plots for the power analysis. These plots are saved in the folder `ratio_estimator/goodness-of-fit/plots`.


If you have any questions (or other feedback), please send an e-mail to afonsofvaz@gmail.com.
