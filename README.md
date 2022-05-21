A Hybrid Feature Selection and Generation Algorithm for Electricity Load Prediction using Grammatical Evolution 
===============================================================================================================

This repo contains the implementation and results from the paper: 
A. M. D. Silva, F. Noorian, R. I. A. Davis and P. H. W. Leong, "[A Hybrid Feature Selection and Generation Algorithm for Electricity Load Prediction Using Grammatical Evolution](http://phwl.org/assets/papers/fs_icmla13.pdf)," 2013 12th International Conference on Machine Learning and Applications, 2013, pp. 211-217, [doi: 10.1109/ICMLA.2013.125](https://doi.org/10.1109/ICMLA.2013.125)

Files
-----
- autoreg_models.R: Auto regressive models testing
- ge_tests.R: Learners, Features and GE results evaluator

- elecdata.R: Loading and conditioning Electricity data
- eleceval.R: Day-Ahead and Month-Ahead kernel based predictor and evaluator
- gram_func.R: Functions used in Grammar
- utils.R: Commonly used Grammar

- elecdata.RData: Contains electricity in R Native format
- ga_res_ohlc.RData: Contains HLC grammar feature names
- ga_res_wvlt.RData: Contains Wavelet grammar feature names
