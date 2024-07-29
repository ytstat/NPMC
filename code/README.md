## Code for simulations
`logistic_model1_check.R` and `knn_model1_check.R` are used to produce the assessment results of strong duality and feasibility, in Figure 2.

`knn.R`, `lda.R`, `logistic.R`, and `nnb.R` are used to produce the numerical results for simulation cases 1-4 in the paper. In each code file, the corresponding function class such as kNN or LDA is considered and equipped by different NP methods and benchmarks.

## Code for real data studies
The code for pre-processing the LendingClub dataset is in `lc_loan_data_preparation.R`, and the other code for producing the numerical results is in the folder "lendingclub".

The code for running experiments on the dry bean dataset is `beans.R`.

The code for running experiments on the Statlog(Landsat satellite) dataset is `sat.R`.

The code for running experiments on the dementia dataset is `dementia.R`.

## Code for producing the figures
The code for producing the figures in the paper can be found in a separate folder "drawing".
