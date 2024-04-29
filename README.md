Reproducibility Guidance
================

This GitHub repository contains the code and the guidance to reproduce the numerical results in the paper "Neyman-pearson multi-class classification via cost-sensitive learning" by Ye Tian and Yang Feng.

## Description of the code
The code for simulations and real data studies are stored in two separate folders. There is another folder storing the code for producing the empirical plots in the paper, where we mention the related figure number for each produced plot in the code files.

## Datasets
We include some small datasets in the folder "data". The original LendingClub dataset is very large, hence we include a Kaggle link for that and include the code to pre-process the original data.

## Packages
All algorithms described in the paper have been implemented in the R package "`npcs`", which is available on CRAN. We added some functions in the second version of the paper, and these functions haven't been added to the public CRAN version (0.1.1) of the package. We will update the CRAN version to 0.2.0 in a few weeks. Before that, we include the experimental version which we used to produce the results of the current paper in a separate folder "package". The interested readers can install the R package manually in R by downloading the `.tar.gz` file and run `intall.packages(".../npcs_0.2.0.tar.gz", repos = NULL, source = TRUE)` by replacing `...` with the local path.

## Code running
We ran all experiments on the HPC cluster and include all the `.sh` files in the "code" folder. Each replication was done by setting up a unique random seed (1-500 for 500 replications, 1-200 for 200 replications), which will produce an individual `.RData` output. Since the number of outputs are numerous due to the large number of replications we have done for each simulation and real data study, we did not include the original output files in this repository.


## References
Tian, Y., & Feng, Y. (2021). "[Neyman-pearson multi-class classification via cost-sensitive learning](https://arxiv.org/abs/2111.04597)." arXiv preprint arXiv:2111.04597.

Gentleman, Robert, and Duncan Temple Lang. “[Statistical Analyses and
Reproducible
Research](http://biostats.bepress.com/cgi/viewcontent.cgi?article=1001&context=bioconductor).”
(2004).
