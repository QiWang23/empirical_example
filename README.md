# empirical_example
Mplus code for empirical study

application_example.dat is the dataset.

cfa_ho1_uli.out and cfa_sa1.out include the dimensionality tests, and reliability estimates (omega coefficients).

The Mplus input files cfa_long_ho_sa_config.inp, cfa_long_ho_sa_metric.inp, and cfa_long_ho_sa_scalar.inp contain the code used to test longitudinal measurement invariance.
 
lclpm-f_uli_ho_sa_without_intercept.out provides the approach for fitting the L-CLPM-F without random intercepts. 

lclpm_ci_ho_sa_without_random_intercept.out is the Mplus code for L-CLPM-CI without random intercept.

lclpm-f_uli_ho_sa_metric.out is the Mplus code for L-RI-CLPM-F.

lclpm_ci_ho_sa.out is the Mplus code for L-RI-CLPM-CI.

causal_predominance_function.r provides a function for testing whether causal predominance is statistically significant using the delta method. To use this function, simply run the script in R and supply the required input parameters.
