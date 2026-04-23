# empirical_example
Mplus code for empirical study

cfa_ls.out and cfa_sa.out include the dimensionality tests, scale-level invariance tests, and reliability estimates (omega coefficients).

lclpm_f.out is the Mplus code for LCLPM-F, and lclpm_ci.out is the Mplus code for LCLPM-CI.
 
lclpm_without_random_intercept_v1.out and lclpm_without_random_intercept_v2.out provide two alternative approaches for fitting the LCLPM-F without random intercepts. One approach is based on within-person factors, whereas the other is based on latent factors. Both approaches produced identical results.

lclpm_ci_without_random_intercept is the Mplus code for LCLPM-CI without random intercept.

causal_predominance_test.r provides a function for testing whether causal predominance is statistically significant using the delta method. To use this function, simply run the script in R and supply the required input parameters.
