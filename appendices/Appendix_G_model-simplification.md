Steps for model simplification
==============================

The mixed effects model starts with maximal random effects structure
(see "Appendix_E_analysis_pipeline").

In case of sampling problems during the model fitting procedure, we will
simplify the random effect structure of the model sequentially as follows,
keeping the maximal model that does not result in sampling issues (see also
Barr, Levy, Scheepers, & Tily, 2013):

1. Force the correlation parameters between all random effects to be zero.
   (see https://rdrr.io/cran/brms/man/brmsformula.html)
2. Prune main effect of movement by items.
3. Prune interaction random slope by subjects.
5. Prune main effect of movement by subjects
6. Prune main effect of word type by subjects
7. Remove item random effects by items altogether
8. Remove item random effects by subjects altogether
