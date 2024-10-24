All the statistical analyses were conducted in R software version 4.2.2, and statistical significance was defined as P<0.05.

# Analysis of the differential gut microbiota related to dyslipidemia
After excluding viruses, protozoa and fungi, we focused on the gut microbiota with minimum mean relative abundance of 0.01% in at least 10% of samples. We calculated α diversity measured by Simpson, Shannon, and Pielou entropy. Beta diversity was calculated by principal coordinate analysis (PCoA) and permutational multivariate analysis of variance (PERMANOVA). Microbiome Multivariable Associations with Linear Models (Maaslin) was used to identify the differential gut microbiota and function related to dyslipidemia and its subtypes employing the R package “MaAslin2”. All high-dimensional tests were corrected for multiple comparisons by controlling the false discovery rate using the Benjamini–Hochberg method, with q values less than 0.25 considered statistically significant.

# Mediation analysis
Mediation analyses were performed using the mediate function in the R package “mediation”, adjusted for sex, age, and ethnicity. The mediation effect of differential microbial species in the associations between lifestyle behavior score and dyslipidemia was identified using mediation analyses. The mediated proportion was determined by dividing the mediated effect by the total effect.

# Analysis of the dyslipidemia-related metabolites
R package “MetaboAnalystR” was used to analyze the differential metabolites between the participants with and without dyslipidemia. Dyslipidemia-related metabolites were screened by orthogonal partial least squares-discriminant analysis (OPLS-DA) and volcano maps, with VIP>1, log2(FC)>2 and P <0.05. Spearman's correlation coefficient was calculated to determine the associations between dyslipidemia-related metabolites and the mediating microbiotas. The biochemical pathways of differential metabolites were identified using the KEGG database and then classified based on their involvement in these pathways. To demonstrate as more relevant correlations, absolute correlation coefficient >0.25 was shown in network plot.

# Sensitivity analysis
To evaluate the robust of the findings, we performed two sensitivity analyses in discovery cohort with the following strategies: 1) excluding those who were ethnic minorities, since the minorities have different lifestyles or gut microbiome with the Han people, and 2) excluding those with hyperlipidemia medications that may affect the gut microbiota.

# External validation
In the validation cohort, we replicated the associations between potential mediating species identified in the discovery cohort and hyperlipidemia risk. Mediation analysis, multivariable logistic regression, and Spearman’s correlation were as described for the discovery cohort.