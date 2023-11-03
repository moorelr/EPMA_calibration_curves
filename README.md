# EPMA_calibration_curves

This is an R script for calculating empirical calibration curves for EPMA data.  On second thought, it could be used for many types of compositional data.

## Background

There's been some commentary in the microprobe community about the use of natural standards (e.g. Smithsonian Microbeam Standards) and issues with variability and heterogeneity.  One way to approach this problem is to use empirical calibration curves to assess how much uncertainty is constrained by the standards we have available.  This doesn't really solve the problem, but it provides a robust way to estimate standards-based uncertainty which is better than "burying one's head in the sand" and/or overestimating the degree of precision provided by conventional microprobe analyses.  Also, this provides a good context for comparing different analytical methods that use different hardware, background corrections, or matrix effect calculations.

## How to use

- Create a tidy table of elemental compositions with a "Comment" column for sample names and a "flag" column with one of two strings to indicate which samples are standards and which samples are unknowns
- Run the R script from the same directory containing the input file
- Profit.

## TODO:

- Write a white paper or something.
