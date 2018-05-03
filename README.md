# R_functions

ordered_dot_plot():

Vizualise a distribution of data with ordered dots. Function will take a data frame, matrix, or list of vectors as input. Median values are indicated by thick red lines, 1st and 3rd quartiles are indicated with dotted gray lines. Either line set can be turned off with median_line or iqr_line arguments. Spread indicates distance of dot distributions from one another. Smaller spread = closer together, do not use spread > 0.5.

Example plot: IC50.ordered_dot_plot.pdf (data from: Klijn et al., 2015, Nature Biotech)
