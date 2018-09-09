# Call this from all the other scripts
packages <- c('ggplot2', 'lattice', 'penalized', 'mice', 'rjags', 'corrplot', 'ggfortify',
 'reshape2', 'stats', 'dlm', 'grid', 'gridExtra','MCMCpack', 'stargazer', 'MASS', 'xtable')
lapply(packages, require, character.only = TRUE)
