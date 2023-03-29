# Pemba-wealth

Data, code and manuscript for "The interdependence of relational and material wealth inequality in Pemba, Zanzibar", published in Philosophical Transactions of the Royal Society B
----------------------------

# Requirements for analyses:

- R: https://cran.r-project.org
- STRAND: https://github.com/ctross/STRAND
- cmdstanr: https://mc-stan.org/cmdstanr/

# Packages used for data processing and visualisation:

- Rethinking: https://xcelab.net/rm/statistical-rethinking/
- iGraph: https://igraph.org/r/
- Rcolorbrewer: https://cran.r-project.org/web/packages/RColorBrewer/index.html
- tidyverse: https://www.tidyverse.org

# Instructions:

In R, set the working directory to that containing this readme file. On Mac, for example, you could say

```
setwd('~/Desktop/Pemba-wealth')
```

Check to see if you're in the right place by typing dir() and see whether this readme file is present.


The analysis uses nine raw data files as input, which can be found in the  '1-raw-data/' folder:

```
'networks.csv' - An edgelist that includes all networks used in the analysis.
'PE_documentation for SharingUnit.xlsx' - Metadata provided by MBM to help with wealth calculations.
'people_observations.csv' - A data table housing all individual-level attributes.
'people.csv' - A data table housing genealogical information.
'possession_costs.csv' - A data table housing cost estimates for all household possessions that have been enumerated.
'su_distances.csv' - A symmetric adjacency matrix that houses data on the physical distance between households in Mitini.
```
These data were then cleaned, standardised and wrangled into a format for the analyses presented in the manuscript. This was done using "process-analysis-data.R", which can be found in the "code/" folder in this repository. 

Once these data were in the correct format, we ran our analyses. The code for the analyses can be found in "analyses-log-wealth.R" in the "code/" folder. Analyses were performed on the high performance computational cluster available in the Department of Human Behavior, Ecology and Culture at the Max Planck Institute for Evolutionary Anthropology. We performed further analyses to examine the sensitivity of our estimates to model specification. These can be found in the "supplementary-analyses/" sub-folder in "code/".  

When all analyses were run, we generated interpretable results tables and figures using the "descriptives-plots-log-wealth.R" file found in "code/". Note that we cannot store the data generated from our analyses, or the figures that we produced, as they are too large to house on GitHub. You may, however, reproduce our analyses and results by running the above mentioned files. 

The project is maintained by Daniel Redhead (daniel_redhead@eva.mpg.de) and is hosted at https://github.com/danielRedhead