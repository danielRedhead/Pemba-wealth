# Dataset for "The interdependence of relational and material wealth inequality in Pemba, Zanzibar"

---------------------------------------------

The data were collected from a small, rural village on the island of Zanzibar to explore how a household's position within different types of social support networks was associated with material wealth and status. Social network, detailed household inventories (to estimate material wealth) and demographic data were collected from all households in the village. Results showed that (i)  materially wealthy households have most relational ties, (ii) the associations between relational and material wealth-—as well as relational wealth more generally—-are patterned by gender differences, and (iii) different forms of relational wealth have similar structural properties and are closely aligned.

## Acknowledgements

* Funding sources: This research was funded by NSF IBSS-L award no. 1743019 and
the Max Planck Institute for Evolutionary Anthropology.

## Description of the underlying data and file structure

The analysis uses nine raw data files as input, which are not shared publicly:

```
'networks.csv' - An edgelist that includes all networks used in the analysis.
'PE_documentation for SharingUnit.xlsx' - Metadata provided by MBM to help with wealth calculations.
'people_observations.csv' - A data table housing all individual-level attributes.
'people.csv' - A data table housing genealogical information.
'possession_costs.csv' - A data table housing cost estimates for all household possessions that have been enumerated.
'su_distances.csv' - A symmetric adjacency matrix that houses data on the physical distance between households in Mitini.
'su_observations.csv' - A data table housing a survey of all household possessions that have been itemised and counted.
```
* All data tables and matrices were linked through either the individual or sharing unit ID.
* Any missing data is coded as NA.

### Details for processed data used in the analyses:

* The underlying raw data described above were processed to produce the data used in the analyses. These data are openly available, and the code used to create these files can be found in "./code/process-analysis-data.R".

### networks

* These data were originally structured as a long edgelist, where each row captures a nomination from one individual (the 'personid' variable) to another (the 'alterid' variable) for a specific network layer (indexed by the 'tie' variable).
* The processing code produced 6 matrices, where each row represents a nominating household, and each column a household that has been nominated.

Details on the network files:

* 1	**su_sharing_1.csv (household-level exchange question)**: *Which households commonly help you with food? For example, people who will bring food (prepared or unprepared) to your house?*
* 2	**su_sharing_2.csv (double sampled household-level exchange question)**: *To which households do you often give food? For example, people to whom you will take food (prepared or unprepared) to their house?*		
* 3	**su_female_q.csv (female-oriented Q network)**: *With whom do you commonly work with, when you are farming, doing household tasks, or business, etc.)?*		
* 4	**su_male_q.csv (male-oriented Q network)**: *With whom do you commonly work with, when you are farming, fishing or doing business?*		
* 5	**su_female_k.csv (female-oriented K network)**: *With whom do you hang out when you are not busy, or when you would like to chat and/or get advice?*		
* 6	**su_male_k.csv (male-oriented K network)**: *With whom do you typically walk to and from mosque, or generally hang out with as a friend?*		

### Sharing unit information (sharing_unit.csv)

* **su_id**: The sharing unit ID that individuals are associated with.
*	**nwives2018**: The number of wives associated with the sharing unit.
* **su_direct_wealth**: The direct wealth calculated for the sharing unit.
* **log_wealth**: The log of the direct wealth.
* **status**: A binary indicator of whether any individual in the sharing unit is considered to have status.	Collected by asking	"*Does he/she have a position/rank/post/responsibility such as in government, in the community, or in the mosque, or are they a "regular citizen"*".
* **khazi**: A binary indicator of whether any household member belonged to the village labour cooperative.

### Details for: su_distance.csv

A matrix of physical distances between all sharing units within the village.

### Details for: su_relatedness.csv

A matrix of the average genetic relatedness between all sharing units within the village.

## Sharing/Access information

These data and code are also accessible on an open GitHub repository.

Links to other publicly accessible locations of the data:
 - [https://github.com/danielRedhead/Pemba-wealth](https://github.com/danielRedhead/Pemba-wealth)

## Code/Software

* For detailed information on methods of data collection/generation, treatment and analyses: see manuscript for details

### Requirements for analyses:

- R: https://cran.r-project.org
- STRAND: https://github.com/ctross/STRAND
- cmdstanr: https://mc-stan.org/cmdstanr/

### Packages used for data processing and visualisation:

- Rethinking: https://xcelab.net/rm/statistical-rethinking/
- iGraph: https://igraph.org/r/
- Rcolorbrewer: https://cran.r-project.org/web/packages/RColorBrewer/index.html
- tidyverse: https://www.tidyverse.org

### Workflow

In R, set the working directory to that containing this readme file. On Mac, for example, you could say

```
setwd('~/Desktop/Pemba-wealth')
```

Check to see if you're in the right place by typing dir() and see whether this readme file is present.

Raw data were then cleaned, standardised and wrangled into a format for the analyses presented in the manuscript. This was done using "process-analysis-data.R", which can be found in the "code/" folder in this repository.

Once these data were in the correct format, we ran our analyses. The code for the analyses can be found in "analyses-log-wealth.R" in the "code/" folder. Analyses were performed on the high performance computational cluster available in the Department of Human Behavior, Ecology and Culture at the Max Planck Institute for Evolutionary Anthropology. We performed further analyses to examine the sensitivity of our estimates to model specification. These can be found in the "supplementary-analyses/" sub-folder in "code/".  

When all analyses were run, we generated interpretable results tables and figures using the "descriptives-plots-log-wealth.R" file found in "code/". Note that we cannot store the data generated from our analyses, or the figures that we produced, as they are too large to house on GitHub. You may, however, reproduce our analyses and results by running the above mentioned files.
