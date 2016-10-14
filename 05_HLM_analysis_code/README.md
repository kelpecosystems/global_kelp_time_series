From Krumhansl, K.A., Okamoto, D.K., Rassweiler, A., Novak, M., Bolton, J.J., Cavanaugh, K.C., Connell, S.D., Johnson, C.R., Ling, S.D., Micheli, F., Norderhaug, K.M., Pérez-Matus, A., Sousa-Pinto, I., Reed, D.C., Salomon, A.K., Shears, N.T., Wernberg, T., Anderson, R.J., Barrett, N.S., Buschmann, A.H., Carr, M.H., Caselle, J.E., Derienne, S., Edgar, G.J., Edwards, M., Estes, J.A., Goodwin, C., Kenner, M.C., Kushner, D.J., Moy, F.E., Nunn, J., Steneck, R., Vasquez, J.A., Watson, J., Witman, J.D., Byrnes, J., In Press. Global patterns of kelp forest change over the past half-century. **PNAS**.

## Introduction 

This folder contains all of the code required to run the hierarchical linear model analyses in R and return basic figures. The core file is the "02_HLM_Analysis.R" file but loads other files in the folder.   Thus one will need to have other files referenced in the working directory specified.  

## Requirements

These analyses require the following dependencies: 

For R: 
rstan,plyr,gdata,ggplot2,parallel,grid,coda,reshape2,dplyr

Other software:
Stan - for installation see https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

## Running the code

1. Open the "02_HLM_Analysis.R" file
2. Ensure the packages that are loaded at the onset are installed
3. Set the working directory appropriately
4. Ensure the files that are being sourced/loaded/read are in the working directory
5. Run the script. 

Note that the model is potentially run for four different geographic aggregations: 
* World (one group, presented in the paper)
* Realm (not presented in the paper, currently not included)
* Province (not presented in the paper, currently not included)
* Ecoregion (smallest groupings, presented in the paper)

and two different temporal inclusions:
* Minimum three years per dataset (presented in the paper)
* Minimum three points per dataset

There are places in the code where one makes choices about which of the above to use


## Contact

Analytical code was primarily written by D.K. Okamoto. Inquiries can be addressed to dokamoto@sfu.ca. 
