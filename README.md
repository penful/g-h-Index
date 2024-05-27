Supplementary information file for the manuscript titled:

"New Risk Measures Derived from the g- and h-Index" by

- F. Bartolucci (University of Perugia, IT)

- F. Pennoni (University of Milano-Bicocca, IT)

- F. Cortese (Institute for Applied Mathematics and Information Technologies, National Research Council of Italy, Milan IT)  

The code is written/evaluated in R with the following software versions:

R version 4.3.2 (2024-04-10)
Platform: Sonoma 14.4.1
Running under: Apple M3 Pro

attached base packages: utils version 4.3.1, base version 4.3.1     

other attached packages:
tidyquant version 1.0.7, dplyr version 1.1.3, ggplot2 version 3.5.
       
This folder contains the following data and files that can be used to reproduce the analyses and figures of the manuscript. The data are only an example to run the code.  

#----
/RCode/
   
   Main implemented functions: 

DD.R  		 —>  computes the three drawdown types illustrated in the paper 

gh_index.R 	 —>  compute the proposed g- and h-index 

port_weights.R  —> computes the portfolio weights, and relative portfolio returns according to  Maillard et al. (2010) 

#-----
/Empirical illustration/

File to reproduce figures, tables, and the results similar to those presented 
in Section 3 of the paper (data are only to run the proposed functions, 
and they are not equivalent to those used in the paper, 
see the links provided in the paper to download the exact time series and periods.)

Illustration.R ---> R script to perform the analysis reported in Section 3 (including descriptive figures) 

example_data.RData  --> workspace file containing  some data used in the illustrative example and required for running Illustration.R 
