The R script is modeling for the inseason estimate of UCI sockeye run size. It includes:
1) Fit inseason OTF data (observed CPUE) to historical run timing curves to select the top 5 historical curves with least mean squared errors; 
2) Use top 5 models to estimate the acumulative CPUE by end of season; 
3) With updated inseason run sizes and acumulative CPUE to estimate the passage rate; 
4) Estimated total run size is calculated by multiplying acumulative CPUE by passage rate; 
5) Five estimates of total run size from the 5 best models are the model output, which is written as a cvs data file.

For the details of modeling and methods, go to the Mundy (1979) and ADF&G reports, for example, Willett (2006) in the document fold. The ADF&G has been used combination of Fortran, SAS, and Excel spreadsheet to do the modeling. 
