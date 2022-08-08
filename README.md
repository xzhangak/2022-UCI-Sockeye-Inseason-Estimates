The R script is modeling for the inseason estimates of UCI sockeye run size. It includes:
1) Fit inseason OTF data (observed CPUE) to historical run timing curves and select the 5 historical curves that fit the inseason OTF data best with least MSE; 
2) Use the top 5 models to estimate the acumulative CPUE by the end of season; 
3) With updated inseason run sizes and acumulative CPUE to estimate the passage rate; 
4) Estimated total run size is calculated by multiplying acumulative CPUE by passage rate; 
5) Five estimates of total run size from the 5 best models are the model output, which is written as a cvs data file.

The ADF&G has been used quite a few old programs combined with Fortran code, SAS code, and Excel spreadsheets to do the inseason estimates. The R script was veried in 2022 as it worked in July inseason with the old programs side by side. The R programs sucessfully produced the same results as ADFG old programs, except for some rounding errors. For the details of the model and methods, go to the Mundy (1979) and ADF&G reports, for example, Willett (2006) in the Document fold. 
