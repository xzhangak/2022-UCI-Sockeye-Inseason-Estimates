The R scripts (model function.R and run the model.R) are modeling for the inseason estimates of UCI sockeye run size. The model integrates test fishery (OTF data), harvest, escapement, and run timing data of sockeye salmon to predict the UCI sockeye run size, which includes Kenai, Kasilof, and Susitna stocks. They includes:
1) Fit inseason OTF data (observed CPUE) to historical run timing curves and select the 5 historical curves that fit the inseason OTF data best with least MSE; 
2) Use the top 5 models to estimate the acumulative CPUE by the end of season; 
3) With updated inseason run sizes and acumulative CPUE(OTF data) to estimate the passage rate; 
4) Estimated total run size is calculated by multiplying estimated acumulative CPUE by end of season by the passage rate; 
5) The model outputs five estimates of total run size from the 5 best models, which is written as a cvs data file.
6) The Run the model.R script also includes estimates of Kneai sockeye part and two figures of UCI and Kenai sockeye estimates across the inseason. 

For the details of the model and methods, go to the Mundy (1979) and ADF&G reports, for example, Willett (2006) in the Document fold. ADF&G used quite a few old programs that were combined with Fortran code, SAS code, Java and Excel spreadsheets to do the inseason estimates. Now the inseason estimates can be done with this R program. It makes doing inseaosn estiamtes easier and less human errors. During the inseason of 2022 the R program worked with ADF&G old programs side by side to compare results. The R programs sucessfully produced the same results as ADF&G old programs did, except for some rounding errors. 
