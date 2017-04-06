Badi H. Baltagi and Jing Li, "Further Evidence on the Spatio-Temporal
Model of House Prices in the United States", Journal of Applied
Econometrics, Vol. 29, No. 3, 2014, pp. 515-522.

Data used in this paper are from a variety of sources. The housing
price index data were obtained from the website of the Federal Housing
Finance Agency at

  http://www.fhfa.gov/Default.aspx?Page=87
  
The data on per capita disposable income were gathered from the
regional economic data published by the Bureau of Economic Analysis at

  http://www.bea.gov/itable
  
The consumer price index comes from the individual regional offices of
the Bureau of Labor Statistics at

  http://www.bls.gov/bls/regnhome.htm. 

All data files are ASCII files in DOS format. They are zipped into two
zip files. Unix/Linux users should use "unzip -a".

The file bl-MSA.zip contains data used to produce MSA level estimates.
The file bl-states.zip contains data for state level analysis. See
below for a description of the variables in the data files.

msa_cd		MSA code (identifier)
st_cd		state code (identifier)
year		year
lhpi_real	log of real housing price index
lpci_real	log of real per capita disposible income
pgr		population growth rate
rcb		real cost of borrowing

The programs we used were made available by Takashi Yamagata. They can
also be found at:

http://www.econ.cam.ac.uk/people/emeritus/mhp1/pp10/Code_for_Table9_10_HollyPesaranYamagata2010.zip
