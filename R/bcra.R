bcra <- function(genetics = 'no', history = 'no', T1 = 0, AgeMen = 99, Age1st = 99, N_Rels = 99, N_Biop = 99, HypPlas = 99, Race = 1){
  myframe <- data.frame(
    ID = 1,
    T1 = as.numeric(T1),
    T2 = as.numeric(T1) + 5,
    N_Biop = as.numeric(N_Biop),
    HypPlas = as.numeric(HypPlas),
    AgeMen = as.numeric(AgeMen),
    Age1st = as.numeric(Age1st),
    N_Rels = as.numeric(N_Rels),
    Race = as.numeric(Race)
  )

  if (myframe$N_Biop == 0)
    {myframe$HypPlas = 99} #Corrects for unintuitive data entry
  
  if(myframe$T1 < 1){
    stop("Tell me your age!")
  }

  if(history == 'yes'){
    stop("This tool cannot calculate breast cancer risk accurately for women with a medical history of any breast cancer or of DCIS or LCIS.")
  }
  
  if(genetics == 'yes'){
    stop("Other tools may be more appropriate for women with known mutations in either the BRCA1 or BRCA2 gene, or other hereditary syndromes associated with higher risk of breast cancer.")
  }

msg = NULL
msg = paste(msg,'<h3>Your risk of invasive breast cancer</h3>')
msg = paste(msg,'<div>5 years: ', sprintf("%.1f",absolute.risk(myframe)), '%.</div>')
if (myframe$T1 < 81){
  myframe$T2 = myframe$T1 + 10
  msg = paste(msg,'<div>10 years: ', sprintf("%.1f",absolute.risk(myframe)), '%.</div>')
  }
if (myframe$T1 < 71){
  myframe$T2 = myframe$T1 + 20
  msg = paste(msg,'<div>20 years: ', sprintf("%.1f",absolute.risk(myframe)), '%.</div>')
}
if (myframe$T1 < 91){
  myframe$T2 = 90
  msg = paste(msg,'<div>Lifetime this patient (age 90): ', sprintf("%.1f",absolute.risk(myframe)), '%.</div>')
  msg = paste(msg,'<div>Lifetime average risk (age 90): ', sprintf("%.1f",absolute.risk(myframe,iloop=2)), '%.</div>')
}
#msg = paste(myframe, sep = ", ", collapse = NULL)
list(message = msg)
}