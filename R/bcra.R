bcra <- function(genetics = 'no', history = 'no', T1 = 0, AgeMen = 99, Age1st = 99, N_Rels = 99, N_Biop = 99, HypPlas = 99, Race = 1, pageformat = 'nci'){
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

  
  x.sub <- subset(myframe, myframe[] == 999)
  
  if(nrow(x.sub) > 0 ){
    msg <- "<div>&nbsp;</div><div style=\"text-align:center\">		<button id=\"startover\" type=\"button\" onclick=\"location.reload()\">Start over</button></div>"
    list(message = msg)
    stop("Please be sure to enter all values!")
  }  
  
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
data(pages)
msg <- subset(pages, pagename==pageformat)[, "content"]

#msg = paste(msg,'<div>5 years: ', sprintf("%.1f",absolute.risk(myframe, 1, 0)), '%.</div>')
if (myframe$T1 < 86){
  #5 years
  myframe$T2 = myframe$T1 + 5
  msg <- sub("X5X", sprintf("%.1f",absolute.risk(myframe, 1, 0)),msg)
  msg <- sub("M5M", sprintf("%.1f",0.8*absolute.risk(myframe, 1, 0)),msg)
  msg <- sub("T5T", sprintf("%.1f",absolute.risk(myframe,iloop=2)),msg)
}
if (myframe$T1 < 81){
  #10 years
  myframe$T2 = myframe$T1 + 10
  msg <- sub("X10X", sprintf("%.1f",absolute.risk(myframe, 1, 0)),msg)
  msg <- sub("M10M", sprintf("%.1f",0.8*absolute.risk(myframe, 1, 0)),msg)
}
if (myframe$T1 < 91){
  myframe$T2 = 90
  msg <- sub("X90X", sprintf("%.1f",absolute.risk(myframe, 1, 0)),msg)
  msg <- sub("M90M", sprintf("%.1f",0.8*absolute.risk(myframe, 1, 0)),msg)
  msg <- sub("T90T", sprintf("%.1f",absolute.risk(myframe,iloop=2)),msg)
}
msg <- paste(msg,"<div>&nbsp;</div><div style=\"text-align:center\">		<button id=\"startover\" type=\"button\" onclick=\"location.reload()\">Start over</button></div>", sep="")
list(message = msg)
}
