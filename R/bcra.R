bcra <- function(genetics = 'no', history = 'no', T1 = 0, AgeMen = 99, Age1st = 99, N_Rels = 99, N_Biop = 99, HypPlas = 99, Race = 1){
T1 <- as.numeric(T1)
ID = 1
T2 = T1 + 5
mytable <- cbind(ID, T1, T2, N_Biop, HypPlas, AgeMen, Age1st, N_Rels, Race)
myframe <- as.data.frame(mytable)

  #if(myframe$T1 < 1){
  #  stop("Tell me your age!")
  #}

  if(history == 'yes'){
    stop("This tool cannot calculate breast cancer risk accurately for women with a medical history of any breast cancer or of DCIS or LCIS.")
  }
  
  if(genetics == 'yes'){
    stop("Other tools may be more appropriate for women with known mutations in either the BRCA1 or BRCA2 gene, or other hereditary syndromes associated with higher risk of breast cancer.")
  }

#myframe$T2 <- myframe$T1 + 5
msg = paste("<h3>Your risk of invasive breast cancer in 5 years</h3><div>",sprintf("%.1f",absolute.risk(myframe)), '%.</div>')

msg = paste(msg,"</ul></li></ul>")
#msg <- mytable
list(message = msg)
}