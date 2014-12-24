pulmnodule <- function(age = 0, diameter = 0, cancer = 0, smoker = 0, spiculation = 0, location = 0){
  if(age < 1){
    stop("Tell me your age!")
  }
  prob = eval(-6.8272 + (age * 0.0391) + (diameter * 0.1274) + (cancer * 1.3388) + (smoker * 0.7917) + (location * 0.7838) + (1.0407 * spiculation))
  prob = round(100 * exp(prob)/(1 + exp(prob)),1)
  citations = "1. Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;"
  citations = paste(citations,"2. Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text ")
  list(
    message = paste(prob, '% probability of cancer. For management options of <a href=\"#\" class=\"hastip\">indeterminant nodules</a><div class=\"tooltiptext\">\"a nodule that is not calcified in a benign pattern or that does not have other features strongly suggestive of a benign etiology, such as intranodular fat that is pathognomonic of hamartoma or a feeding artery and vein typical for arteriovenous malformation\" - ACCP Guidelines. Chest 2013 <a href=\"http://pubmedcentral.gov/PMC3749714\">PMC3749714</a></div>, see 2013 ACCP Guidelines at <a href=\"http://pubmedcentral.gov/PMC3749714\">PMC3749714</a>.')
  )
#Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;
# Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text 
}
