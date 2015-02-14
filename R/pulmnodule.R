pulmnodule <- function(age = 0, diameter = 0, cancer = 0, smoker = 0, spiculation = 0, location = 0){
  if(age < 1){
    stop("Tell me your age!")
  }
  message = '';
  prob = eval(-6.8272 + (age * 0.0391) + (diameter * 0.1274) + (cancer * 1.3388) + (smoker * 0.7917) + (location * 0.7838) + (1.0407 * spiculation))
  prob = round(100 * exp(prob)/(1 + exp(prob)),1)
  citations = "1. Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;"
  citations = paste(citations,"2. Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text ")
	if (prob >= 5){
		message = paste(message, '<style>.emphasis{color:red;font-weight:bold}</style>');
		};
	message = paste(message,'<div><span class=\'emphasis\'>', prob, '%</span> probability of cancer. For next step, consider</div><ul>');
	if (prob < 5){
		message = paste(message,'<li><span class=\'emphasis\'>CT surveillance</span> as risk is very low (ACCP 2012 rec 4.5.1.1)</li>');
		};
	if (prob >= 5){
		message = paste(message,'<li><span class=\'emphasis\'>PET scan</span> as risk is low to moderate (ACCP 2012 rec 4.2.4.1)</li>');
		};
	if (prob > 65){
		message = paste(message,'<li><span class=\'emphasis\'>Surgical diagnosis</span> as risk is high (ACCP 2012 rec 4.6.3.1.1)</li>');
		};
	message = paste(message, '</ul><div>For detail, see guidelines referenced below.</div>');
  list(message = message)
#Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;
# Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text 
}
