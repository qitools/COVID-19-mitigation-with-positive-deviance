cardiovascularrisk <- function(age0 = 0, gender = "", ethnicity = "", smoke0 = 0, diabetes0 = 0, bprx = 0, sbp = 0, tchol0 = 0, hdl0 = 0){

  if(age0 < 1){
    stop("Tell me your age!")
  }

age = 0
age2 = 0
int1 = 0
int2 = 0
int3 = 0
int4 = 0
int5 = 0
tchol = 0
hdl = 0
sbpR0 = 0
sbpR = 0
sbpN0 = 0
sbpN = 0
smoke = 0
diabetes = 0
baseline = 0
meancoef = 0

if (bprx == 1){sbpR0 = sbp}else{sbpN0 = sbp}

#MEN
if (gender == "m")
{
if (ethnicity == "w" || ethnicity == "a" || ethnicity == "h")
{
#WHITE MEN
age = 12.344 * log(age0)
age2 = 0 #NA
tchol = 11.853 * log(tchol0)
int1 = -2.664 * log(age0) * log(tchol0)
hdl = -7.990 * log(hdl0)
int2 = 1.769 * log(age0) * log(hdl0)
if (sbpR0 > 0) {sbpR = 1.797 * log(sbpR0)}else{sbpR = 0}
if (sbpN0 > 0) {sbpN = 1.764 * log(sbpN0)}else{sbpN = 0}
smoke = 7.837 * smoke0 
int3 = -1.795 * log(age0) * smoke0
diabetes = 0.658 * diabetes0
int4 = 0
int5 = 0
meancoef = 61.18
baseline = 0.9144
}
else
{
#AA MEN
age = 2.469 * log(age0)
age2 = 0 #NA
tchol = 0.302 * log(tchol0)
int1 = 0 #NA
hdl = -0.307 * log(hdl0)
int2 = #NA
if (sbpR0 > 0) {sbpR = 1.916 * log(sbpR0)}else{sbpR = 0}
if (sbpN0 > 0) {sbpN = 1.809 * log(sbpN0)}else{sbpN = 0}
smoke = 0.549 * smoke0 
int3 = 0 #NA
diabetes = 0.645 * diabetes0
int4 = 0
int5 = 0
meancoef = 19.54
baseline = 0.8954
}
}
#WOMEN
if (gender == "f")
{
if (ethnicity == "w" || ethnicity == "a" || ethnicity == "h")
{
#WHITE WOMEN
age = -29.799 * log(age0)
age2 = 4.884 * log(age0)^2
tchol = 13.540 * log(tchol0)
int1 = -3.114 * log(age0) * log(tchol0)
hdl = -13.578 * log(hdl0)
int2 = 3.149 * log(age0) * log(hdl0)
if (sbpR0 > 0) {sbpR = 2.019 * log(sbpR0)}else{sbpR = 0}
int3 = 0 #NA
if (sbpN0 > 0) {sbpN = 1.957 * log(sbpN0)}else{sbpN = 0}
int4 = 0 #NA
smoke = -1.665 * smoke0 
int5 = -1.665 * log(age0) * smoke0
diabetes = 0.661 * diabetes0
meancoef = -29.18
baseline = 0.9665
}
else
{
#AA WOMEN
age = 17.114 * log(age0)
age2 = 0 #NA
tchol = 0.940 * log(tchol0)
int1 = 0 #NA
hdl = -18.920 * log(hdl0)
int2 = 4.475 * log(age0) * log(hdl0)
if (sbpR0 > 0) {sbpR = 29.291 * log(sbpR0)}else{sbpR = 0}
if (sbpR0 > 0) {int3 = -6.432 * log(age0) * log(sbpR0)}else{int3 = 0}
if (sbpN0 > 0) {sbpN = 27.820 * log(sbpN0)}else{sbpN = 0}
if (sbpN0 > 0) {int4 = -6.087 * log(age0) * log(sbpN0)}else{int4 = 0}
smoke = 0.691 * smoke0 
int5 = 0 #NA
diabetes = 0.874 * diabetes0
meancoef = 86.61
baseline = 0.9533
}
}

sum = age + age2 + tchol + int1 + hdl + int2 + sbpR + int3 + sbpN + int4 + int5 + smoke + diabetes
sum
#FIX FORMATTING SO HAS AT LEAST 0
prob = round (100 * (1 - baseline^exp(sum - (meancoef))),2)
prob
  
  list(
    message = paste(sprintf("%.1f",prob), '% probability of cardiovascular event within 10 years.')
  )
#Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;
# Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text 
}