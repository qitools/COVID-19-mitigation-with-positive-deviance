bcra <- function(age0 = 0, gender = "", ethnicity = "", smoke0 = 0, diabetes0 = 0, bprx = 0, sbp = 0, tchol0 = 0, hdl0 = 0){

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

#Friedewald 
estLDL = tchol0 - hdl0 - 30 # 30 is typical trig of 150/5

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
int2 = 0 #NA
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
smoke = 7.574 * smoke0 
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
#FIX FORMATTING SO HAS AT LEAST 0
prob = round (100 * (1 - baseline^exp(sum - (meancoef))),2)
#Revise for withstatins
arr = prob * 0.27
withstatins = prob - arr

#Revise for SBP
if (bprx == 1){sbpR0 = 140}else{sbpN0 = 140}
#MEN
if (gender == "m")
{
if (ethnicity == "w" || ethnicity == "a" || ethnicity == "h")
{
#WHITE MEN
if (sbpR0 > 0) {sbpR = 1.797 * log(sbpR0)}else{sbpR = 0}
if (sbpN0 > 0) {sbpN = 1.764 * log(sbpN0)}else{sbpN = 0}
}
else
{
#AA MEN
if (sbpR0 > 0) {sbpR = 1.916 * log(sbpR0)}else{sbpR = 0}
if (sbpN0 > 0) {sbpN = 1.809 * log(sbpN0)}else{sbpN = 0}
}
}
#WOMEN
if (gender == "f")
{
if (ethnicity == "w" || ethnicity == "a" || ethnicity == "h")
{
#WHITE WOMEN
if (sbpR0 > 0) {sbpR = 2.019 * log(sbpR0)}else{sbpR = 0}
if (sbpN0 > 0) {sbpN = 1.957 * log(sbpN0)}else{sbpN = 0}
}
else
{
#AA WOMEN
if (sbpR0 > 0) {sbpR = 29.291 * log(sbpR0)}else{sbpR = 0}
if (sbpR0 > 0) {int3 = -6.432 * log(age0) * log(sbpR0)}else{int3 = 0}
if (sbpN0 > 0) {sbpN = 27.820 * log(sbpN0)}else{sbpN = 0}
if (sbpN0 > 0) {int4 = -6.087 * log(age0) * log(sbpN0)}else{int4 = 0}
}
}
sum = age + age2 + tchol + int1 + hdl + int2 + sbpR + int3 + sbpN + int4 + int5 + smoke + diabetes
#FIX FORMATTING SO HAS AT LEAST 0
withsbp = round (100 * (1 - baseline^exp(sum - (meancoef))),2)

#Revise for nonsmoking
if (ethnicity != "b" && gender != "f"){int3=0}
sum = age + age2 + tchol + int1 + hdl + int2 + sbpR + int3 + sbpN + int4 + 0 + 0 + diabetes
withsmokecess = round (100 * (1 - baseline^exp(sum - (meancoef))),2)
arr_smoke = prob - withsmokecess
#Revise for all
optimal = withsmokecess * 0.73
arr_both = prob - optimal

msg = paste("<h3>Your risk of cardiovascular disease in 10 years</h3><div>",sprintf("%.1f",prob), '% probability of cardiovascular event within 10 years.</div>')

#Start SVG output
if (prob >= 7.5)
#if (grepl("<li>", msg) > 0)
	{
	#msg = paste(msg, "<div>Assuming statin medications reduce your risk by 27% (3), the following is expected:</div><div>&nbsp;</div>")
	}
#Make SVG
svgheight = 150
if (smoke0 > 0){svgheight=svgheight+40}
if (sbp > 140) {svgheight=svgheight+40}
svgtext = paste("<svg x=\"0px\" y=\"0px\" width=\"420px\" height=\"",svgheight,"px\" viewBox=\"0 0 420 ",svgheight,"\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">
<!-- Scale -->
<text x=\"0\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">0%</text><text x=\"90\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">25%</text><text x=\"190\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">50%</text><text x=\"290\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">75%</text><text x=\"380\" y=\"15\" fill=\"black\" style=\"font-weight:bold\">100%</text>
<polygon points=\"0,18 400,18 400,20 0,20\"  style=\"fill:black;fill-opacity:1;stroke-width:0\"/>
<text x=\"0\" y=\"35\" fill=\"black\" style=\"\">Your current risk</text>
<polygon points=\"0,40 ", prob*4,",40 ", prob*4,",60 0,60\"  style=\"fill:red;fill-opacity:0.5;stroke-width:0\"/><text x=\"",10+prob*4,"\" y=\"55\" style=\"fill:red;font-weight:bold\">", sprintf("%.1f",prob),"%</text>\n", sep = "")
currenty = 40
if (sbp > 140)
	{
	currenty = currenty + 35
	svgtext = paste(svgtext,"<text x=\"0\" y=\"",currenty, "\" fill=\"black\" style=\"\">With blood pressure of 140</text>\n", sep = "")
	currenty = currenty + 5
	svgtext = paste(svgtext,"<polygon points=\"0,",currenty,",", withsbp*4,",", currenty, ",", withsbp*4,",", currenty+20, ",0,", currenty+20, "\"  style=\"fill:green;fill-opacity:0.5;stroke-width:0\"/><text x=\"",10+withsbp*4,"\" y=\"", currenty+15,"\" style=\"fill:green;font-weight:bold\">", sprintf("%.1f",withsbp),"%</text>\n", sep = "")
	}
#Statins
	currenty = currenty + 35
	svgtext = paste(svgtext,"<text x=\"0\" y=\"",currenty, "\" fill=\"black\" style=\"\">With statins for 10 years</text>\n", sep = "")
	currenty = currenty + 5
	svgtext = paste(svgtext,"<polygon points=\"0,",currenty,",", withstatins*4,",", currenty, ",", withstatins*4,",", currenty+20, ",0,", currenty+20, "\"  style=\"fill:green;fill-opacity:0.5;stroke-width:0\"/><text x=\"",10+withstatins*4,"\" y=\"", currenty+15,"\" style=\"fill:green;font-weight:bold\">", sprintf("%.1f",withstatins),"%</text>\n", sep = "")
if (smoke0 > 0)
	{
	currenty = currenty + 35
	svgtext = paste(svgtext,"<text x=\"0\" y=\"",currenty, "\" fill=\"black\" style=\"\">With smoking cessation (after three years)</text>\n", sep = "")
	currenty = currenty + 5
	svgtext = paste(svgtext,"<polygon points=\"0,",currenty,",", withsmokecess*4,",", currenty, ",", withsmokecess*4,",", currenty+20, ",0,", currenty+20, "\"  style=\"fill:green;fill-opacity:0.5;stroke-width:0\"/><text x=\"",10+withsmokecess*4,"\" y=\"", currenty+15,"\" style=\"fill:green;font-weight:bold\">", sprintf("%.1f",withsmokecess),"%</text>\n", sep = "")
	}
#Make optimal bar
currenty = currenty + 35
if (smoke0 > 0)
	{
	svgtext = paste(svgtext,"<text x=\"0\" y=\"",currenty, "\" fill=\"black\" style=\"\">Without smoking and with optimal cholesterol and blood pressure (after three years)</text>\n", sep = "")
	}
else
	{
	svgtext = paste(svgtext,"<text x=\"0\" y=\"",currenty, "\" fill=\"black\" style=\"\">With optimal cholesterol and blood pressure (after three years)</text>\n", sep = "")
	}
currenty = currenty + 5
svgtext = paste(svgtext,"<polygon points=\"0,",currenty,",", optimal*4,",", currenty, ",", optimal*4,",", currenty+20, ",0,", currenty+20, "\"  style=\"fill:green;fill-opacity:0.5;stroke-width:0\"/><text x=\"",10+optimal*4,"\" y=\"", currenty+15,"\" style=\"fill:green;font-weight:bold\">", sprintf("%.1f",optimal),"%</text>", sep = "")
#In case old browser
svgtext = paste(svgtext,"Sorry, your browser does not support inline SVG for dynamic graphics.</svg>")
#End of SVG
msg = paste(msg, svgtext)	
#Details
msg = paste(msg, "<h4>Details:</h4><ul>")
if (smoke0 > 0)
	{
msg = paste(msg, "<li>Smoking cessation:")
msg = paste(msg, "  <ul>")
msg = paste(msg, "  <li>You have a one in ", format(round(100/arr_smoke,digits = 0), nsmall = 0), " chance of avoiding cardiovascular disease over 10 years.</li>")
msg = paste(msg, "  <li>The <a href=\"http://www.cebm.net/number-needed-to-treat-nnt/\">number needed to treat</a> (NNT) is ", format(round(100/arr_smoke,digits = 0), nsmall = 0),".</li>")
msg = paste(msg, "  <li><a href=\"https://en.wikipedia.org/wiki/Absolute_risk_reduction\">Absolute risk reduction</a> (ARR) is ", sprintf("%.1f",arr_smoke), "%.</li>")
msg = paste(msg, "  </ul>")
msg = paste(msg, "</li>")
	}
msg = paste(msg, "<li>Medication ('statins') for cholesterol (3):")
msg = paste(msg, "  <ul>")
msg = paste(msg, "  <li>You have a one in ", format(round(100/arr,digits = 0), nsmall = 0), " chance of avoiding cardiovascular disease over 10 years.</li>")
msg = paste(msg, "  <li>The <a href=\"http://www.cebm.net/number-needed-to-treat-nnt/\">number needed to treat</a> (NNT) is ", format(round(100/arr,digits = 0), nsmall = 0),".</li>")
msg = paste(msg, "  <li><a href=\"https://en.wikipedia.org/wiki/Absolute_risk_reduction\">Absolute risk reduction</a> (ARR) is ", format(round(arr,digits = 1), nsmall = 1), "%.</li>")
msg = paste(msg, "  </ul>")
msg = paste(msg, "</li>")
if (smoke0 > 0)
	{
msg = paste(msg, "<li>Both:")
msg = paste(msg, "  <ul>")
msg = paste(msg, "  <li>You have a one in ", format(round(100/arr_both,digits = 0), nsmall = 0), " chance of avoiding cardiovascular disease over 10 years if you stop smoking and take a statin.</li>")
msg = paste(msg, "  <li>The <a href=\"http://www.cebm.net/number-needed-to-treat-nnt/\">number needed to treat</a> [NNT] is ", format(round(100/arr_both,digits = 0), nsmall = 0),".</li>")
msg = paste(msg, "  <li><a href=\"https://en.wikipedia.org/wiki/Absolute_risk_reduction\">Absolute risk reduction</a> (ARR) is ", sprintf("%.1f",arr_both), "%.</li>")
msg = paste(msg, "  </ul>")
msg = paste(msg, "</li>")
	}
msg = paste(msg, "</ul>")
msg = paste(msg, "<h3>Recommendations:</h3><ul>")
if (smoke0 > 0)
	{
	msg = paste(msg, "<li><a href=\"http://www.cdc.gov/tobacco/campaign/tips/quit-smoking/guide/steps-to-prepare.html\">Smoking - make a plan to quit</a></li>")
	}
msg = paste(msg, "<li><a href=\"http://pubmed.gov/24222015\">Healthy lifestyle</a> such as the <a href=\"http://dietamediterranea.com/en/piramide/\">Mediterranean Diet</a> (5)
 or <a href=\"http://www.nhlbi.nih.gov/health/public/heart/hbp/dash/\">Dash Diet</a> (lowers blood pressure) or <a href=\"http://www.heart.org/HEARTORG/GettingHealthy/Diet-and-Lifestyle-Recommendations_UCM_305855_Article.jsp\">AHA Diet</a>.</li>")
#Start of AHA/ACC recommendations
msg = paste(msg, "<li>Statins per AHA/ACC 2013 Recommendations (PMID: <a href=\"http://pubmed.gov/24222016\">24222016</a>):<ul>")
 if (prob >= 7.5)
	{
	if (diabetes0 == 1){msg = paste(msg, "<li>Since diabetic: use <a href=\"javascript:alert('Atorvastatin 40 - 80\\nRosuvastatin 20 - 40')\">high</a> intensity statin</li>")}
	if (diabetes0 == 0){msg = paste(msg, "<li>Since not diabetic: use <a href=\"javascript:alert('Atorvastatin 10 - 20\\nPravastain 40 - 80\\nRosuvastatin 5 - 10\\nSimvastatin 20 - 40')\">moderate</a> to <a href=\"javascript:alert('Atorvatstin 40 - 80\\nRosuvasatin 20 - 40')\">high</a> intensity statin</li>")}
	if (age0 < 40 || age0 > 75){msg = paste(msg, "<li>Statins: since age not 40 - 75, benefit is less clear</li>")}
	}
else
	{
		if (diabetes0 == 1)
		{
		msg = paste(msg, "<li>Since diabetic: use <a href=\"javascript:alert('Atorvastatin 10 - 20\\nPravastain 40 - 80\\nRosuvastatin 5 - 10')\">moderate</a> intensity statin</li>")
		}
	else
		{
		if (estLDL >= 190){msg = paste(msg, "<li>Statins may be needed. Non-HDL cholesterol is high at ", tchol0 - hdl0, " mg/dl. Consider measuring LDL as may be <u>></u> 190 mg/dl per Friedewald equation(2). If so, use <a href=\"javascript:alert('Atorvastatin 40 - 80\\nRosuvastatin 20 - 40')\">high</a> intensity statin if a candidate, else <a href=\"javascript:alert('Atorvastatin 10 - 20\\nPravastain 40 - 80\\nRosuvastatin 5 - 10')\">moderate</a> intensity statin.</li>")}
		}
	}
msg = paste(msg,"</ul></li></ul>")
list(message = msg)
#Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;
# Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text 
}