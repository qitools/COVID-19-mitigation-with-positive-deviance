covid19positivedeviance <- function(fips = 0, sizetolerance = 0, densitytolerance = 0, growthratemethod = 'nlr'){

x <- getURL("https://raw.githubusercontent.com/qitools/COVID-19-mitigation-with-positive-deviance/master/data/data.counties.final.csv")
data.counties.final <- read.csv(text = x)
county.index <- data.counties.final[data.counties.final$fips == fips,]

message = '';

# Filter by density
data.deviants <- data.counties <- data.counties.final[abs(data.counties.final$density/county.index$density -1) < 0.25,]

# Filter by absolute population (this is V2)
data.deviants.eligible <- data.deviants[abs(data.deviants$population/county.index$population -1) < 0.25,]

# Sort
data.deviants.eligible <- data.deviants.eligible[with(data.deviants.eligible, order(Growth.rate.reg)), ]

# Keep deviants and index
data.deviants <- data.deviants.eligible[c(1:5,which(data.deviants.eligible$fips ==fips),(nrow(data.deviants.eligible)-4):nrow(data.deviants.eligible)),]

	message = paste(message,'<div>data.counties.final: ',nrow(data.counties.final),'</div>');
	message = paste(message,'<div>eligible deviants: ',nrow(data.deviants.eligible),'</div>');
	message = paste(message,'<table><tr><th>County</th><th>State</th><th>FIPS</th><th>cases</th><th>days</th><th>Growth rate (%)</th><th>Growth rate (%)<br/>(by nonlinear regression)</th><th>Doubling.time</th><th>tests<br/>(Not available)</th><th>testing.rate<br/>(Not available)</th></tr>');

i <- 1
style <- 'white'
for (i in 1:nrow(data.deviants)){
	if (data.deviants$fips[i] == fips){
		backgroundcolor <- 'lightgreen'
		}else{
		backgroundcolor <- 'white'
		}
	message = paste(message,'<tr style=\"background-color:',backgroundcolor,'\"><td><a target=_blank href=\"https://en.wikipedia.org/wiki/',data.deviants$county[i],'_County,_',data.deviants$state[i],'\">',data.deviants$county[i],'</a></td><td>',data.deviants$state[i],'</td><td>',data.deviants$fips[i],'</td><td>',data.deviants$cases[i],'</td><td>',data.deviants$days[i],'</td><td>',round(100*(data.deviants$Growth.rate[i]-1),1),'</td><td>',round(100*(data.deviants$Growth.rate.reg[i]-1),1),'</td><td>',round(data.deviants$Doubling.time[i],1),'</td><td>',data.deviants$tests[i],'</td><td>',data.deviants$testing.rate[i],'</td></tr>');
	}
	message = paste(message,'</table>');
	list(message = message)
}
