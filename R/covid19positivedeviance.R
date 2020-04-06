covid19positivedeviance <- function(fips, sizetolerance, densitytolerance, nlr){

x <- getURL("https://raw.githubusercontent.com/qitools/COVID-19-mitigation-with-positive-deviance/master/data/data.counties.final.csv")
data.counties.final <- read.csv(text = x)

data.counties.final$density <- as.numeric(data.counties.final$density)
data.counties.final$population <- as.numeric(data.counties.final$population)

county.index <- data.counties.final[data.counties.final$fips == fips,]

message = '';

# Filter by density
data.deviants <- data.counties <- data.counties.final[abs(data.counties.final$density/county.index$density -1) < densitytolerance/100,]

# Filter by absolute population (this is V2)
data.deviants.eligible <- data.deviants[abs(data.deviants$population/county.index$population -1) < sizetolerance/100,]

if (nrow(data.deviants.eligible) < 10){
stop(paste("Error. Insufficient counties identified: ",nrow(data.deviants.eligible), sep=""))
}

# Sort
if (nlr){
data.deviants.eligible <- data.deviants.eligible[with(data.deviants.eligible, order(data.deviants.eligible$Growth.rate.reg)), ]
}else{
data.deviants.eligible <- data.deviants.eligible[with(data.deviants.eligible, order(data.deviants.eligible$Growth.rate)), ]
}

# Keep deviants and index
data.deviants <- data.deviants.eligible[c(1:5,which(data.deviants.eligible$fips ==fips),(nrow(data.deviants.eligible)-4):nrow(data.deviants.eligible)),]

	message = paste(message,'<div>data.counties.final: ',nrow(data.counties.final),'</div>');
	message = paste(message,'<div>eligible deviants: ',nrow(data.deviants.eligible),'</div>');
	if (nrow(data.deviants.eligible) > 10){
		message = paste(message,'<div>There are two green rows in your table, because your county was a deviant. Perhaps broadern your criteria.</div>');
	}
	message = paste(message,'<table><tr><th>County</th><th>State</th><th>FIPS</th><th>Population<br/>(2018 est)</th><th>Pop density<br/>(2018 est)</th><th>cases</th><th>Start date</th><th>days</th><th>Growth rate (%)</th><th>Growth rate (%)<br/>(by nonlinear regression)</th><th>Doubling.time</th><th>tests<br/>(Not available)</th><th>testing.rate<br/>(Not available)</th><th>Stay at home<br/>(<a href=\"https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html\" target=_blank>links</a>)</th></tr>');

i <- 1
style <- 'white'
for (i in 1:nrow(data.deviants)){
	if (data.deviants$fips[i] == fips){
		backgroundcolor <- 'lightgreen'
		}else{
		backgroundcolor <- 'white'
		}
	message = paste(message,'<tr style=\"background-color:',backgroundcolor,'\"><td><a target=_blank href=\"https://en.wikipedia.org/wiki/',data.deviants$county[i],'_County,_',data.deviants$state[i],'\">',data.deviants$county[i],'</a></td><td>',data.deviants$state[i],'</td><td>',data.deviants$fips[i],'</td><td>',data.deviants$population[i],'</td><td>',data.deviants$density[i],'</td><td>',data.deviants$cases[i],'</td><td>',data.deviants$CountyStartDate[i],'</td><td>',data.deviants$days[i],'</td><td>',round(100*(data.deviants$Growth.rate[i]-1),1),'</td><td>',round(100*(data.deviants$Growth.rate.reg[i]-1),1),'</td><td>',round(data.deviants$Doubling.time[i],1),'</td><td>',data.deviants$tests[i],'</td><td>',data.deviants$testing.rate[i],'</td><td>Comments: imposed by the state<br/>Date: 03/30/2020 Day no: 23<br/><a href=\"https://governor.kansas.gov/governor-kelly-issues-temporary-statewide-stay-home-order-in-ongoing-effort-to-combat-covid-19/\" target=_blank>Link</a> Contributed by jdoe@kumc.edu</td></tr>');
	}
	message = paste(message,'</table>');
	list(message = message)
}
