covid19positivedeviance <- function(fips, sizetolerance, densitytolerance, Growth.rate.14){

x <- getURL("https://raw.githubusercontent.com/qitools/COVID-19-mitigation-with-positive-deviance/master/data/data.counties.final.csv")
data.counties.final <- read.csv(text = x)

data.counties.final$density <- as.numeric(data.counties.final$density)
data.counties.final$population <- as.numeric(data.counties.final$population)

message = '';

(database.date <- as.Date(data.counties.final$CountyStartDate[1]) + data.counties.final$days[1])
message = paste(message,'<div>Database (<a href=\"https://github.com/nytimes/covid-19-data\">source at GitHub</a> current through: ', as.character(database.date),')</div>');

county.index <- data.counties.final[data.counties.final$fips == fips,]

if (nrow(county.index) == 0){
stop(paste("Error. County not present in the database.", sep=""))
}

# Filter by density
data.deviants <- data.counties <- data.counties.final[abs(data.counties.final$density/county.index$density -1) < densitytolerance/100,]

# Filter by absolute population (this is V2)
data.deviants.eligible <- data.deviants[abs(data.deviants$population/county.index$population -1) < sizetolerance/100,]

if (nrow(data.deviants.eligible) < 10){
stop(paste("Error. Insufficient counties identified: ",nrow(data.deviants.eligible), sep=""))
}

# Sort
if (Growth.rate.14){
#data.deviants.eligible <- data.deviants.eligible[with(data.deviants.eligible, order(data.deviants.eligible$Growth.rate.reg)), ]
data.deviants.eligible <- data.deviants.eligible[with(data.deviants.eligible, order(data.deviants.eligible$Growth.rate.14)), ]
}else{
data.deviants.eligible <- data.deviants.eligible[with(data.deviants.eligible, order(data.deviants.eligible$Growth.rate)), ]
}

# Keep deviants and index
data.deviants <- data.deviants.eligible[c(1:5,which(data.deviants.eligible$fips ==fips),(nrow(data.deviants.eligible)-4):nrow(data.deviants.eligible)),]

	message = paste(message,'<div>Total counties in database, data.counties.final: ',nrow(data.counties.final),' (excluded counties with < 2 cases or < 14 days since the first case.)</div>');
	message = paste(message,'<div>Eligible deviant counties: ',nrow(data.deviants.eligible),'</div>');
	if (nrow(data.deviants.eligible) < 20){
	  message = paste(message,'<div><span style=\"font-weight:bold;color:red\">Alert</span>: Less than 20 elibible counties for comparison. Consider expanding your population and density criteria.</div>');
	}
	if (length(unique(data.deviants$fips)) < 11){
		message = paste(message,'<div><span style=\"font-weight:bold;color:red\">Alert</span>: There are two green rows in your table because your county was a deviant. Perhaps broadern your criteria.</div>');
	}
	message = paste(message,'<table><tr><th>County</th><th>State</th><th>Population<br/>(2018 est)</th><th>Pop density<br/>(2018 est)</th><th>cases</th><th>Start date</th><th>days</th><th>Growth rate (%)<br/>(since first case)</th><th>Growth rate (%)<br/>(last 14 days)</th><th>Doubling.time</th><th>tests<br/>(Not available)</th><th>testing.rate<br/>(Not available)</th><th>Unacast<br/>(mobility data)</th><th>Stay at home<br/>(<a href=\"https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html\" target=_blank>links</a>)</th></tr>');

i <- 1
style <- 'white'
for (i in 1:nrow(data.deviants)){
	if (data.deviants$fips[i] == fips){
		backgroundcolor <- 'lightgreen'
		}else{
		backgroundcolor <- 'white'
		}
	message = paste(message,'<tr style=\"background-color:',backgroundcolor,'\"><td><a target=_blank href=\"https://en.wikipedia.org/wiki/',data.deviants$county[i],'_County,_',data.deviants$state[i],'\">',data.deviants$county[i],'</a><br/>FIPS: ',data.deviants$fips[i],'</td><td>',data.deviants$state[i],'</td><td>',data.deviants$population[i],'</td><td>',data.deviants$density[i],'</td><td>',data.deviants$cases[i],'</td><td>',as.character(data.deviants$CountyStartDate[i]),'</td><td>',data.deviants$days[i],'</td><td>',round(100*(data.deviants$Growth.rate[i]-1),1),'</td><td>',round(100*(data.deviants$Growth.rate.reg[i]-1),1),'</td><td>',round(data.deviants$Doubling.time[i],1),'</td><td>',data.deviants$tests[i],'</td><td>',data.deviants$testing.rate[i],'</td><td><a href=\"https://en.wikipedia.org/wiki/U.S._state_and_local_government_response_to_the_2020_coronavirus_pandemic#',data.deviants$state[i],\"'>State response at WikiPedia</a><br/><a href=\"https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html#',data.deviants$state[i],\"'>State response at NYTimes</a></td><td><a href=\"https://www.unacast.com/covid19/social-distancing-scoreboard?view=county&fips=',data.deviants$fips[i],'\">Unacast data</a></td><td>Comments: imposed by the state<br/>Date: 03/30/2020 Day no: 23<br/><a href=\"https://governor.kansas.gov/governor-kelly-issues-temporary-statewide-stay-home-order-in-ongoing-effort-to-combat-covid-19/\" target=_blank>Link</a> Contributed by jdoe@kumc.edu</td></tr>');
	}
	message = paste(message,'</table>');
	list(message = message)
}
