# For downloading Google trends results
# requires a google account
# Results saved into spreadsheet March 27, 2016



#install.packages("gtrendsR")
library("gtrendsR") 
#options("google.user" = "username")
#options("google.password" = "password")
#setwd("where the data at?")
data = read.csv("DemPrimaryData.csv", stringsAsFactors = T)
dates = as.Date(data$Date, format = "%m/%d/%Y")
today = as.Date(Sys.time())
already = unique(dates[dates < today])
gdata = data.frame(states = data$Geography, Google = numeric(nrow(data)))
conn = gconnect()
for (i in seq_along(already)) {
  d = already[i]
  states = data$Geography[which(dates == d)]
  google = gtrends(c("Bernie Sanders", "Hillary Clinton"), geo = "US",
                   start_date = d - 31, end_date = d - 1)  
  gs = google$Top.subregions.for.bernie.sanders
  grows = which(gs$Subregion %in% states)
  gs = gs[grows, ]
  gs = gs[order(gs$Subregion), ]
  gdata[gdata$states %in% states, "Google"] = gs$bernie.sanders / (gs$bernie.sanders + gs$hillary.clinton)
}
d = today
states = data$Geography[which(!dates %in% already)]
states = states[1:(length(states)-1)] # Remove Puerto Rico
google = gtrends(c("Bernie Sanders", "Hillary Clinton"), geo = "US",
                 start_date = d - 60, end_date = d)  
gs = google$Top.subregions.for.bernie.sanders
grows = which(gs$Subregion %in% states)
gs = gs[grows, ]
gs = gs[order(gs$Subregion), ]
gdata[gdata$states %in% states, "Google"] = gs$bernie.sanders / (gs$bernie.sanders + gs$hillary.clinton)
gdata # Copy/pasted into spreadsheet
# Puerto Rico result entered by hand
