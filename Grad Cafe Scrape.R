######### Libraries

library(xts)
library(RCurl)
library(XML)

######### GET DATA #############

d1 = data.frame()

for(i in 1:30){
    theurl <- paste("http://thegradcafe.com/survey/index.php?q=statistics&t=a&pp=250&o=&p=",i, sep="")
      tables <- readHTMLTable(theurl)
      tables = tables[[1]]
      tables = data.frame(tables$Institution, tables$"Date Added", tables$"Decision & Date")
      tables = tables[-1,]
      d1 = rbind(d1, tables )
      print(i)
      if(nrow(tables)<250) break
}


####### STORE DATA FRAME AND INITIALIZE DATE RANGE FOR DATA SET #################################
d = d1
names(d) = c("Institution","Date Added" ,"Decision & Date")
d2 = as.Date(d[,2],format='%d %b %Y')

data.range = seq(min(d2), max(d2), by="days")

#### COUNT NUMBER OF ACCEPT/REJECT/WAIT LISTED ##################################################
accept = d[grep("accepted", tolower(d$"Decision & Date")),]
reject = d[grep("rejected", tolower(d$"Decision & Date")),]
wait = d[grep("wait listed", tolower(d$"Decision & Date")),]

accept.totals = table(as.Date(accept$"Date Added",format='%d %b %Y') )
reject.totals = table(as.Date(reject$"Date Added",format='%d %b %Y') )
wait.totals = table(as.Date(wait$"Date Added",format='%d %b %Y') )

acc.v = rep(0, length(as.character(data.range)))
rej.v = rep(0, length(as.character(data.range)))
wait.v = rep(0, length(as.character(data.range)))

acc.v[which(as.character(data.range) %in% names(accept.totals)  == T)] = accept.totals[which(names(accept.totals) %in% as.character(data.range) == T)]
rej.v[which(as.character(data.range) %in% names(reject.totals)  == T)] = reject.totals[which(names(reject.totals) %in% as.character(data.range) == T)]
wait.v[which(as.character(data.range) %in% names(wait.totals)  == T)] = wait.totals[which(names(wait.totals) %in% as.character(data.range) == T)]


response = data.frame(data.range, acc.v, rej.v, wait.v)
names(response) = c("date", "accept", "reject", "wait")

### MONTHLY SUMS ###################################################################################
acc.m = apply.monthly(as.xts(response$accept,order.by=response$date), FUN= sum )
rej.m = apply.monthly(as.xts(response$reject,order.by=response$date), FUN= sum )
wait.m = apply.monthly(as.xts(response$wait,order.by=response$date), FUN= sum )

### PLOT #################################################################################
dat = data.frame(acc.m, rej.m, wait.m)

dev.off()

barplot(t(as.matrix(dat)), col=c("green","red", "yellow"), cex.axis=.7, cex.names=.7)
legend("topleft", legend=c("Accepted", "Rejected", "Wait Listed"), fill=c("green","red", "yellow"))
title(main = paste("Grad Cafe Results for Statistics Programs as of", format(max(data.range),"%A %B %d") , sep=" " ))



