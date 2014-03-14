
# Load Libraries 
library(RCurl)
library(XML)

######### GET DATA FOR 2014 #############

d1 = data.frame()

for(i in 1:3){
  theurl <- paste("http://thegradcafe.com/survey/index.php?q=statistics&t=a&pp=250&o=&p=",i, sep="")
  tables <- readHTMLTable(theurl)
  tables = tables[[1]]
  tables = data.frame(tables$Institution, tables$"Date Added", tables$"Decision & Date")
  tables = tables[-1,]
  d1 = rbind(d1, tables )
  
  print(i)
  if(length( grep("2013",d1[,3])) > 0) {
    d1 = d1[-grep("2013",d1[,3]),]
      break                 
  }
  
}

####### Find the Results for the schools I applied #########################

tables = as.vector(tolower(d1[,1])) 

ind =  grep("rice|angeles|north carolina|southern california|usc|ucla|unc", tables) 

bp = table(tables[ind])
bp = bp[-grep("state", names(bp) )] 

rice = sum(bp[grep("rice", names(bp))])
unc = sum(bp[grep("unc|north carolina|chapel hill", names(bp))])
ucla = sum(bp[grep("ucla|angeles", names(bp))])
usc = sum(bp[grep("usc|southern california", names(bp))])

########## Make the Bar Plot###############################################

schools = c(rice, unc, ucla, usc)
bplt = barplot(schools ,names.arg= 
          c("rice", "unc", "ucla", "usc")  ,  
        cex.names=.7, main=paste("Results as of", 
        Sys.Date(), sep= " "), ylim=c(0, max(c(rice, unc, ucla, usc))+10)
)
text(x=bplt , y=schools ,labels=as.character( schools ), pos=3  )



######## TODO #################

# Split the Response Rate by Masters and PhD



