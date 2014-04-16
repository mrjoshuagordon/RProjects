
# Load Libraries 
library(RCurl)
library(XML)

######### GET DATA FOR 2014 #############

d1 = data.frame()

for(i in 1:3){
  theurl <- paste("http://thegradcafe.com/survey/index.php?q=statistics&t=a&pp=250&o=&p=",i, sep="")
  tables <- readHTMLTable(theurl)
  tables = tables[[1]]
  tables = data.frame(tables$Institution, tables$"Date Added", tables$"Decision & Date", tables$"Program (Season)")
  tables = tables[-1,]
  d1 = rbind(d1, tables )
  
  print(i)
  if(length( grep("2013",d1[,3])) > 0) {
    d1 = d1[-grep("2013",d1[,3]),]
      break                 
  }
  
}

d.out = d1


############# Split into Masters/PhD ######################################


program = "phd"

program = "masters"
if(program == "phd"){
  d1 = d.out[grep("phd", tolower(d.out[,4])),]
} else{
  d1 = d.out[grep("masters", tolower(d.out[,4])),]
  
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
        cex.names=.7, main=paste(toupper(program), "Results as of", 
        Sys.Date(), sep= " "), ylim=c(0, max(c(rice, unc, ucla, usc))+10)
)
text(x=bplt , y=schools ,labels=as.character( schools ), pos=3  )



######## TODO #################

# Split the Response Rate by Masters and PhD

d2 = d1[grep("rice|angeles|north carolina|southern california|usc|ucla|", tolower(d1[,1])),  ]

bp2 = d2[-grep("state", tolower(d2[,1]) ),] 

school = rep(0, nrow(bp2) )
bp3 = data.frame(bp2, school)


bp3$school[grep("rice", tolower(bp3[,1]))] = "rice"
bp3$school[grep("north carolina|unc", tolower(bp3[,1]))] = "unc"
bp3$school[grep("ucla|angeles", tolower(bp3[,1]))] = "ucla"
bp3$school[grep("usc|southern california", tolower(bp3[,1]))] = "usc"







