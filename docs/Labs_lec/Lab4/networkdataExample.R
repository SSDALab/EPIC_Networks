###########################
## You can install a networkdata package I built from github
## Using devtools
###########################

## Mark Uses the Lazega Laywer data example in his class
## Here is a basic example of how to get the data from 
## my networkdata package
## There are a lot of other datasets here too

library(devtools)
install_githup("zalmquist/networkdata")

library(networkdata)
help(package=networkdata)

help(lazega)
data(lazega)


efit <- ergm(lazega[[1]]~edges
   + match("office")
   + nodecov("seniority"),
              MPLEonly=TRUE)
              
# Condition on the outdegrees
#
efit <- ergm(lazega[[1]]~edges
   + match("office")
   + match("practice") + mutual)
   
   summary(efit)