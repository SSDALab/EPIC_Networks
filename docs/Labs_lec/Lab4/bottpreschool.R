library(sna)
library(network)
library(networkdata)


help("ergm-terms")

data(bott)

# Edge is when one child irritated another child
gplot(bott[[4]])

bott[[4]]
par(mfrow=c(1,2)) # Setup a 2 panel plot (for later)
gplot(bott[[4]], main="Bott-Preschool", cex.main=0.8)
summary(bott[[4]]~edges) # Look at the $g(y)$ statistic for this model
bottmodel.01 <- ergm(bott[[4]]~edges) # Estimate the model 
summary(bottmodel.01) # The fitted model object

summary(bott[[4]]~edges+triangle)
bottmodel.02 <- ergm(bott[[4]]~edges + triangle)
summary(bottmodel.02)
class(bottmodel.02)

bott[[4]]
age <- bott[[4]] %v% "age.month"
summary(age)
gplot(bott[[4]], main="Bott-Preschool", cex.main=0.8, vertex.cex=age/24)
summary(bott[[4]]~edges+nodecov('age.month'))
bottmodel.03 <- ergm(bott[[4]]~edges+nodecov('age.month'))
summary(bottmodel.03)

bottmodel.04 <- ergm(bott[[4]]~edges+mutual)
summary(bottmodel.04)

adj.01<-as.sociomatrix(bott[[1]])
adj.01

# Test the irritation network also using edges from the talking network
bottmodel.05 <- ergm(bott[[4]]~edges+edgecov(bott[[1]]))

# The probability of an edge in bott[[4]] is 0.17, but if it also appears 
# in the talking network then the probability increases to 0.39

agediff <- abs(outer(bott[[4]]%v%"age.month",bott[[4]]%v%"age.month","-"))
bottmodel.06 <- ergm(bott[[4]]~edges+edgecov(bott[[1]])+edgecov(agediff))
summary(bottmodel.06)

# Adding the age difference between dyadic pairs changes significance level of
# edges but not the edges from the talking network (bott[[1]])

## Testing GOF ------------------------------------------------------------
bottmodel.06.gof <- gof(bottmodel.06~ esp + distance)
bottmodel.06.gof
plot(bottmodel.06.gof)
