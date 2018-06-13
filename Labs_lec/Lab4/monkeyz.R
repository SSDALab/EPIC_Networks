###############################
### ERGM
### Ryan Larson
##############################

library(statnet)
library(ergm)
library(sna)
library(coda)
library(networkdata)

help(package="networkdata")

data(gandg) 
monkey <- gandg$grooming

summary(monkey~edges) # Look at the $g(y)$ statistic for this model
monkey.edges <- ergm(monkey~edges) # Estimate the model 
summary(monkey.edges) # The fitted model object

summary(monkey~edges+triangle) # Look at the g(y) stats for this model
monkey.edgetri <- ergm(monkey~edges+triangle) 
summary(monkey.edgetri)

gender <- monkey %v% 'sex' # %v% references vertex attributes
age <- monkey %v% 'age_grade' # %v% references vertex attributes


plot(monkey, vertex.col=gender, vertex.cex=age, main="Monkeys", cex.main=0.8)
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model
monkey.gender <- ergm(monkey~edges+triangle+nodecov('sex'))
summary(monkey.gender)

monkey.age <- ergm(monkey~edges+triangle+nodecov('sex')+nodecov('age_grade'))
summary(monkey.age)

monkey.match <- ergm(monkey~edges+triangle+nodecov('age_grade')+nodematch('sex',diff=T))
summary(monkey.match)

mixingmatrix(monkey,'sex')

monkey.mutual <- ergm(monkey~edges+triangle+nodecov('age_grade')+nodematch('sex',diff=T)+mutual)
summary(monkey.mutual)

mcmc.diagnostics(monkey.mutual)
gof(monkey.mutual)

m <- ergm(monkey~edges+nodecov('age_grade'))
summary(m)

gof <- gof(m)
plot(gof)

m <- ergm(monkey~edges+nodeicov('age_grade'))
summary(m)

m <- ergm(monkey~edges+nodeocov('age_grade'))
summary(m)

m <- ergm(monkey~edges+sender)
summary(m)

