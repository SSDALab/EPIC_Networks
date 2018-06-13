library("statnet")  # version 2016.9 with ergm 3.7.1
library("texreg")   # version 1.36.23
library("xergm")    # version 1.8.2 with btergm 1.9.0 and xergm.common 1.7.7
set.seed(10)

# sessionInfo() for replication purposes:

# R version 3.3.0 (2016-05-03)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 16.04.2 LTS
# 
# locale:
#  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
#  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
#  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
#  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
#  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#  [1] xergm_1.8.2          GERGM_0.11.2         rem_1.1.2           
#  [4] tnam_1.6.5           btergm_1.9.0         ggplot2_2.1.0       
#  [7] xergm.common_1.7.7   texreg_1.36.23       statnet_2016.9      
# [10] sna_2.4              ergm.count_3.2.2     tergm_3.4.0         
# [13] networkDynamic_0.9.0 ergm_3.7.1           network_1.13.0      
# [16] statnet.common_3.3.0
# 
# loaded via a namespace (and not attached):
#  [1] deSolve_1.12        gtools_3.5.0        lpSolve_5.6.13     
#  [4] splines_3.3.0       lattice_0.20-33     mstate_0.2.8       
#  [7] colorspace_1.3-0    flexsurv_0.7        stats4_3.3.0       
# [10] mgcv_1.8-12         survival_2.39-4     nloptr_1.0.4       
# [13] RColorBrewer_1.1-2  muhaz_1.2.6         speedglm_0.3-2     
# [16] trust_0.1-7         plyr_1.8.4          robustbase_0.92-7  
# [19] munsell_0.4.3       gtable_0.1.2        caTools_1.17.1     
# [22] mvtnorm_1.0-5       coda_0.19-1         permute_0.8-4      
# [25] parallel_3.3.0      DEoptimR_1.0-8      Rcpp_0.12.10       
# [28] KernSmooth_2.23-15  ROCR_1.0-7          scales_0.4.1       
# [31] gdata_2.17.0        vegan_2.3-1         RcppParallel_4.3.20
# [34] lme4_1.1-10         gplots_2.17.0       grid_3.3.0         
# [37] quadprog_1.5-5      tools_3.3.0         bitops_1.0-6       
# [40] magrittr_1.5        RSiena_1.1-232      cluster_2.0.4      
# [43] MASS_7.3-45         Matrix_1.2-6        minqa_1.2.4        
# [46] boot_1.3-17         igraph_1.0.1        nlme_3.1-128


# EXAMPLE 1: ALLIANCES

data("alliances", package = "xergm.common")

pdf("alliances.pdf", width = 9, height = 3)
par(mfrow = c(1, 3), mar = c(0, 0, 1, 0))
for (i in (length(allyNet) - 2):length(allyNet)) {
  plot(allyNet[[i]], main = paste("t =", i))
}
dev.off()

model.1a <- btergm(allyNet ~ edges + gwesp(0, fixed = TRUE) +
    edgecov(LSP) + edgecov(warNet) + nodecov("polity") +
    nodecov("cinc") + absdiff("polity") + absdiff("cinc") +
    edgecov(contigMat), R = 50, parallel = "snow", ncpus = 2)
summary(model.1a, level = 0.95)

gof.1a <- gof(model.1a, nsim = 50, statistics = c(esp, geodesic, deg))
pdf("gof-1a.pdf", width = 8, height = 2.5)
plot(gof.1a)
dev.off()

model.1b <- btergm(allyNet ~ edges + gwesp(0, fixed = TRUE) +
    edgecov(LSP) + edgecov(warNet) + nodecov("polity") +
    nodecov("cinc") + absdiff("polity") + absdiff("cinc") +
    edgecov(contigMat) + memory(type = "stability") +
    timecov(transform = function(t) t) + timecov(warNet,
    transform = function(t) t), R = 50)

texreg(list(model.1a, model.1b), single.row = TRUE,
    include.nobs = FALSE, file = "alliance-table.tex",
    caption = "International alliance TERGM examples.",
    label = "alliance-table", custom.model.names =
    c("Model~1a", "Model~1b"), use.packages = FALSE,
    booktabs = TRUE, dcolumn = TRUE)

gof.1b <- gof(model.1b, nsim = 50, statistics = c(esp, geodesic, deg))
pdf("gof-1b.pdf", width = 8, height = 2.5)
plot(gof.1b)
dev.off()


# EXAMPLE 2: SCHOOL FRIENDSHIP NETWORK

data("knecht", package = "xergm.common")

for (i in 1:length(friendship)) {
  rownames(friendship[[i]]) <- 1:nrow(friendship[[i]])
  colnames(friendship[[i]]) <- 1:ncol(friendship[[i]])
}
rownames(primary) <- rownames(friendship[[1]])
colnames(primary) <- colnames(friendship[[1]])
sex <- demographics$sex
names(sex) <- 1:length(sex)

friendship <- handleMissings(friendship, na = 10, method = "remove")
friendship <- handleMissings(friendship, na = NA, method = "fillmode")

for (i in 1:length(friendship)) {
  s <- adjust(sex, friendship[[i]])
  friendship[[i]] <- network(friendship[[i]])
  friendship[[i]] <- set.vertex.attribute(friendship[[i]], "sex", s)
  idegsqrt <- sqrt(degree(friendship[[i]], cmode = "indegree"))
  friendship[[i]] <- set.vertex.attribute(friendship[[i]],
      "idegsqrt", idegsqrt)
  odegsqrt <- sqrt(degree(friendship[[i]], cmode = "outdegree"))
  friendship[[i]] <- set.vertex.attribute(friendship[[i]],
      "odegsqrt", odegsqrt)
}
sapply(friendship, network.size)

pdf("knecht.pdf")
par(mfrow = c(2, 2), mar = c(0, 0, 1, 0))
for (i in 1:length(friendship)) {
  plot(network(friendship[[i]]), main = paste("t =", i),
  usearrows = TRUE, edge.col = "grey50")
}
dev.off()

model.2a <- btergm(friendship ~ edges + mutual + ttriple +
    transitiveties + ctriple + nodeicov("idegsqrt") +
    nodeicov("odegsqrt") + nodeocov("odegsqrt") +
    nodeofactor("sex") + nodeifactor("sex") + nodematch("sex") +
    edgecov(primary), R = 100)

model.2b <- btergm(friendship ~ edges + mutual + ttriple +
    transitiveties + ctriple + nodeicov("idegsqrt") +
    nodeicov("odegsqrt") + nodeocov("odegsqrt") +
    nodeofactor("sex") + nodeifactor("sex") + nodematch("sex") +
    edgecov(primary) + delrecip + memory(type = "stability"),
    R = 100)

texreg(list(model.2a, model.2b), single.row = TRUE, 
    include.nobs = FALSE, file = "knecht-table.tex", 
    caption = "TERGM examples on friendship networks in a school class.", 
    label = "knecht-table", custom.model.names = 
    c("Model~2a", "Model~2b"), use.packages = FALSE, 
    booktabs = TRUE, dcolumn = TRUE)

delrecip <- lapply(friendship, function(x) t(as.matrix(x)))[1:3]
stability <- lapply(friendship, function(x) {
  mat <- as.matrix(x)
  mat[mat == 0] <- -1
  return(mat)
})[1:3]

model.2c <- btergm(friendship[2:4] ~ edges + mutual + ttriple +
    transitiveties + ctriple + nodeicov("idegsqrt") +
    nodeicov("odegsqrt") + nodeocov("odegsqrt") +
    nodeofactor("sex") + nodeifactor("sex") + nodematch("sex") +
    edgecov(primary) + edgecov(delrecip) + edgecov(stability),
    R = 100)

model.2d <- mtergm(friendship ~ edges + mutual + ttriple +
    transitiveties + ctriple + nodeicov("idegsqrt") +
    nodeicov("odegsqrt") + nodeocov("odegsqrt") +
    nodeofactor("sex") + nodeifactor("sex") + nodematch("sex") +
    edgecov(primary) + delrecip + memory(type = "stability"),
    control = control.ergm(MCMC.samplesize = 5000, MCMC.interval = 2000))

model.2e <- btergm(friendship[1:3] ~ edges + mutual + ttriple +
    transitiveties + ctriple + nodeicov("idegsqrt") +
    nodeicov("odegsqrt") + nodeocov("odegsqrt") +
    nodeofactor("sex") + nodeifactor("sex") + nodematch("sex") +
    edgecov(primary) + delrecip + memory(type = "stability"),
    R = 100)

gof.2e <- gof(model.2e, nsim = 100, target = friendship[[4]],
    formula = friendship[3:4] ~ edges + mutual + ttriple +
    transitiveties + ctriple + nodeicov("idegsqrt") +
    nodeicov("odegsqrt") + nodeocov("odegsqrt") +
    nodeofactor("sex") + nodeifactor("sex") + nodematch("sex") +
    edgecov(primary) + delrecip + memory(type = "stability"),
    coef = coef(model.2b), statistics = c(esp, dsp, geodesic,
    deg, triad.undirected, rocpr))
pdf("gof-2e.pdf", width = 8, height = 6)
plot(gof.2e, roc.rgraph = TRUE, pr.rgraph = TRUE)
dev.off()

gof.2e
plot(gof.2e[[6]])
gof.2e[[6]]$auc.pr

nw <- simulate(model.2e, nsim = 10, index = 3)

interpret(model.2b, type = "dyad", i = 12, j = 15, t = 3)

dyads <- list()
for (t in 2:length(friendship)) {
  sex <- get.vertex.attribute(friendship[[t]], "sex")
  mat <- as.matrix(friendship[[t]])
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (i != j && sex[i] == sex[j]) {
        dyads[[length(dyads) + 1]] <- c(i, j, t, interpret(model.2b,
            type = "tie", i = i, j = j, t = t - 1), sex[i])
      }
    }
  }
}
dyads <- do.call("rbind", dyads)
dyads <- as.data.frame(dyads)
colnames(dyads) <- c("i", "j", "t", "prob", "sex")

samplesize <- 10000
results <- list()
for (t in 2:length(friendship)) {
  for (s in 1:2) {
    label <- ifelse(s == 1, paste0("f", t), paste0("m", t))
    d <- dyads[dyads$sex == s & dyads$t == t, ]
    n <- nrow(d)
    means <- sapply(1:samplesize, function(x) {
      samp <- sample(1:n, n, replace = TRUE)
      mean(d[samp, ]$prob)
    })
    results[[label]] <- means
  }
}

quantiles <- sapply(results, function(x) {
  return(c(quantile(x, 0.025), quantile(x, 0.5), quantile(x, 0.975)))
})

library("gplots")
pdf("interpret.pdf")
barplot2(quantiles[2, ], col = c("lightpink", "cornflowerblue"),
    plot.ci = TRUE, ci.l = quantiles[1, ], ci.u = quantiles[3, ],
    names = rep(c("F", "M"), 3), space = c(0.2, 0.2, 0.6, 0.2, 0.6, 0.2),
    xlab = "Time from t = 2 to t = 4",
    ylab = "Median edge probability (with 95 percent CI)", ci.lwd = 2,
    main = "Same-sex friendship probabilities over time")
dev.off()

ep <- edgeprob(model.2b)
head(ep)

degen <- checkdegeneracy(model.2b, nsim = 1000)
degen
