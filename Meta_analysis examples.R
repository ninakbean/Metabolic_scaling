############ 1 ##################
#Example
### copy data into 'dat' and examine data 
dat <- dat.konstantopoulos2011 
dat

### regular random-effects model 
res <- rma(yi, vi, data=dat) 
print(res, digits=3)

### regular random-effects model using rma.mv() 
res <- rma.mv(yi, vi, random = ~ 1 | study, data=dat) #Saying study is a random factor
print(res, digits=3)

### multilevel random-effects model 
res.ml <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)  #District in school is random
print(res.ml, digits=3)

### profile variance components 
profile(res.ml, progbar=FALSE)

### multivariate parameterization of the model 
res.mv <- rma.mv(yi, vi, random = ~ factor(school) | district, data=dat) 
print(res.mv, digits=3) 

### tau^2 from multivariate model = sum of the two variance components from the multilevel model 
round(sum(res.ml$sigma2), 3)

### rho from multivariate model = intraclass correlation coefficient based on the multilevel model 
round(res.ml$sigma2[1] / sum(res.ml$sigma2), 3)

############ 2 ##################

### copy data into 'dat' 
dat <- dat.hasselblad1998
### calculate log odds for each study arm 
dat <- escalc(measure="PLO", xi=xi, ni=ni, add=1/2, to="all", data=dat) 
dat

### create network graph ('plyr' and 'igraph' packages must be installed) 
## Not run: 
require(plyr) 
require(igraph) 
pairs <- do.call(rbind, sapply(split(dat$trt, dat$study), function(x) t(combn(x,2)))) 
pairs <- ddply(data.frame(pairs), .(X1, X2), count) 
g <- graph.edgelist(as.matrix(pairs[,1:2]), directed=FALSE) 
plot(g, edge.curved=FALSE, edge.width=pairs$freq, vertex.label.dist=.7, 
     vertex.label=c("Individual\nCounseling", "Group\nCounseling", "No Contact", "Self-Help")) 
## End(Not run)


### convert trt variable to factor with desired ordering of levels 
dat$trt <- factor(dat$trt, levels=c("no_contact", "self_help", "ind_counseling", "grp_counseling"))
### add a space before each level (this makes the output a bit more legible) 
levels(dat$trt) <- paste0(" ", levels(dat$trt))

### network meta-analysis using an arm-based model with fixed study effects 
### by setting rho=1/2, tau^2 reflects the amount of heterogeneity for all treatment comparisons 
res <- rma.mv(yi, vi, mods = ~ factor(study) + trt - 1, random = ~ trt | study, 
              rho=1/2, data=dat, btt="trt")
res
### all pairwise odds ratios of interventions versus no contact 
predict(res, newmods=cbind(matrix(0, nrow=3, ncol=24), diag(3)), 
        intercept=FALSE, transf=exp, digits=2)

### all pairwise odds ratios comparing interventions (ic vs sh, gc vs sh, and gc vs ic) 
predict(res, newmods=cbind(matrix(0, nrow=3, ncol=24), 
        rbind(c(-1,1,0), c(-1,0,1), c(0,-1,1))), intercept=FALSE, transf=exp, digits=2)






