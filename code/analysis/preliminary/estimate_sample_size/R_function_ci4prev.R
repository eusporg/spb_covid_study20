ci4prev=function(poz,n,se=1,sp=1,level=.95,dec=3,method="bl"){

# calculates exact confidence limits for the prevalence of disease
# adjusted for sensitivity and specificity of the diagnostic test

# written by Jenő Reiczigel, 2009 (reiczigel.jeno@aotk.szie.hu)

# if using please cite 
#  Reiczigel, Földi and Ózsvári (2010) Exact confidence limits for 
#  prevalence of a disease with an imperfect diagnostic test, 
#  Epidemiology and infection, 138: 1674-1678.

# poz - number of test positives
# n - sample size
# se - test sensitivity
# sp - test specificity
# level - prescribed confidence level
# dec - required number of decimals for the result
# method - "bl" for Blaker, "st" for Sterne, 
#          "cp" for Clopper-Pearson, "wi" for Wilson (for n>500)

# ci4prev calls the following functions:
#     blakerci()  for Blaker's interval (see below), 
#     sterne.int()  for Sterne's interval (see below),
#     binconf()  from library(Hmisc) for Wilson & Clopper-Pearson

 if(method=="cp") cl=binconf(poz,n,method="e",alpha=1-level)[2:3]
  else if (method=="st") cl=sterne.int(poz,n,alpha=1-level)
   else if (method=="bl") cl=blakerci(poz,n,level=level)
    else if (method=="wi") cl=binconf(poz,n,method="w",alpha=1-level)[2:3]
     else stop('valid methods are "bl", "st", "cp", and "wi"')

 adj.cl=(cl+sp-1)/(se+sp-1) 
 adj.cl=pmax(adj.cl,c(0,0))
 adj.cl=pmin(adj.cl,c(1,1))

 names(adj.cl)=NULL
 return(round(adj.cl,digits=dec))
}



library(Hmisc)



# -----------------------------------
# Blaker's interval (by Helge Blaker)
# -----------------------------------

blakerci <- function(x,n,level=.95,tolerance=1e-04){
  lower = 0
  upper = 1
  if (x!=0){lower = qbeta((1-level)/2, x, n-x+1)
    while (acceptbin(x, n, lower + tolerance) < (1 - level))
      lower = lower+tolerance
   }
  if (x!=n){upper = qbeta(1 - (1-level)/2, x+1, n-x)
    while (acceptbin(x, n, upper - tolerance) < (1 - level))
      upper = upper-tolerance
   }
 c(lower,upper)
}
# Computes the Blaker exact ci (Canadian J. Stat 2000)
# for a binomial success probability
# for x successes out of n trials with
# confidence coefficient = level; uses acceptbin function

acceptbin = function(x, n, p){
 #computes the Blaker acceptability of p when x is observed
 # and X is bin(n, p)
   p1 = 1 - pbinom(x - 1, n, p)
   p2 = pbinom(x, n, p)
   a1 = p1 + pbinom(qbinom(p1, n, p) - 1, n, p)
   a2 = p2 + 1 - pbinom(qbinom(1 - p2, n, p), n, p)
   return(min(a1,a2))
}



######################################################################
#
# EXACT CONFIDENCE BOUNDS FOR A BINOMIAL PARAMETER p
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Calculate exact Sterne confidence bounds for a binomial parameter
# by the method described in Duembgen (2004): Exact confidence
# bounds in discrete models - algorithmic aspects of Sternes method.
# Preprint, available on www.stat.unibe.ch/~duembgen
#
# Kaspar Rufibach, August 2004
#
######################################################################

# define function sterne.int (find example at the bottom)
sterne.int <- function(x,n,alpha=0.05,del=10^-5){

logit <- function(p){log(p/(1-p))}
invlogit <- function(y){exp(y)/(1+exp(y))}
theta <- function(k,x,n){(lchoose(n,x)-lchoose(n,k))/(k-x)}
Feta <- function(x,eta){pbinom(x,n,invlogit(eta))}

##############################################################
# The function pi_eta(X,eta) automatically accounts for the
# fact that if k_alpha(X)=min(J) then a_alpha^st(X)=a_alpha(X)

piXeta <- function(x,eta){
	if (invlogit(eta)>=1){f <- 0} else {
	J <- c(0:(x-1),(x+1):n)

	# on (-infty,theta_0]
	t1 <- theta(0,x,n)
	if (is.na(t1)!=1 && eta<=t1){f <- 1-Feta(x-1,eta)}

	# on [theta_0,mode]
	k1 <- J[J<(x-1)]
	if (length(k1)>0){
	the1 <- theta(k1,x,n)
	the2 <- theta(k1+1,x,n)
	pos <- (the1<=eta)*(eta<the2)
	if (sum(pos)>0){f <- 1-Feta(x-1,eta)+Feta(max(k1*pos),eta)}}

	# mode
	the1 <- theta(x-1,x,n)
	the2 <- theta(x+1,x,n)
	if (eta>=the1 && eta<=the2){f <- 1}}

	# on [mode,theta_n]
	k2 <- J[J>(x+1)]
	if (length(k2)>0){
	the1 <- theta(k2-1,x,n)
	the2 <- theta(k2,x,n)
	kre <- sum(k2*(the1<eta)*(eta<=the2))
	if (kre>0){f <- 1-Feta(kre-1,eta)+Feta(x,eta)}}

	# on [theta_n,infty)
	t2 <- theta(n,x,n)
	if (is.na(t2)!=1 && eta>=t2){f <- Feta(x,eta)}
	f}

#####################################
# lower bound a_alpha^st(X)
if (x==0){pu <- 0} else {
J <- c(0:(x-1),(x+1):n)
k1 <- min(J)
pi1 <- piXeta(x,theta(k1,x,n))

# calculation of k_alpha(X)
if (pi1>=alpha){kal <- k1} else {
	k <- x-1
	while (k1<k-1){
		k2 <- floor((k+k1)/2)
		pi2 <- piXeta(x,theta(k2,x,n))
		if (pi2>=alpha){k <- k2} else {k1 <- k2}
	}
kal <- k
}

# calculation of a_alpha^st(X)
b1 <- theta(kal,x,n)
pi1 <- 1-Feta(x-1,b1)+Feta(kal-1,b1)
if (pi1<=alpha){b <- b1} else {
	b <- max(theta(kal-1,x,n),logit(del))
	pi <- 1-Feta(x-1,b)+Feta(kal-1,b)
	while (b1-b>del || pi1-pi>del){
		b2 <- (b+b1)/2
		pi2 <- 1-Feta(x-1,b2)+Feta(kal-1,b2)
		if (pi2>alpha){
			b1 <- b2
			pi1 <- pi2} else {
			b <- b2
			pi <- pi2}}}
pu <- invlogit(b)}

######################################
# upper bound b_alpha^st(X)
if (x==n){po <- 1} else {
J <- c(0:(x-1),(x+1):n)
k1 <- max(J)
pi1 <- piXeta(x,theta(k1,x,n))

# calculation of k_alpha(X)
if (pi1>=alpha){kau <- k1} else {
	k <- x+1
	pi <- 1
	while (k1>k+1){
		k2 <- floor((k+k1)/2)
		pi2 <- piXeta(x,theta(k2,x,n))
		if (pi2>=alpha){k <- k2} else {k1 <- k2}
	}
kau <- k
}

# calculation of b_alpha^st(X)
b1 <- theta(kau,x,n)
pi1 <- 1-Feta(kau,b1)+Feta(x,b1)

if (pi1<=alpha){
	b <- b1
	po <- pi1} else {
	b <- min(theta(kau+1,x,n),b1+n)
	pi <- 1-Feta(kau,b)+Feta(x,b)
	while (b-b1>del || pi1-pi>del){
		b2 <- (b+b1)/2
		pi2 <- 1-Feta(kau,b2)+Feta(x,b2)
		if (pi2>alpha){
			b1 <- b2
			pi1 <- pi2} else {
			b <- b2
			pi <- pi2}}}
po <- invlogit(b)}

c('a_alpha^St'=pu,'b_alpha^St'=po)
}