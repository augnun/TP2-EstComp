require("goftest")
#####################
# Gerador congruencial da U(0,1)
# set.seed(30)
x <- runif_congruencial(10^5)
ks.test(x, "punif")

# One-sample Kolmogorov-Smirnov test
# 
# data:  x
# D = 0.0021867, p-value = 0.7253
# alternative hypothesis: two-sided
# 
# Warning message:
#   In ks.test(x, "punif") :
#   ties should not be present for the Kolmogorov-Smirnov test

cvm.test(x)

# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: uniform distribution
# 
# data:  x
# omega2 = 0.065046, p-value = 0.7823

ad.test(x)

# Anderson-Darling test of goodness-of-fit
# Null hypothesis: uniform distribution
# 
# data:  x
# An = 0.69843, p-value = 0.5601

par(mfrow=c(2,1))
hist(x, main = "Vetor usando o gerador congruencial")
hist(runif(10^5), main = "Vetor usando runif()")

##################
# Gerador Normal Polar Direto
set.seed(30)
x <- rnorm_polarDireto((10^7)/2)
ks.test(x,"pnorm")
# 
# One-sample Kolmogorov-Smirnov test
# 
# data:  x
# D = 0.00020027, p-value = 0.8173
# alternative hypothesis: two-sided
cvm.test(x,"pnorm")
# 
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: Normal distribution
# 
# data:  x
# omega2 = 0.05309, p-value = 0.8573
ad.test(x,"pnorm")
# 
# 
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: Normal distribution
# 
# data:  x
# An = 0.42628, p-value = 0.8223

par(mfrow=c(2,1), mar=c(3,2,2,2))
plot(density(x), main = "Vetor usando o Método Polar de Marsaglia")
plot(density(rnorm(10^7)), main = "Vetor usando rnorm()")


#0 at 0.09
x <- format(pnorm_MC(x=seq(0,0.09,0.01)), digits = 6) 

resid1<- format(pnorm_MC(x=seq(0,0.09,0.01))-pnorm(q=seq(0,0.09,0.01)),digits=3)

#0.1 a 0.19
x <- format(pnorm_MC(x=seq(0.1,0.19,0.01)), digits = 6) 

resid1<- format(pnorm_MC(x=seq(0.1,0.19,0.01))-pnorm(q=seq(0.1,0.19,0.01)),digits=3)

#0.2 a 0.29
x <- format(pnorm_MC(x=seq(0.2,0.29,0.01)), digits = 6) 

resid1<- format(pnorm_MC(x=seq(0.2,0.29,0.01))-pnorm(q=seq(0.2,0.29,0.01)),digits=3)

#0.3 a 0.39
x <- format(pnorm_MC(x=seq(0.3,0.39,0.01)), digits = 6) 

resid1<- format(pnorm_MC(x=seq(0.3,0.39,0.01))-pnorm(q=seq(0.3,0.39,0.01)),digits=3)


#0.4 a 0.49
x <- format(pnorm_MC(x=seq(0.4,0.49,0.01)), digits = 6) 

resid1<- format(pnorm_MC(x=seq(0.4,0.49,0.01))-pnorm(q=seq(0.4,0.49,0.01)),digits=3)


#0.5 a 0.59
x <- format(pnorm_MC(x=seq(0.5,0.59,0.01)), digits = 6) 

resid1<- format(pnorm_MC(x=seq(0.5,0.59,0.01))-pnorm(q=seq(0.5,0.59,0.01)),digits=3)


#0.6 a 0.69
format(pnorm_MC(x=seq(0.6,0.69,0.01)), digits = 6) 

format(pnorm_MC(x=seq(0.6,0.69,0.01))-pnorm(q=seq(0.6,0.69,0.01)),digits=3)


#0.7 a 0.79
format(pnorm_MC(x=seq(0.7,0.79,0.01)), digits = 6) 

format(pnorm_MC(x=seq(0.7,0.79,0.01))-pnorm(q=seq(0.7,0.79,0.01)),digits=3)


#0.8 a 0.89
format(pnorm_MC(x=seq(0.8,0.89,0.01)), digits = 6) 

format(pnorm_MC(x=seq(0.8,0.89,0.01))-pnorm(q=seq(0.8,0.89,0.01)),digits=3)

#0.9 a 0.99
format(pnorm_MC(x=seq(0.9,0.99,0.01)), digits = 6) 
format(pnorm_MC(x=seq(0.9,0.99,0.01))-pnorm(q=seq(0.9,0.99,0.01)),digits=3)


#1.0 a 1.09
format(pnorm_MC(x=seq(1.0,1.09,0.01)), digits = 6) 
format(pnorm_MC(x=seq(1.0,1.09,0.01))-pnorm(q=seq(1.0,1.09,0.01)),digits=3)


#1.1 a 1.19
format(pnorm_MC(x=seq(1.1,1.19,0.01)), digits = 6) 
format(pnorm_MC(x=seq(1.1,1.19,0.01))-pnorm(q=seq(1.1,1.19,0.01)),digits=3)


#1.2 a 1.29
format(pnorm_MC(x=seq(1.2,1.29,0.01)), digits = 6) 
format(pnorm_MC(x=seq(1.2,1.29,0.01))-pnorm(q=seq(1.2,1.29,0.01)),digits=3)


#1.2 a 1.29
pnorm_MC(x=seq(1.2,1.29,0.01))


#1.3 a 1.39
pnorm_MC(x=seq(1.3,1.39,0.01))


#1.4 a 1.49
pnorm_MC(x=seq(1.4,1.49,0.01))



#1.5 a 1.59
pnorm_MC(x=seq(1.5,1.59,0.01))

#1.6 a 1.69
pnorm_MC(x=seq(1.6,1.69,0.01))


#1.7 a 1.79
pnorm_MC(x=seq(1.7,1.79,0.01))


#1.8 a 1.89
pnorm_MC(x=seq(1.8,1.89,0.01))


#1.9 a 1.99
pnorm_MC(x=seq(1.9,1.99,0.01))


#2.0 a 2.09
pnorm_MC(x=seq(2.0,2.09,0.01))
pnorm_MC(x=seq(2.1,2.19,0.01))
pnorm_MC(x=seq(2.2,2.29,0.01))
pnorm_MC(x=seq(2.3,2.39,0.01))
pnorm_MC(x=seq(2.4,2.49,0.01))
pnorm_MC(x=seq(2.5,2.59,0.01))
pnorm_MC(x=seq(2.6,2.69,0.01))
pnorm_MC(x=seq(2.7,2.79,0.01))
pnorm_MC(x=seq(2.8,2.89,0.01))
pnorm_MC(x=seq(2.9,2.99,0.01))
pnorm_MC(x=seq(3.0,3.09,0.01))
pnorm_MC(x=seq(3.1,3.19,0.01))
pnorm_MC(x=seq(3.2,3.29,0.01))
pnorm_MC(x=seq(3.3,3.39,0.01))
pnorm_MC(x=seq(3.4,3.49,0.01))
pnorm_MC(x=seq(3.5,3.59,0.01))
pnorm_MC(x=seq(3.6,3.69,0.01))
pnorm_MC(x=seq(3.7,3.79,0.01))
pnorm_MC(x=seq(3.8,3.89,0.01))
pnorm_MC(x=seq(3.9,3.99,0.01))


#POLAR 
sapply(pnorm_polar(x=seq(0.0,0.09,0.01)),print)

sapply(pnorm_polar(x=seq(0.1,0.19,0.01)),print)
sapply(pnorm_polar(x=seq(0.2,0.29,0.01)),print)
sapply(pnorm_polar(x=seq(0.3,0.39,0.01)),print)
sapply(pnorm_polar(x=seq(0.4,0.49,0.01)),print)
sapply(pnorm_polar(x=seq(0.5,0.59,0.01)),print)
sapply(pnorm_polar(x=seq(0.6,0.69,0.01)),print)
sapply(pnorm_polar(x=seq(0.7,0.79,0.01)),print)
sapply(pnorm_polar(x=seq(0.8,0.89,0.01)),print)
sapply(pnorm_polar(x=seq(0.9,0.99,0.01)),print)
sapply(pnorm_polar(x=seq(1.0,1.09,0.01)),print)
sapply(pnorm_polar(x=seq(1.1,1.19,0.01)),print)
sapply(pnorm_polar(x=seq(1.2,1.29,0.01)),print)
sapply(pnorm_polar(x=seq(1.3,1.39,0.01)),print)
sapply(pnorm_polar(x=seq(1.4,1.49,0.01)),print)
sapply(pnorm_polar(x=seq(1.5,1.59,0.01)),print)
sapply(pnorm_polar(x=seq(1.6,1.69,0.01)),print)
sapply(pnorm_polar(x=seq(1.7,1.79,0.01)),print)
sapply(pnorm_polar(x=seq(1.8,1.89,0.01)),print)
sapply(pnorm_polar(x=seq(1.9,1.99,0.01)),print)
sapply(pnorm_polar(x=seq(2.0,2.09,0.01)),print)
sapply(pnorm_polar(x=seq(2.1,2.19,0.01)),print)
sapply(pnorm_polar(x=seq(2.2,2.29,0.01)),print)
sapply(pnorm_polar(x=seq(2.3,2.39,0.01)),print)
sapply(pnorm_polar(x=seq(2.4,2.49,0.01)),print)
sapply(pnorm_polar(x=seq(2.5,2.59,0.01)),print)
sapply(pnorm_polar(x=seq(2.6,2.69,0.01)),print)
sapply(pnorm_polar(x=seq(2.7,2.79,0.01)),print)
sapply(pnorm_polar(x=seq(2.8,2.89,0.01)),print)
sapply(pnorm_polar(x=seq(2.9,2.99,0.01)),print)
sapply(pnorm_polar(x=seq(3.0,3.09,0.01)),print)
sapply(pnorm_polar(x=seq(3.1,3.19,0.01)),print)
sapply(pnorm_polar(x=seq(3.2,3.29,0.01)),print)
sapply(pnorm_polar(x=seq(3.3,3.39,0.01)),print)
sapply(pnorm_polar(x=seq(3.4,3.49,0.01)),print)
sapply(pnorm_polar(x=seq(3.5,3.59,0.01)),print)
sapply(pnorm_polar(x=seq(3.6,3.69,0.01)),print)
sapply(pnorm_polar(x=seq(3.7,3.79,0.01)),print)
sapply(pnorm_polar(x=seq(3.8,3.89,0.01)),print)
sapply(pnorm_polar(x=seq(3.9,3.99,0.01)),print)

pnorm_polar(x=seq(1.3,1.39,0.01))
pnorm_polar(x=seq(1.4,1.49,0.01))
pnorm_polar(x=seq(1.5,1.59,0.01))
pnorm_polar(x=seq(1.6,1.69,0.01))
pnorm_polar(x=seq(1.7,1.79,0.01))
pnorm_polar(x=seq(1.8,1.89,0.01))
pnorm_polar(x=seq(1.9,1.99,0.01))
pnorm_polar(x=seq(2.0,2.09,0.01))
pnorm_polar(x=seq(2.1,2.19,0.01))
pnorm_polar(x=seq(2.2,2.29,0.01))
pnorm_polar(x=seq(2.3,2.39,0.01))
pnorm_polar(x=seq(2.4,2.49,0.01))
pnorm_polar(x=seq(2.5,2.59,0.01))
pnorm_polar(x=seq(2.6,2.69,0.01))
pnorm_polar(x=seq(2.7,2.79,0.01))
pnorm_polar(x=seq(2.8,2.89,0.01))
pnorm_polar(x=seq(2.9,2.99,0.01))
pnorm_polar(x=seq(3.0,3.09,0.01))
pnorm_polar(x=seq(3.1,3.19,0.01))
pnorm_polar(x=seq(3.2,3.29,0.01))
pnorm_polar(x=seq(3.3,3.39,0.01))
pnorm_polar(x=seq(3.4,3.49,0.01))
pnorm_polar(x=seq(3.5,3.59,0.01))
pnorm_polar(x=seq(3.6,3.69,0.01))
pnorm_polar(x=seq(3.7,3.79,0.01))
pnorm_polar(x=seq(3.8,3.89,0.01))
pnorm_polar(x=seq(3.9,3.99,0.01))