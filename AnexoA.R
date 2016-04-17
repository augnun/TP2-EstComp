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
