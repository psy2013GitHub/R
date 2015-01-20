require('MASS')
month.abbr <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month <- factor(rep(month.abbr, length=72), levels = month.abbr)
models <- alply(ozone,1:2,function(x) rlm(x ~ month - 1, maxit=50))

# models failed to converge
failed <- laply(models, function(x) !x$converged)


# coefs array
coefs <- laply(models, coef)
dimnames(coefs)[[3]] <- month.abbr
names(dimnames(coefs))[3] <- "month"
