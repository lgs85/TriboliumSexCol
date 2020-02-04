# Standard error ----------------------------------------------------------

se <- function(x) sd(x)/sqrt(length(x))




# Rounding ----------------------------------------------------------------


#Round to 2 or 3 dp and keep trailing zeros
round2 <- function(x,lessthan=F)
{
  if(lessthan == T && x < 0.01)
  {
    return('< 0.01')
  } else
  {
    sprintf(round(x,2), fmt="%.2f")
  }
}

round3 <- function(x,lessthan = T)
{
  if(lessthan == T && x < 0.001)
  {
    return('< 0.001')
  } else
  {
    sprintf(round(x,3), fmt="%.3f")
  }
}



# Numbers to words --------------------------------------------------------


num2word <- function(x)
{
  numbers <- c("one","two","three","four","five","six","seven","eight","nine")
  if(x < 10)
  {
    return(numbers[x])
  } else
  {
    return(as.character(x))
  }
}


# Overdispersion ----------------------------------------------------------

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
