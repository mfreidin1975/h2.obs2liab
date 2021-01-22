### recalculation of h2 on observed scale to h2 on liability scale

h2<-function(h2o,pop,samp) {  # h2 on observed scale, population prevalence, sample prevalence
              LT<-dnorm(qnorm(pop)) # liability threshold
              num<-h2o*pop^2*(1-pop)^2
              den<-LT^2*samp*(1-samp)
              return(num/den)
}
