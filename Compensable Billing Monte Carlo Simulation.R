### Compensable Billing Model

# growth: Variation in presentation growth rate
# per.comp: Variation in percentage compensable 
# price.patient: Variation in $ per patient
# per.rec: Variation in % recoverable
# cost.patient: Variation in cost per patient

# an.pres: Annual presentations
# an.comp: Annual compensable
# price.pot: $ potentially recoverable
# price.act: $ actually recoverable
# cost: Cost of recovery in wages
# total: Total $ recovered

results2 = NULL
for (k in 1:50000) # Number of iterations
{
  an.pres <- 91828
  an.comp <- 3287
  
      results = NULL
      for (k in 1:30) # Simulates 30 years of data
        {
        growth <- rpert(1, min=1, mode=1.0274, max=1.0548, shape=4)
        per.comp <- rpert(1, min=0.0322, mode=0.0358, max=0.0394, shape=4)
        price.patient <- rpert(1, min=19.68, mode=21.87, max=24.06, shape=4)
        #price.patent <- rpert(1, min 60.14, mode=66.82, max=73.50, shape=4)  
        per.rec <- rpert(1, min=0.80, mode=0.90, max=1, shape=4)
        cost.patient <- rpert(1, min=0.0233, mode=0.0260, max=0.0285, shape=4)
        
        an.pres <- an.pres*growth
        an.comp <- an.pres*per.comp
        price.pot <- an.comp*price.patient
        price.act <- price.pot*per.rec
        cost <- an.comp*cost.patient
        total <- price.act - cost
        
        results = rbind(results, data.frame(total))
        }
    
    npv <- NPV(cf0=69756.69, cf=results$total, times=1:30, i=0.02, plot=FALSE)
            # npv function requires FinancialMath package
    
    results2 = rbind(results2, data.frame(npv))
                }

results2 %>% summary()
results2 %>% quantile(probs = c(0.025, 0.975), 1)
results2 %>% ggplot(aes(x=npv,
                     fill=I("light blue"), 
                     col=I("black"))) + geom_histogram(binwidth = 10000)
