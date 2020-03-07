### Compensable Billing Model


# growth: Variation in presentation growth rate
# per.comp: Variation in percentage compensable 
# price.patient: Variation in $ per patient
# per.rec: Variation in % recoverable
# hrs: Variation in cost per patient

# an.pres: Annual presentations
# an.comp: Annual compensable
# price.pot: $ potentially recoverable
# price.act: $ actually recoverable
# cost: Cost of recovery in wages
# total: Total $ recovered

results2 = NULL
for (k in 1:1000)
{
  an.pres <- 91828
  an.comp <- 3287
  cost <- 71*30 #71hrs x $30/hr
  
      results = NULL
      for (k in 1:30)
        {
        growth <- rpert(1, min=1, mode=1.0274, max=1.0548, shape=4)
        an.pres <- an.pres*growth
        per.comp <- rpert(1, min=0.0322, mode=0.0358, max=0.0394, shape=4)
        an.comp <- an.pres*per.comp
        price.patient <- rpert(1, min=19.68, mode=21.87, max=24.06, shape=4)
        price.pot <- an.comp*price.patient
        per.rec <- rpert(1, min=0.80, mode=0.90, max=1, shape=4)
        price.act <- price.pot*per.rec
        hrs <- rpert(1, min=0.0233, mode=0.0260, max=0.0285, shape=4)
        cost <- an.comp*hrs
        total <- price.act - cost
        results = rbind(results, data.frame(total))
        }
    
    npv <- NPV(cf0=69756.69, cf=results$total, times=1:30, i=0.02, plot=FALSE)
            
    results2 = rbind(results2, data.frame(npv))
                }

summary(results2)