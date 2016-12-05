# input cell populations as a percentage, # of dec places doesn't matter
# you only have to put in early, late, and dead (both PI and A4)
# alive is calculated after the fact
early = 0.206
dead = 0.63
alive = (1 - early - dead)

# change this to how many cells were actually analyzed
# you said 1000 in the past so that's what i used here
t.cells = 10000

# makes a character vector of length t.cells where each "state" is
# replicated a number of times proportional to the percents above
cells = as.factor(rep(c("Early","Dead","Alive"),+
                        +c(early*t.cells,dead*t.cells,alive*t.cells+1)))

# n.trials specifies how many samplings you want to do, trial.size how big the sample - 
# change to whatever you want
n.trials = 5
trial.size = 1000

# for each sampling, you'll get data about the percentage of each pop
# in the sampling
trials=matrix(data=NA,nrow=n.trials,ncol=3)
colnames(trials)=c("Alive","Dead","Early")
for (i in 1:n.trials){
  trials[i,]=summary(sample(cells,trial.size))/trial.size*100
}

# statistics
# p.percent is the -predicted- percent that you specified in the beginning
# everything else is pretty self-explanatory
mean = c(mean(trials[,1]),mean(trials[,2]),+
           +mean(trials[,3]))
p.percent = c(alive,dead,early)
stdev = c(sd(trials[,1]),sd(trials[,2]),+
            +sd(trials[,3]))
serror = c(sd(trials[,1])/sqrt(n.trials),sd(trials[,2])/sqrt(n.trials),+
             +sd(trials[,3])/sqrt(n.trials))

stats = rbind(p.percent,mean,stdev,serror)
colnames(stats)=colnames(trials)
final = rbind(trials,stats)

# final result will be a .csv file in your working directory that will
# have the results of all the random samplings and stats at the bottom
write.csv(final,file="trials.csv")
