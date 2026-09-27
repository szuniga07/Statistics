################################################################################
#                                   Week 3                                     #
################################################################################

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location,
#   remember to use forward slashes, "/"
folder_location = "C:/Users/szuni/Documents/kp/Education/Getting started in research/"

#2. We'll paste "folder_location" with any file name, you can always
#   skip that and type the full location. Pasting them together is
#   not required, just a shortcut.

##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
lbw <- read.csv( paste0(folder_location, "lbw.csv") )


#####################
## Power analysis  ##
#####################

#######################################
## Graph that shows concept of power ##
#######################################
rec <- function(x) (abs(x) < 1) *.5
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
x <- seq(from= -4, to = 6, by = .001)
x11(width=16, height=10)
plot(x, rec(x), type = "n", ylim = c(0,.5), lty = 1,
     ylab = " ", xlab="Standard Errors",
     main="Visualization of 0.80 power",
     cex.lab=1.5,cex.main=2,cex.sub=1.5, axes=FALSE)
axis(1)
lines(x, gauss(x), lty = 1, col="red", lwd=4)
lines(x, gauss(x-2.802), col="blue",
      lty = 2, lwd=4)                    #I added 1.96 (97.5 centile) + .8416 (80% above this point) to show power.
abline(v=1.96, col="green", lwd=5)
legend(-4.2,.525, legend = c("Null hypothesis", "Alternative hypothesis", "97.5th pctile of H0:"),
       lty = c(1:2,1), title ="Sampling Distributions:", bty = "n",
       col=c("red", "blue",  "green"), lwd=3, cex=1.4)
arrows(x0=2.1 ,y0=.43 ,x1=4 , y1=.43, col="blue", lwd=3)
text(x=2.05, y=.48, pos=4, cex = 1.5,
     labels="80% of the Alt. Hypothesis distribution")
text(x=2.05, y=.46, pos=4, cex = 1.5,
     labels="is to the right of the green line")

dev.off() # closes graph or just click 'x' on the graph to manually close

#################################################
##  Relationship between Power and sample size ##
#################################################
###Proportion example
samp <- 2:10000
rslt <- data.frame(N=rep(0,length(samp)),Power=rep(0,length(samp)))
for (i in 1:length(samp)) {
  rslt[i,1] <- samp[i]
  rslt[i,2] <- power.prop.test(n = samp[i], sig.level = 0.05, p1 = .050, p2 = .065)["power"]
}
x11(width=16, height=10)
plot(rslt$N, rslt$Power, type= "l", col=4, lwd=5,
     axes=F, ylab="Power", xlab="Sample size",
     main="Power as a function of sample size",
     xlim=c(0,10000), cex.lab=1.5,cex.main=1.5,cex.sub=1.5)
abline(h=.60, col=1, lty=2, lwd=2)     #50% power
#abline(v=2360, col=1, lty=2, lwd=2)    #50% power
abline(h=.70, col="purple", lty=2, lwd=2)     #50% power
#abline(v=2973, col="purple", lty=2, lwd=2)    #50% power
abline(h=.80, col="red", lwd=3)     #80% power
#abline(v=3780, col="red", lwd=3)    #80% power
abline(h=.90, col=3, lty=1, lwd=3)     #50% power
#abline(v=5060, col=3, lty=1, lwd=3)    #50% power
legend(5800,.58, c("Power levels", "0.60", "0.70", "0.80", "0.90"), # xy coordinates to begin legend.
       lwd=c(4,4,4,4,4), lty=c(1,2,2,1,1), cex=2, #
       col=c("white", "black", "purple", "red", "green"),bty="n") # gives the legend lines the correct color and width)dev.off()
axis(2,at=seq(0,1,.1), labels=seq(0,1,.1), las=1)
axis(1,at=seq(0,10000,500), labels=seq(0,10000,500), las=1)
box()

dev.off() # closes graph or just click 'x' on the graph to manually close

## Birth weight ##

############
## t-test ##
############

## Sample Size independent t-test ##

power.t.test(power=0.80, delta=300, sd=700, sig.level=0.05,
             type="two.sample", alternative="two.sided")

## Calculate the power level

# Need to first calculate the harmonic mean when group sizes are unequal
# Use this formula
round((2*(74 * 115))/(74 + 115), 0)


# n = 90 = harmonic mean of 74 smokers and 115 non-smokers
power.t.test(n=90, delta=300, sd=700, sig.level=0.05,
             type="two.sample", alternative="two.sided")


######################
## Chi-square test  ##
######################

# proportion of smokers and non-smokers moms with low birth weight babies
# 0.41 and 0.25

# Need to first calculate the harmonic mean when group sizes are unequal
# Use this formula
round((2*(74 * 115))/(74 + 115), 0)

###############
## Example 1 ##
###############

# Get sample size
power.prop.test(power= 0.80, p1=0.25, p2=0.41,
                sig.level=0.05, alternative="two.sided" )
# need 135 per group


###############
## Example 2 ##
###############

# Get power level
power.prop.test(n= 90, p1=0.25, p2=0.41,
                sig.level=0.05, alternative="two.sided" )
# power = 0.63

