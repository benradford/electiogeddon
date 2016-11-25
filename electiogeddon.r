#####################################################
##                                                 ##
## electiogeddon                                   ##
## Benjamin J. Radford                             ##
## November 24, 2016                               ##
## http://www.benradford.com                       ##
## https://www.github.com/benradford/electiogeddon ##
##                                                 ##
## Data and code to evaluate vote discrepencies in ##
## the 2016 U.S. presidential election.            ##
##                                                 ##
#####################################################

## Load necessary libraries
library(rgdal)
library(lme4)
library(sjPlot)
library(MASS)
library(RColorBrewer)

predictions <- function(model)
{
  ## Function to draw random coefficients for lm predictions.
  ## args
  ##  model: a model object from lm.
  ## returns
  ##  A matrix of size 500,000 x D where D is the number of coefficients.
  se <- summary(model)$coefficients[,2]
  beta <- summary(model)$coefficients[,1]
  draws <- mvrnorm(500000,beta,diag(se^2))
  draws
}

## Vectors for converting state name to state abbreviation
state_list <- c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming','District of Columbia')
abbr_list <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY','DC')

## Read the datasets
pop <- read.csv("data/PopulationEstimates.csv",stringsAsFactors=F)
results <- read.csv("data/election_results.csv",stringsAsFactors=F)
machine <- read.csv("data/verifier-search.csv",stringsAsFactors=F)
unemployment <- read.csv("data/unemployment.csv",stringsAsFactors=F)
education <- read.csv("data/Education.csv",stringsAsFactors=F)
# census <- read.csv("data/census.csv",stringsAsFactors=F)
census <- read.csv("data/census_cleaned.csv",stringsAsFactors=F)

## Clean up the census data
# census <- census[census$AGEGRP==0 & census$YEAR==8,]
# census$percent_white <- (census$WA_MALE + census$WA_FEMALE) / census$TOT_POP
# census$abbr <- abbr_list[match(census$STNAME,state_list)]
# census$state_county <- paste(census$abbr, census$CTYNAME)

## Clean up the election results data
results$dem <- results$votes_dem > results$votes_gop
results$gop_ratio <- results$votes_gop / (results$votes_gop + results$votes_dem)
results$state_county <- paste(results$state_abbr, results$county_name)
results$percent_white <- census$percent_white[match(results$state_county, census$state_county)]

## Clean up the Verified Voter data
machine$FIPS <- as.numeric(unlist(lapply(machine$FIPS.code, FUN=function(x)ifelse(nchar(x)==9,substr(x,1,4),substr(x,1,5)))))
machine$Make <- toupper(machine$Make)
electric <- machine[grepl("DRE",machine$Equipment.Type),]

## Read in the US map and merge it with the other data by state and county
us <- readOGR(dsn="cb_2015_us_county_500k", layer="cb_2015_us_county_500k")
us$FIPS <- as.numeric(as.character(paste0(us$STATEFP,us$COUNTYFP)))
us$state_abbr <- results$state_abbr[match(us$FIPS,results$combined_fips)]
us$dem <- results$dem[match(us$FIPS,results$combined_fips)]
us$gop_ratio <- results$gop_ratio[match(us$FIPS,results$combined_fips)]
us$pop <- log(as.numeric(gsub(",","",pop$POP_ESTIMATE_2015[match(us$FIPS,pop$FIPS)])))
us$std_pop <- (us$pop - min(us$pop,na.rm=T))/(max(us$pop,na.rm=T)-min(us$pop,na.rm=T))
us$dre <- us$FIPS %in% electric$FIPS
us$unemployment <- unemployment$Unemployment_rate_2015[match(us$FIPS, unemployment$FIPS_Code)]
us$college <- education$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2010.2014[match(us$FIPS,education$FIPS.Code)]
us$percent_white <- results$percent_white[match(us$FIPS,results$combined_fips)]
test <- us$state_abbr
us <- us[!us$state_abbr %in% c("AK","HI"),]
us <- us[!is.na(us$state_abbr),]

## Output election map
png("election_map.png",width=1024,height=612)
myPal <- colorRampPalette(rev(brewer.pal(7,"RdBu")[c(1,2,4,6,7)]))
par(bg="#444444")
plot(us, col=myPal(100)[us$gop_ratio * 99 + 1], border=NA)
rect(grconvertX(seq(0,0.25,length.out=10), from="npc", to="user"),
     grconvertY(0,from = "npc", to = "user"),
     grconvertX(seq(0,0.25,length.out=10)+(0.25/10),from = "npc", to = "user"),
     grconvertY(0.05,from = "npc", to = "user"),col=myPal(10)[1:10],border=NA)
text(grconvertX(seq(0,0.25,length.out=3) + .25/20, from = "npc", to = "user"), grconvertY(0.075,from = "npc", to = "user"), c("Dem","Split","Rep"), col="white")
mtext("2016 Election Results by County",side=3, line=1, cex=2, col="white")
mtext("Benjamin J. Radford", side=3, line=-1, cex=1, col="white")
dev.off()

## Output header image
png("header_map.png",width=1200,height=600)
myPal <- colorRampPalette(rev(brewer.pal(7,"RdBu")[c(1,1,2,4,6,7,7)]))
par(bg="#444444")
plot(us, col=myPal(100)[us$gop_ratio * 99 + 1], border=NA) #ifelse(us$dre,"white",NA))
plot(us[us$dre==1,], border="white",add=T)
dev.off()

## Output population map
png("population_map.png",width=1024,height=612)
myPal <- colorRampPalette(brewer.pal(7,"YlGnBu"))
par(bg="#444444")
plot(us, col=myPal(100)[us$std_pop * 99 + 1], border=NA)
mtext("Population per County (logged)",side=3, line=1, cex=2, col="white")
mtext("Benjamin J. Radford", side=3, line=-1, cex=1, col="white")
dev.off()

## Output unemployment map
png("unemployment_map.png",width=1024,height=612)
myPal <- colorRampPalette(brewer.pal(7,"YlGnBu"))
par(bg="#444444")
plot(us, col=myPal(100)[(us$unemployment-min(us$unemployment,na.rm=T))/(max(us$unemployment,na.rm=T)-min(us$unemployment,na.rm=T)) * 99 + 1], border=NA)
mtext("Unemployment by County",side=3, line=1, cex=2, col="white")
mtext("Benjamin J. Radford", side=3, line=-1, cex=1, col="white")
dev.off()

## Output electronic voting map
png("electronic_voting.png",width=1024,height=612)
par(bg="#444444")
plot(us, col=ifelse(us$dre, "#b2182b", "#e0e0e0"), border=NA)
mtext("Electronic Voting Machines",side=3, line=1, cex=2, col="white")
mtext("Benjamin J. Radford", side=3, line=-1, cex=1, col="white")
dev.off()

## Run models
us_data <- data.frame("gop_vote"=us$gop_ratio, "electronic"=as.numeric(us$dre), "state"=as.factor(us$state_abbr), "population"=us$pop, "unemployment"=us$unemployment, "college"=us$college, "percent_white"=us$percent_white)
wi_data <- us_data[us_data$state=="WI",]
pa_data <- us_data[us_data$state=="PA",]
model_a <- lmer(gop_vote ~ electronic + (1|state), data=us_data)
model_b <- lmer(gop_vote ~ electronic + population + unemployment + college + percent_white + (1|state), data=us_data)
model_wi_a <- lm(gop_vote ~ electronic, data=wi_data)
model_wi_b <- lm(gop_vote ~ electronic + population + unemployment + college + percent_white, data=wi_data)
model_pa_a <- lm(gop_vote ~ electronic, data=pa_data)
model_pa_b <- lm(gop_vote ~ electronic + population + unemployment + college + percent_white, data=pa_data)

## Output model tables
sjt.lmer(model_a,model_b,file="national_table.html")
sjt.lm(model_wi_a,model_wi_b,model_pa_a,model_pa_b,file="wi_pa_table.html")

## Out-of-sample prediction
sample <- sample(1:nrow(us_data),1000,replace=F)
in_sample <- us_data[sample,]
out_sample <- us_data[!1:nrow(us_data) %in% sample,]
model_c <- lmer(gop_vote ~ electronic + population + unemployment + college + percent_white + (1|state), data=in_sample)
preds <- predict(model_c,us_data)
us$pred <- preds
us$pred_density <- -1
us$pred_density[sample] <- 20

## Out-of-sample prediction map
png("predicted_map.png",width=1024,height=612)
myPal <- colorRampPalette(rev(brewer.pal(7,"RdBu")[c(1,2,4,6,7)]))
par(bg="#444444")
plot(us, col=myPal(100)[us$pred * 99 + 1], border=NA, density=us$pred_density)
rect(grconvertX(seq(0,0.25,length.out=10), from="npc", to="user"),
     grconvertY(0,from = "npc", to = "user"),
     grconvertX(seq(0,0.25,length.out=10)+(0.25/10),from = "npc", to = "user"),
     grconvertY(0.05,from = "npc", to = "user"),col=myPal(10)[1:10],border=NA)
text(grconvertX(seq(0,0.25,length.out=3) + .25/20, from = "npc", to = "user"), grconvertY(0.075,from = "npc", to = "user"), c("Dem","Split","Rep"), col="white")
mtext("2016 Out-of-Sample Predicted Results",side=3, line=1, cex=2, col="white")
mtext("Benjamin J. Radford", side=3, line=-1, cex=1, col="white")
dev.off()

## Subset to states that have both electronic (DRE) and non-electronic voting
states_with_dre <- sort(unique(us$state_abbr[us$dre!=0]))
states_without_dre <- sort(unique(us$state_abbr[us$dre==0]))
states_with_dre <- intersect(states_with_dre, states_without_dre)

## Run models for states with both electronic and non-electronic voting
model_list <- list()
for(ss in states_with_dre)
{
  state_data <- us_data[us_data$state==ss,]
  state_data[complete.cases(state_data),]
  model_list <- c(model_list,list(lm(gop_vote ~ electronic + population + unemployment + college + percent_white, data=state_data)))
}
names(model_list) <- states_with_dre

demo_dre <- demo_nodre <- as.data.frame(t(colMeans(us_data[,!names(us_data) %in% c("state","gop_vote")],na.rm=T)))
demo_dre$electronic <- 1
demo_nodre$electronic <- 0

## Output state-wise maps
png("dre_states.png",width=1024,height=1024)
myPal <- colorRampPalette(rev(brewer.pal(7,"RdBu")[c(1,2,4,6,7)]))
par(mfrow=c(ceiling(sqrt(length(states_with_dre))), ceiling(sqrt(length(states_with_dre)))), mar=c(3.1,1.1,3.1,1.1), bg="#444444")
for(ss in states_with_dre)
{
  plot(us[us$state_abbr==ss,],col=ifelse(us$dre[us$state_abbr==ss],"#b2182b", "#999999"), border=NA)  
  mtext(ss,3,line=0,cex=2,col="white")
  
  if(sum(is.na(coef(model_list[[ss]]))) == 0)
  {
    draws <- predictions(model_list[[ss]])
    sims_dre <- draws %*% as.matrix(unlist(c(1,demo_dre)))
    sims_nodre <- draws %*% as.matrix(unlist(c(1,demo_nodre)))
    # sims_dre <- sort(sims_dre[round(.025*500000):round(0.975*500000)])
    # sims_nodre <- sort(sims_nodre[round(.025*500000):round(0.975*500000)])
    
    dens_dre <- density(sims_dre)
    dens_dre <- data.frame(x=dens_dre$x, y=dens_dre$y)
    dens_dre <- dens_dre[dens_dre$x >= 0 & dens_dre$x <= 1,]
    dens_dre <- rbind(c(0,0),dens_dre,c(1,0))
    
    dens_nodre <- density(sims_nodre)
    dens_nodre <- data.frame(x=dens_nodre$x, y=dens_nodre$y)
    dens_nodre <- dens_nodre[dens_nodre$x >= 0 & dens_nodre$x <= 1,]
    dens_nodre <- rbind(c(0,0),dens_nodre,c(1,0))
    
    
    dens_dre$y <- dens_dre$y/max(dens_dre$y)/4
    dens_nodre$y <- dens_nodre$y/max(dens_nodre$y)/4
    
    polygon(grconvertX(dens_nodre$x, from="npc", to="user"),grconvertY(dens_nodre$y, from="npc", to="user"), col="#e0e0e099", border="white")
    polygon(grconvertX(dens_dre$x, from="npc", to="user"),grconvertY(dens_dre$y, from="npc", to="user"), col="#b2182b99", border="white")
    
    mtext(sprintf("beta: %.2f  |  s.e.: %.2f  |  t: %.2f", coef(model_list[[ss]])["electronic"], summary(model_list[[ss]])$coefficients[,2]['electronic'], summary(model_list[[ss]])$coefficients[,3]['electronic']),1,line=0.25,col="white")
  }
}
plot(0,0,type="n",frame=F,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1))
rect(0,0,0.25,0.25,col="#e0e0e0",border="white")
rect(0,0.5,0.25,0.75,col="#b2182b",border="white")
text(0.3,0.25/2,"No E-Voting",cex=2,pos=4,col="white")
text(0.3,0.25/2+0.5,"E-Voting",cex=2,pos=4,col="white")
dev.off()




