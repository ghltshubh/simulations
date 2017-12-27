# Set parameters
max.people = 50
max.trials = 500
plot.step  = 1

# load libraries
library(tidyverse)

#Set up an initial data frame
df<-data.frame("trial"=NA,"people"=NA, "val"=NA)

# Set up a common theme for plots
ztheme<-function(){
  theme_classic()+
    theme(panel.background=element_rect(fill="#F0F0F0", color="#F0F0F0"))+
    theme(plot.background=element_rect(fill="#F0F0F0", color="#F0F0F0"))}

#Run main loop
for(trial in 1:max.trials){
  # set up a buffer. Makes the program run a lot faster.
  buff<-data.frame("trial"=NA,"people"=NA, "val"=NA)
  for(people in 1:max.people){
    buff<-rbind(buff,data.frame("trial"=trial,"people"=people, "val"=NA))
    samp<-sample(1:365, people, replace=T)
    if(length(unique(samp))==length(samp)){
      buff$val[nrow(buff)]<-0
    }else{
      buff$val[nrow(buff)]<-1
    }; rm(samp)}
  df<-rbind(df, buff); rm(buff)
  print(paste(round(trial/(max.trials)*100, 2), "% Complete", sep=""))
}
df<-subset(df, !is.na(df$trial))
rm(max.people); rm(people); rm(trial)

# Generate multiple plots of result
for(n in seq(plot.step,max.trials,plot.step)){
  print(
    ggplot(summarise(group_by(subset(df, trial<=n), people), prob=mean(val)), aes(people, prob))+
      geom_bar(stat="identity", fill="steelblue1")+
      geom_smooth(se=F, color="black", method="loess")+
      scale_y_continuous(labels=scales::percent, limits=c(0,1))+
      labs(title="Birthday Paradox",
           subtitle=paste("Based on",n,"simulations."),
           x="Number of People in Room",
           y="One or More Matching Birthdays (True/False Ratio)",
           caption="created by /u/zonination")+
      ztheme())
  ggsave(paste("bday_", formatC(n,width=5,flag = "0"), ".png", sep=""), height=4.5, width=7, dpi=120, type="cairo-png")
}; rm(n)