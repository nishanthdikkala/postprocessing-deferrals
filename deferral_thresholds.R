####################################
# Experiment on COMPAS data for Broward County
# as used by ProPublica (https://github.com/propublica/compas-analysis)
# Main paper: https://arxiv.org/pdf/1810.02003.pdf
# Code demonstrating how one could equalize PPV and NPV across African-American and
# Caucasian defendants by using two thresholds per group.
####################################
library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

# hyperparameter for plots
cex_factor = 1.7


# Load and clean the data selecting for the desired fields
raw_data <- read.csv("./compas-scores-two-years.csv")
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count,
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>%
                    filter(days_b_screening_arrest <= 30) %>%
                    filter(days_b_screening_arrest >= -30) %>%
                    filter(is_recid != -1) %>%
                    filter(c_charge_degree != "O") %>%
                    filter(score_text != 'N/A')

df_blacks = filter(df,race=="African-American")
df_whites = filter(df,race=="Caucasian")
num_blacks = nrow(df_blacks)
num_whites = nrow(df_whites)

# Compute frequency distributions for COMPAS scores for both the groups
black_score_freq = 1:10
white_score_freq = 1:10
black_score_pdf = 1:10
white_score_pdf = 1:10
for (i in seq(1,10,by=1)){
  black_score_freq[i] = nrow(filter(df,race=="African-American", decile_score==i))
  black_score_pdf[i] = black_score_freq[i]/num_blacks
  white_score_freq[i] = nrow(filter(df,race=="Caucasian", decile_score==i))
  white_score_pdf[i] = white_score_freq[i]/num_whites
}


# function to compute PPV or NPV for a group given a threshold
predictiveValue <- function(threshold, positive=T, r="African-American"){
  if (positive){
    # all members with score >= threshold are marked will recidivate
    total_members = 0
    total_recid = 0
    if(threshold>10){
      # no one is marked will recidivate => PPV = 1
      return(1)
    }
    for(i in seq(threshold,10,by=1)){
      total_members = total_members + nrow(filter(df,race==r,decile_score==i))
      total_recid = total_recid + nrow(filter(df,race==r,decile_score==i, is_recid==1))
    }
    if(total_members==0){
      return(1)
    }
    else{
      return(total_recid/total_members)
    }
  }
  else{
    # all members with score <= threshold are marked will not recidivate
    total_members = 0
    total_not_recid = 0
    if(threshold <1){
      # no one is marked will not recidivate
      return(1)
    }
    for(i in seq(1,threshold,by=1)){
      total_members = total_members + nrow(filter(df,race==r,decile_score==i))
      total_not_recid = total_not_recid + nrow(filter(df,race==r,decile_score==i, is_recid==0))
    }
    if(total_members==0){
      return(1)
    }
    else{
      return(total_not_recid/total_members)
    }
  }
}



# Try all pairs of thresholds to equalize PPV and NPV simultaneously
black_ppv = 1:11
white_ppv = 1:11
black_npv = 1:11
white_npv = 1:11
for(t in seq(1,11)){
  black_ppv[t] = predictiveValue(t,positive=T,r="African-American")
  white_ppv[t] = predictiveValue(t,positive=T,r="Caucasian")
  black_npv[t] = predictiveValue(t-1,positive=F,r="African-American")
  white_npv[t] = predictiveValue(t-1,positive=F,r="Caucasian")
}
cat("Black PPV: ", black_ppv,"\n")
cat("White PPV: ", white_ppv,"\n")
cat("Black NPV: ", black_npv,"\n")
cat("White NPV: ", white_npv,"\n")
###


(black_score_pdf[3]+black_score_pdf[4])
print(white_score_pdf[5])

# Pick colors to be used in the barplots
col1 = "lightblue1"#"darkturquoise"
col2 = "dimgray"
col3 = "salmon"

pdf('black_thresholds.pdf')
barplot(black_score_pdf,beside=F,names.arg=1:10, ylab="", xlab="Decile Score", ylim=c(0,0.15),
        main=paste("Thresholds for African-American scores","\nPPV = 0.682, NPV = 0.699, ","Deferrals = 20%"),
        col=c(col1,col1,col2,col2,col3,col3,col3,col3,col3,col3),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
title(ylab="Probability Density", line=2.7, cex.lab=cex_factor)
legend("topright",
      legend = c("0", "Defer","1"),
      fill = c(col1, col2, col3))
abline(h=0, lwd=2)
abline(v=2.4, lwd=3)
abline(v=5, lwd=3)
dev.off()

pdf('white_thresholds.pdf')
barplot(white_score_pdf,beside=F,names.arg=1:10, ylab="", xlab="Decile Score", ylim=c(0,0.3),
        main=paste("Thresholds for Caucasian scores","\nPPV = 0.677, NPV = 0.684, ","Deferrals = 9%"),
        col=c(col1,col1,col1,col1,col2,col3,col3,col3,col3,col3),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
title(ylab="Probability Density", line=2.7, cex.lab=cex_factor)
legend("topright",
       legend = c("0", "Defer","1"),
       fill = c(col1, col2, col3))
abline(h=0, lwd=2)
abline(v=4.8, lwd=3)
abline(v=6.2, lwd=3)
dev.off()

##
