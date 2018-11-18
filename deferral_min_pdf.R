####################################
# Experiment on COMPAS data for Broward County
# as used by ProPublica (https://github.com/propublica/compas-analysis)
# Main paper: https://arxiv.org/pdf/1810.02003.pdf
# Code demonstrating how one could equalize the AP of African-American and
# Caucasian defendants by the min-PDF method.
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
                    days_b_screening_arrest, decile_score, is_recid, c_jail_in, c_jail_out) %>%
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

score_pdfs = rbind(black_score_pdf,white_score_pdf)


# MIN PDF target
min_pdf = pmin(black_score_pdf,white_score_pdf)
black_deferrals = 1:10
white_deferrals = 1:10
for(i in seq(1,10,by=1)){
  if(black_score_pdf[i] < white_score_pdf[i]){
    black_deferrals[i] = 0
    white_deferrals[i] = nrow(filter(df,race=="Caucasian", decile_score==i)) *(1 - black_score_pdf[i]/white_score_pdf[i])

  }
  if(black_score_pdf[i] > white_score_pdf[i]){
    black_deferrals[i] = nrow(filter(df,race=="African-American", decile_score==i)) *(1 - white_score_pdf[i]/black_score_pdf[i])
    white_deferrals[i] = 0
  }
}

black_nondeferrals = black_score_freq - black_deferrals
white_nondeferrals = white_score_freq - white_deferrals



deferrals = rbind(black_deferrals,white_deferrals)

deferrals_frac = rbind(black_deferrals/black_score_freq, white_deferrals/white_score_freq)


total_black_deferral_fraction = Reduce(f="+",x = black_deferrals,accumulate=F)/num_blacks
cat("Total black deferral fraction: ",total_black_deferral_fraction,"\n")

total_white_deferral_fraction = Reduce(f="+",x = white_deferrals,accumulate=F)/num_whites
cat("Total white deferral fraction: ",total_white_deferral_fraction,"\n")

# Pick colors to be used in the barplots
col_black = "gray38"
col_black_deferral = "gray70"
col_white = "orange2"
col_white_deferral = "moccasin"


# Generate barplot to display the deferrals under the min-PDF method
dat = cbind(rbind(min_pdf,black_score_pdf-min_pdf,0,0), rbind(0,0,min_pdf,white_score_pdf-min_pdf))
dat = dat[,c(1,11,2,12,3,13,4,14,5,15,6,16,7,17,8,18,9,19,10,20)]

pdf('min_method.pdf')
barplot(dat, space=c(0.5,0.), col=c(col_black,col_black_deferral,col_white,col_white_deferral), ylim = c(0,0.3),
        ylab="", xlab="Decile Score",names.arg=seq(1,10.5,by=0.5),
        main="Equalizing AP using the minimum method.\n Total deferral rate in each group = 24.5%",
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
barplot(dat,add=T, space=c(0.5,0.),angle=c(0,45,0,45),density=c(0,20,0,20), col=c("white","white","black","black"),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
title(ylab="Probability Density", line=2.7, cex.lab=cex_factor)
legend("topright",
        legend=c("African-American non deferrals","African-American deferrals",
                      "Caucasian non deferrals","Caucasian deferrals"),
        fill = c(col_black,col_black_deferral,col_white,col_white_deferral))
legend("topright",
        legend=c("African-American non deferrals","African-American deferrals",
                      "Caucasian non deferrals","Caucasian deferrals"),
        density=c(0,20,0,20), fill=c("white","white","black","black"))
abline(h=0,lwd=2)
dev.off()
