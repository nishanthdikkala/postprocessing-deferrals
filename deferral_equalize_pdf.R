####################################
# Experiment on COMPAS data for Broward County
# as used by ProPublica (https://github.com/propublica/compas-analysis)
# Main paper: https://arxiv.org/pdf/1810.02003.pdf
# Code demonstrating how one could equalize the DOCS of African-American and
# Caucasian defendants by deferring on one of the two groups.
####################################

library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

# hyperparameter for plots
cex_factor = 1.6

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


# Defer on African-Americans so as to convert their DOCS into that of Caucasians
ratio_pdfs = black_score_pdf/white_score_pdf
delta = 1 - min(ratio_pdfs)
q_blacks = 1 - (1-delta)/ratio_pdfs
black_deferrals = black_score_freq*q_blacks
black_deferrals_frac = black_score_pdf*q_blacks
black_nondeferrals_frac = black_score_pdf*(1-q_blacks)

# Defer on Caucasians so as to convert their DOCS into that of African-Americans
ratio_pdfs = white_score_pdf/black_score_pdf
delta = 1 - min(ratio_pdfs)
q_whites = 1 - (1-delta)/ratio_pdfs
white_deferrals = white_score_freq*q_whites
white_deferrals_frac = white_score_pdf*q_whites
white_nondeferrals_frac = white_score_pdf*(1-q_whites)

# Pick colors to be used in the barplots
col_black = "gray38"
col_black_deferral = "gray70"
col_white = "orange2"
col_white_deferral = "moccasin"

# Generate barplot to display the deferrals of African-Americans to convert
# their DOCS into that of Caucasians
dat1 = rbind(black_nondeferrals_frac,black_deferrals_frac)
pdf('black_to_white.pdf')
barplot(dat1, beside=F,main=paste("Converting African-American DOCS \ninto Caucasian DOCS.",
                                    "Deferral rate = 60%"),xlab="Decile Score", ylab="",
        ylim=c(0,0.15), names.arg=1:10, space=c(0.5,0.5),
        col=c(col_black,col_black_deferral),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
barplot(dat1,space=c(0.5,0.5),add=T,angle=c(0,45), density=c(0,20),
        cex.axis= cex_factor)
title(ylab="Probability Density", line=2.85, cex.lab=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        fill = c(col_black,col_black_deferral), cex=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        density = c(0,20), fill=c("white","white"),cex=cex_factor)
abline(h=0)
dev.off()

# Printing some statistics
total_black_deferrals = Reduce(f="+",x = black_deferrals,accumulate=F)
cat("Total black deferrals: ",total_black_deferrals,"\n")
cat("Fraction of black deferrals: ", total_black_deferrals/num_blacks,"\n")


# Generate barplot to display the deferrals of Caucasians to convert
# their DOCS into that of African-Americans
dat2 = rbind(white_nondeferrals_frac,white_deferrals_frac)
pdf('white_to_black.pdf')
barplot(dat2, beside=F,main=paste("Converting Caucasian DOCS into \nAfrican-American DOCS.",
                                    "Deferral rate = 67%"),xlab="Decile Score",
        ylab="", ylim=c(0,0.3), names.arg=1:10, space=c(0.5,0.5),
        col=c(col_white,col_white_deferral),
        cex.names = cex_factor, cex.lab=cex_factor, cex.axis=cex_factor, cex.main=cex_factor)
barplot(dat2,space=c(0.5,0.5),add=T,angle=c(0,45), density=c(0,20), col=c("white","black"),
        cex.axis=cex_factor)
title(ylab="Probability Density", line=2.85, cex.lab=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        fill = c(col_white,col_white_deferral), cex=cex_factor)
legend("topright",
        legend=c("Non deferrals","Deferrals"),
        density = c(0,20),fill=c("white","black"), cex=cex_factor)
abline(h=0)
dev.off()

# Printing some statistics
total_white_deferrals = Reduce(f="+",x = white_deferrals,accumulate=F)
cat("Total white deferrals: ",total_white_deferrals,"\n")
cat("Fraction of white deferrals: ",total_white_deferrals/num_whites,"\n")
