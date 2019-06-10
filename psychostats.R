# Psych Open Statistics - psychostats package - v 0.1 - One way ANOVAs only (with Mauchly test of sphericity)


if(!require(reshape2)){install.packages("reshape2")}
if(!require(Rfast)){install.packages("Rfast")}

library("reshape2")
library("Rfast")

#source("https://raw.githubusercontent.com/anthonyhaffey/psychOpenStatsbook/master/psychoStatReleases/psychostats.0.1.R") 

po_anova = function(this_data = this_data,          #how do I give insights into each input?
                   within = within,
                   between = between_col){
  
  #within-subject ANOVA
  if(length(within)>1){                            #the user is analysing wide rather than long data
    
    within.frame = subset(this_data,select=within) #this focuses on the relevant data for one-way
    #within-subject ANOVAs.
    output = list()
    output$cond_k   = length(within)
    output$subj_n   = length(this_data[,1])
    output$df_cond  = length(within.frame) - 1
    output$df_subjs = length(within.frame[,1]) - 1
    
    #################################
    # Mauchley's test of sphericity #
    #################################
    # Including an estimate of the Chi-Square
    
    this_mauch <- mauchly.test(lm(data.matrix(within.frame) ~ 1), X= ~1)
    d <- 1 - ((2 * (( output$df_cond )^2)+( output$df_cond )+2)/(6*( output$df_cond )*(output$df_subjs)))
    this_mauch$est.chi <- as.numeric(-(output$df_subjs)*d*log(this_mauch$statistic))
    
    
    
    
    ## ez_anova hack to get the Greenhouse-Geiser and Hein-Feldt values - 
    #  would welcome a more direct way to access these values 
    ez_within = within.frame
    ez_within$pp = 1:length(within.frame[,1])
    ez_summary_data = melt(ez_within,
                           id.vars =c("pp"),
                           measure.vars=within)

    this_aov_ez <- aov_ez(id = "pp",
                          dv = "value",
                          data = ez_summary_data,
                          within="variable",
                          anova_table = "pes")

    summary(this_aov_ez)
    ez_summary = summary(this_aov_ez)
    this_mauch$gg_eps = ez_summary$pval.adjustments[colnames(ez_summary$pval.adjustments) == "GG eps"]
    this_mauch$hf_eps = ez_summary$pval.adjustments[colnames(ez_summary$pval.adjustments) == "HF eps"]
    
    output$mauchley = this_mauch
    
    
    ################################
    # One way within-subject ANOVA #
    ################################
    
    #mean for each within subject condition
    
    cond_means = colMeans(within.frame)
    ws_means = rep(0,length(within))
    for(i in 1:length(within.frame)){
      ws_means[i] = sum((within.frame[,i] - cond_means[i])^2)
    }
    pp_means = rowMeans(within.frame)
    
    output$ss_cond  = output$subj_n * sum((cond_means - mean(cond_means))^2)
    output$ss_ws    = sum(ws_means)
    output$ss_subjs = output$cond_k * sum((pp_means - mean(pp_means))^2)
    output$ss_error = output$ss_ws - output$ss_subjs
    output$ms_cond  = output$ss_cond/output$df_cond
    output$ms_error = output$ss_error/(output$df_subjs * output$df_cond)
    output$df_error = output$df_cond * output$df_subjs
    output$f_value  = output$ms_cond/output$ms_error
    output$p_value = pf(output$f_value, output$df_cond, output$df_error, lower.tail = F)
    
    
    ####################
    # Corrected values #
    ####################
    #Greenhouse-Geiser 
    output$df_cond_gg  = output$df_cond * output$mauchley$gg_eps
    output$df_error_gg = output$df_error * output$mauchley$gg_eps
    output$p_value_gg  = pf(output$f_value, output$df_cond_gg, output$df_error_gg, lower.tail = F)
    
    #Huyn-Feldt
    output$df_cond_hf  = output$df_cond * output$mauchley$hf_eps
    output$df_error_hf = output$df_error * output$mauchley$hf_eps
    output$p_value_hf  = pf(output$f_value, output$df_cond_hf, output$df_error_hf, lower.tail = F)
    
    oneway = list()
    oneway$ss_cond     = output$ss_cond 
    oneway$ss_ws       = output$ss_ws   
    oneway$ss_subjs    = output$ss_subjs
    oneway$ss_error    = output$ss_error
    oneway$ms_cond     = output$ms_cond 
    oneway$ms_error    = output$ms_error
    oneway$df_error    = output$df_error
    oneway$f_value     = output$f_value 
    oneway$p_value     = output$p_value
    oneway$df_cond_gg  = output$df_cond_gg
    oneway$df_error_gg = output$df_error_gg
    oneway$p_value_gg  = output$p_value_gg
    oneway$df_cond_hf  = output$df_cond_hf
    oneway$df_error_hf = output$df_error_hf
    oneway$p_value_hf  = output$p_value_hf
    output$oneway = oneway
    
    ### tidying up the above code
    # the above can probably be replaced by a tapply functions in places ###
    ###
    
    return(output)
    
  } else {                               #the user is analysing long data
    
  }
}
# 
# po_f_p_val <- function(this_list){
#   
#   
# }
