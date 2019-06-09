# Psych Open Statistics - psychostats package - v 0.1 - One way ANOVAs only (with Mauchly test of sphericity)

library("reshape2")

#source("https://raw.githubusercontent.com/anthonyhaffey/psychOpenStatsbook/master/psychoStatReleases/psychostats.0.1.R") 

poanova = function(this_data = this_data,        #how do I give insights into each input?
                   within = within,
                   between = between_col){
  
  #within-subject ANOVA
  if(length(within)>1){               #the user is analysing wide rather than long data
    
    within.frame = subset(this_data,select=within) #this focuses on the relevant data for one-way
    #within-subject ANOVAs.
    
    output$df_cond  = length(within.frame) - 1
    output$df_subjs = length(within.frame[,1]) - 1
    
    #################################
    # Mauchley's test of sphericity #
    #################################
    # Including an estimate of the Chi-Square
    
    this_mauch <- mauchly.test(lm(data.matrix(within.frame) ~ 1), X= ~1)
    d <- 1 - ((2 * (( output$df_cond )^2)+( output$df_cond )+2)/(6*( output$df_cond )*(output$df_subjs)))
    this_mauch$est.chi <- as.numeric(-(output$df_subjs)*d*log(this_mauch$statistic))
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
    
    oneway = list()
    oneway$subj_n   = length(this_data[,1])
    oneway$cond_k   = length(within)
    oneway$ss_cond  = output$subj_n * sum((cond_means - mean(cond_means))^2)
    oneway$ss_ws    = sum(ws_means)
    oneway$ss_subjs = output$cond_k * sum((pp_means - mean(pp_means))^2)
    oneway$ss_error = output$ss_ws - output$ss_subjs
    oneway$ms_cond  = output$ss_cond/output$df_cond
    oneway$ms_error = output$ss_error/(output$df_subjs * output$df_cond)
    oneway$df_error = output$df_cond * output$df_subjs
    oneway$f_value  = output$ms_cond/output$ms_error
    oneway$p_value = pf(output$f_value, output$df_cond, output$df_error, lower.tail = F)
    output$oneway = oneway
    
    ### tidying up the above code
    # the above can probably be replaced by a tapply functions in places ###
    ###
    
    return(output)
    
  } else {                               #the user is analysing long data
    
  }
}
