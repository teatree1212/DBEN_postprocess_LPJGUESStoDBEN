#script to check the demographic carbon budget according to D-BEN criteria
#Annemarie Eckes-Shephard
#
# requires DGVMTools
# requires the D-BEN format file
# requires output created from create_netcdfs_clean.R, which converts LPJ-GUESS to standard "DBEN" format.

library(DGVMTools)
file.dir.lpjguess = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_demographic_model_intercomparison/D-BEN-site-sims_local/Paper_1/tmp/Outputs/LPJGUESS/1_raw/publication/"

par(mfrow=c(3,1))

     run  = "P0"
    for(site in c("BCI","FIN","BIA")){#} site="BCI"
    model_name ="LPJ-GUESS"
    
        DVM_WBgrowth <- get_model_output(var="WBgrowth",site=site,run="P0",
                                         model_name=model_name,
                                         co2_levels = "412ppm")
        DVM_cmort    <- get_model_output(var="cmort",site=site,run="P0",
                                         model_name=model_name,
                                         co2_levels = "412ppm")
        DVM_cwood    <- get_model_output(var="cwood_size",site=site,
                                         run="P0",model_name=model_name,
                                         co2_levels = "412ppm")
        
        
      
      #remove the first 30 years of equilibrium. here we focus on the regrowth phase:
      cwood <- omit_equilibrium_phase(DVM_cwood,model_name=model_name,site=site,selection_by_year=TRUE)
      cmort <- omit_equilibrium_phase(DVM_cmort,model_name=model_name,site=site,selection_by_year=TRUE)
      WBgrowth <- omit_equilibrium_phase(DVM_WBgrowth,model_name=model_name,site=site,selection_by_year=TRUE)
      
      lgth <- length(cwood@data$Total)
      # start from 2 to remove output writeout-timing problems with SEIB. This does not impact the validity of this test.
      test_Wbudget_tolerance_threshold(WBgrowth_in = WBgrowth@data$Total, cmort_in = cmort@data$Total,
                                       site = site,model_name = model_name, cwood_in = cwood@data$Total,
                                       time_range = 2:450)##post_dist_year[3]:410)
    }
  
     
mtext(outer=TRUE,line=1,side=1,"Years since disturbance")
mtext(outer=TRUE,line=2,side=2,"cwood, KgC m-2")