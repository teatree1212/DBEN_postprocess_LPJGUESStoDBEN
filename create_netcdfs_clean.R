#!/usr/bin/env Rscript

# script to Create_DBEN_netcdf output - this updated version treats output for paper1 (site simulations)
#Annemarie Eckes-Shephard, July, August 2022
# location: /scripts

# changes made from create_netcdfs to create_netcdfs_paper1.R 
#Annemarie Eckes-Shephard, March 2023
# location: /scripts

#location names and amounts have been changed, so the loops are no longer relevant. Some experimental runs (with disturbance) 
# are no longer necessary, so I no longer distinguish between them either. 

#setup
#takes as argument jura or local (annemaries local mac, feel free to add your own arguments here)
#adjusts pathnames based on where the script is run
args = commandArgs(trailingOnly=TRUE)

#"e" for elevated and "a" for ambient 
co2levels <- args[2]

base.path  <- args[1]


#check whether to create elevated or ambient CO2 run output files:
if(args[2] == "e"){
  co2levels <- "PS_562ppm/"
}
if(args[2] == "a"){
  co2levels <- "PS_412ppm/"
}


library(dplyr)# to arrange data
library(reshape2) #s.a.
library(tidyr)# s.a.
library(ncdf4) # to write .nc file



#functions needed for post-processing several of the below variables:

# to set out NaNs = 0 in dfs:
#https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# apply the same principle to is.infinite function:
is.infinite.data.frame <- function(x)
  do.call(cbind, lapply(x, is.infinite))

create_nc<- function(model_name,var_name,simulation,location,var_meta,data_df,dim){
  # convenience function to turn data frame into array, and subsequently writeout content to netcdf files 
  # variable name
  # variable metadata to be added to nc file
  # data as df
  
  if(dim =='PFT'){
    ns <- names(data_df)[1:2]
    data_df <- data_df[with(data_df, order(Year, PFT)),]
    #create array [year, PFT]
    dM <- reshape2::melt(data_df, id.var=ns)
    data_ar <- array(dM$value, dim= c(length(PFT),sim_duration)) 
  }
  if(dim=='sizeclass'){
    data_df <- data_df[with(data_df, order(Year, sizeclass)),]
    #create array [year,sizeclass]
    dM <- reshape2::melt(data_df, id.var=c( 'Year', 'sizeclass'))
    data_ar <- array(dM$value, dim=c(16,sim_duration))
  }
  
  filename = paste0(file.processed.dir,paste(model_name,var_name,simulation,location,sep="_"),'.nc')  
  print('saving to')
  print(filename)
 
  ncnew <- nc_create(filename, list(var_meta))
  
  ncvar_put(ncnew, var_meta, t(data_ar), start=c(1,1), count=c(-1,-1),verbose = FALSE)
 
  nc_close(ncnew)
}

add_pft_placeholder <- function(df,PFT){
  #df=veg:structure, 
  #PFT= any pft number necessary,to be added  
  
  add_rows <-  as.data.frame(matrix(0,nrow=length(unique(df$Year)),ncol=dim(df)[2] ) )
  names(add_rows) <- names(df)
  
  add_rows$PFT <- PFT
  add_rows$IID <- PFT
  add_rows$Lon <- unique(df$Lon)
  add_rows$Lat <- unique(df$Lat)
  add_rows$Year <- unique(df$Year)
  return(add_rows)
}

add_missing_sizeclasses <- function(sizeclasses,var_object,var_name){
  #INPUT: sizeclasses vector from above
  # var_object, object that contains a variable that is missing some sizeclasses. must contain row Year, sizeclasses and varname
  # var_name name of the variable, is also third column name in var_object
  
  missing_sc <- setdiff(sizeclasses, var_object$sizeclass)
  if(length(missing_sc)!=0){ # if sizeclass is missing, add it:
    add_df <- data.frame(Year = year_start,sizeclass = missing_sc)
    add_df$NEW <- rep(0,length(missing_sc))
    names(add_df)[3] <- var_name
    # put the two together
    var_object <- rbind(var_object,add_df)
  }
  #reorder by year just to be sure:
  var_object <- var_object[with(var_object, order(Year, sizeclass)),]
  
  # now add all non-existent trees, and count them as frequency = 0
  #create list so use the var_name that is passed on:
  fill <-  list(0)
  names(fill)[1] <- var_name
  var_object <- as.data.frame(complete(var_object,Year,sizeclass, fill = fill ))
  
  
  
  # Find missing Years in nstem (typically the years with disturbace)
  missing_years <- setdiff(year_start:year_end, var_object$Year)
  if(length(missing_years)!=0){ # if there was a patch-destroying disturbance:
    add_df <- data.frame( Year = missing_years, sizeclass = rep(1:16,length(missing_years)))
    add_df$NEW <- 0
    names(add_df)[3] <- var_name
    # put the two together
    var_object <- rbind(var_object,add_df)
  }
  # return updated, object, now contains complete set of sizeclasses for all years.
  return(var_object)
}

add_grasses_missing_from_veg_structure_file <- function(PFT,var_object,var_name){
  #function adds a pft 8 with value 0 into PFT variable output, so that all pfts 
  # are represented. Even though for many variables, e.g. cwood related, these values are 0
  # INPUT: pft number vector from above
  # var_object, object that contains a variable that is missing some sizeclasses. must contain row Year, pft and varname
  # var_name name of the variable, is also third column name in var_object
  
  missing_pft <- setdiff(PFT, var_object$PFT)
  if(length(missing_pft)!=0){ # if sizeclass is missing, add it:
    add_df <- data.frame(Year = year_start:year_end,PFT = rep(missing_pft,length(year_start:year_end)))
    add_df$NEW <- rep(0,length(missing_pft))
    names(add_df)[3] <- var_name
    # put the two together
    var_object <- rbind(var_object,add_df)
  }
  #reorder by year just to be sure:
  var_object <- var_object[with(var_object, order(Year, PFT)),]
  
  # return updated, object, now contains complete set of PFTs for all years.
  return(var_object)
}

read_and_arrange_file_fornetcdfoutput <- function(var,varname, LPJGUESSoutput.type = "carbon_flux" ){
  # function to process closs-sizeclass  (DBEN: cmort variable) and stemloss (DBEN stemmort) files:
  # to rearrange them to be  in the formad ready for create_nc().
  # input : var table content of any closs_*.out file or stemloss_*.out file.
  # be able to handle both flux and count files in the same function
  # varname = string of variable name. I think needed for create_nc() to run. or maybe not necessary?
  # dens is from diam_dens.out, and is here implicit as global variable. maybe should be an input, just to be clearer
  
  if(LPJGUESSoutput.type == "carbon_flux"){
    # remove lat lon and year colmns
    var <- var[4:dim(var)[2]]
  }
  
  if(LPJGUESSoutput.type == "number_flux"){
    # consider patch density to get to proper count:
    # patch density is already taken care of within LPJG.
    var <- var[,4:dim(var)[2]] *1000 *10 # 
  
  }
  
  
  # rearrange df to fit with above-used sizeclass tibble and therefore dimensions in netcdf file:
  vartmp <- as.data.frame(t(var))
  
  #assemble everything:
  varnew <- data.frame(Year=sort(rep(year_start:year_end,times=length(sizeclasses))),
                       sizeclass = rep(sizeclasses,length(year_start:year_end)),
                       cmort = stack(vartmp)[1])
  names(varnew)[3] <- varname
  
  if(LPJGUESSoutput.type == "carbon_flux"){
    #unit conversion  year to second:
    #varnew[[varname]] <- varnew[[varname]]  * 1/year_to_seconds # keeping it as year. no point to convert, because we reconvert to year again later.
  }
  
  return(varnew)
}

merge_grasses <- function(df_in){
  # In the site simulations, we have now added grasses But lump C3G and C4G
  # (should they both be present) into one category, "Grasses". This function
  # performs this step.
  df_in$Grasses <- df_in$C3G + df_in$C4G
  df_in$C3G <- NULL
  df_in$C4G <- NULL
  return(df_in)
}

add_grassPFT <- function(df,grass_df){
  #function to add grass PFTs back into into dataframes created from the 
  # veg_structure (only woody PFTs) output, which does not contain grasses.
  # for this, the equivalent .out file for this variable must be used.
  # to add the correct variable into the woody PFT dataframe.
  df_tmp <-  unique(df[,c("Year")])
  df_tmp$PFT <- 8
  df_tmp$tmname <- grass_df$Grasses
  #rename column
  names(df_tmp)[3] <- names(df)[3]
  #merge temp df with large df which contains all woody PFTs from a site
  out <- rbind(df,df_tmp)
  #order correctly, otherwise .nc output gets scrambled
  out_final <- out[
    with(out, order(Year, PFT)),
  ]
  return(out_final)
}

add_missing_PFTs <- function(df,PFTnames){
  # df - data frame with content from LPJGUESS .out file, which contains PFTs as headers.
  # PFTnames - list of strings with PFT names that are used in DBEN.
  # function to add the PFTs that are required according to the protocol, but which do not exist/
  # do not get simulated at a given site. For the netcdf file, all PFTS should be present,
  # and non-simulated/nonexistent PFTS are given 0 values.
  
  # The .out file is organised by PFT, therefore have to subset for relevant columns, and fill 
  # create and fill nonexistent PFTs with 0
  
  all <- as.data.frame(matrix(0,nrow=dim(df)[1],ncol=length(PFTnames)))
  names(all) <- PFTnames
  dfnames <- names(df)
  
  # identify indeces for PFTs which data is available:
  # idx_PFT <- which(PFTnames %in% dfnames)
  # idx_var <- which(dfnames %in% PFTnames)
  # then fill those indeces in the df "all" with the data, to arrive at a df
  # that contains the not simulated PFTs with 0 values, and the 
  # simulated PFTs with their output values.
  # all[idx_PFT] <- df[idx_var]
  
  # must be organised in the order of PFTnames, so working with indeces does not satisfy this (bug where this had erroneously switched PFTs for lai at BIA)
  # this loop adresses this:
  for(PFT in PFTnames){
    if(PFT %in% dfnames){
      all[,PFT] <- df[,PFT]
    }

  }
  
  return(all)
  
}


##############
# some notes:

#model runs for paper0:
#run on kutabiri in folder /scratch/annemarie/DBEN/mechanistic_mortality_dben/build
#using TRENDY forcing from there, and some modifications for recycling climate over 30 years period. 
# used branch mechanistic_mortality_dben r11299, with cfx-input function merged into this, and some local modifications for climate recycling



########################################################
#  convert_to_ncdf
#<model name>_<variable name>_<simulation>_<location>
#Where “simulation” is P0  and P1 and “location” is FIN1, FIN2, BIA or BCI.

# for some unit conversions, must provide the number of patches
npatch       =  500 # 3 for testing, 500 for DBEN runs.
model_name     <- 'LPJGUESS'


path.simdata   <- paste0(base.path,'1_raw_paper0/',co2levels)
file.processed.dir = paste0(base.path,'2_processed_paper0/',co2levels)
         
path.simdata   <- paste0(base.path,co2levels)
file.processed.dir = paste0(base.path,co2levels)

simulation <- c("P0")#,"P1")
location <- c("BCI","FI","BIA")#,                                                   
PFTnames <- c("BINE","BNE", "IBS", "TeBS","TrIBE", "TrBE","TrBR","Grasses")

for(l in location){
 
   for (p in simulation){ 

      ### read in output file for different simulations:
      filename <- paste0(path.simdata,l,'/veg_structure','_',p,'.out')
      
      print(filename)
      veg_structure <- read.table(filename,header=TRUE)
      
      #retrieve number of patches:
      # sometimes not all possible patches are populated, therefore select multiple years ( i.e. <=1920)
      npatch = length(unique(veg_structure[which(veg_structure$Year <=1920),]$PID))
      
    # needed for some evaluations about missing years:
    year_start   <- min(veg_structure$Year)
    year_end     <- max(veg_structure$Year) ##using this manually now, because some P-simulations have +0 woody biomass at disturbance level 7, we need 0 -level entries, otherwise the script doesn't work
    
    print(year_end)

    ### add empty columns for PFTs that weren't simulated at the respective sites. 'Reindex' the PFTs, based on their pft number prescribed in the protocol:
    #Lon 23.25°, Lat 62.25° Boreal: Finland, OG: Pinus sylvestris (PFT1 -> place in PFT dimension 1 in netcdf), Picea abies (PFT2 -> place in PFT dimension   2 in netcdf), betula pendula (PFT3 -> place in PFT dimension 3 in netcdf)  (FIN1)
    #Lon 25.24°, Lat 61.25° Boreal: Finland, RG: Pinus sylvestris (PFT1), Picea abies (PFT2), betula pendula (PFT3) (FIN2)
    #Lon 23.75°, Lat 52.75° Temperate; Bialowieza: Pinus sylvestris (PFT1), Picea abies (PFT2) (BIA)
    #Lon -79.75°, Lat 9.25° Tropical; Barro Colorado Island:Tropical broadleaf evergreen shade intolerant (PFT4) + Tropical broadleaf evergreen shade   tolerant (PFT5) Tropical broadleaf deciduous (PFT6) (BCI)
    
    if(l  == "FI" ){
      
      #add some placeholder PFTs for the ones that are not simulated at FIN or BIA, but to get all PFT dimensions in the file:
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=4))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=5))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=6))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=7))
     
      
      #LPJGUESS starts indexing PFTS 0 starts indexing with 1, so switch to that.
      #lpjguess indexes PFTs in the order they appear in the insfile and only when they are active. So they need to be manually adjusted in order to fit the   correct dimension in the netcdf files:
      #Pinus sylvestris (PFT1): BINE, Picea abies (PFT2): BNE, betula pendula (PFT3):IBS  (FIN)
      # Grasses (C3G) (PFT 8)
      #NB: PFT3 and PFT4 are C3G and C4G grasses. They may not both occur at the same site.
      #NB: Grasses are not recorded in veg_structure.out, so commented out here and below
      #veg_structure[which(veg_structure$PFT==4),]$PFT <- 8
      #veg_structure[which(veg_structure$PFT==3),]$PFT <- 8
      veg_structure[which(veg_structure$PFT==2),]$PFT <- 3
      veg_structure[which(veg_structure$PFT==1),]$PFT <- 1
      veg_structure[which(veg_structure$PFT==0),]$PFT <- 2
      
      lname ="FIN" # quick fix to  have finland location be following Protocol naming conventions. 
      #IN all upstream LPJ-GUESS filse ( e.g. insfiles , in there, Finland simulations are called "FI" and not "FIN")
    }
    
    if(l  == "BIA"  ){ # only PFT 2, 4, and 8 present (at this point still called pft 0 and 1)
      
      #add some placeholder PFTs for the ones that are not simulated at BIA, but to get all PFT dimensions in the file:
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=1))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=5))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=6))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=7))
      
      # only active PFTs, and in the following order of insfiles:
      # Picea abies (PFT2): BNE, Betula spp. (PFT3): IBS, 
      # Carpinus betulus or Tilia cordata (PFT4): TeBS, 
      # Grasses (C3G) (PFT 8) (BIA)
      #veg_structure[which(veg_structure$PFT==4),]$PFT <- 8
      #veg_structure[which(veg_structure$PFT==3),]$PFT <- 8
      veg_structure[which(veg_structure$PFT==2),]$PFT <- 3
      veg_structure[which(veg_structure$PFT==1),]$PFT <- 4
      veg_structure[which(veg_structure$PFT==0),]$PFT <- 2
      
      lname ="BIA"
    }
    
    if(l  == "BCI"){ # 
      
      # only active PFTs, and in the following order.
      # Tropical broadleaf evergreen shade tolerant (PFT5)   "TrBE"
      # Tropical broadleaf evergreen shade intolerant (PFT5)   "TrIBE"
      # Tropical broadleaf deciduous (PFT7)   "TrBR"
      # Grasses (PFT8) (C4G)
      # which corresponds to the way I arranged them in the insfile
      # now they need to be 're-indexed' to correspond to PFT 5 to 8. 
      
      #veg_structure[which(veg_structure$PFT==4),]$PFT <- 8
      #veg_structure[which(veg_structure$PFT==3),]$PFT <- 8
      veg_structure[which(veg_structure$PFT==2),]$PFT <- 7
      veg_structure[which(veg_structure$PFT==1),]$PFT <- 5
      veg_structure[which(veg_structure$PFT==0),]$PFT <- 6
      
      #add some placeholder PFTs for the ones that are not simulated at BCI, but to get all PFT dimensions in the file:
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=1))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=2))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=3))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=4))
      
      lname ="BCI"
    }
    if(l  == "BCI/turnover_test_npatch100" ){ # 
      
      # only active PFTs, and in the following order.
      # Tropical broadleaf evergreen shade tolerant (PFT5)   "TrBE"
      # Tropical broadleaf evergreen shade intolerant (PFT5)   "TrIBE"
      # Tropical broadleaf deciduous (PFT7)   "TrBR"
      # Grasses (PFT8) (C4G)
      # which corresponds to the way I arranged them in the insfile
      # now they need to be 're-indexed' to correspond to PFT 5 to 8. 
      
      #veg_structure[which(veg_structure$PFT==4),]$PFT <- 8
      #veg_structure[which(veg_structure$PFT==3),]$PFT <- 8
      veg_structure[which(veg_structure$PFT==2),]$PFT <- 7
      veg_structure[which(veg_structure$PFT==1),]$PFT <- 5
      veg_structure[which(veg_structure$PFT==0),]$PFT <- 6
      
      #add some placeholder PFTs for the ones that are not simulated at BCI, but to get all PFT dimensions in the file:
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=1))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=2))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=3))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=4))
      
      lname ="BCI"
    }
    if(l == "BCI/turnover_test_dominant_pft"){ # ONLY one pft was run:
      veg_structure[which(veg_structure$PFT==0),]$PFT <- 5
      
      #add some placeholder PFTs for the ones that are not simulated at BCI, but to get all PFT dimensions in the file:
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=1))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=2))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=3))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=4))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=6))
      veg_structure <- rbind(veg_structure,add_pft_placeholder(df=veg_structure,PFT=7))
      
      lname ="BCI"
      
    }
    
    #For the simulations, certain years are not in the output file, probably because there is nothing to record on that year bc of.
    # the patch destroyingdisturbances.
    #so therefore the entry for 200 hast to be manually added. I just add one row here, because the below complete() function will 
    # take care of adding the others
    years_missing <-  setdiff(year_start:year_end, unique(veg_structure$Year))
    if(length(years_missing)!=0 ){
      add_row <-  as.data.frame(matrix(0,nrow = length(years_missing),ncol = dim(veg_structure)[2] ) )
      names(add_row) <- names(veg_structure)
      add_row$Year  <- years_missing
      add_row$Lat = unique(veg_structure$Lat)
      add_row$Lon = unique(veg_structure$Lon)
      add_row$PFT = 1
      add_row$IID = 1
      veg_structure <- rbind(veg_structure,add_row)
    }
    
    
    # test whether all PFTs that were simulated continue to have a complete record throughout the entire the simulation period. 
    # if that is not the case,  add them back in with 0s as values. E.g. sometimes a PFT disappears for a few years. all years must have at least one instance of each pft as an entry, therefore this  missing pft must be added in that year in the data with columns  of 0
    # find out which PFTs are missing implicitly, and add new row for them:
    #NOTE: this changes the order of columns, but I don't think it matters:
   
    #strangely, CrownArea is a character bc of "!" in P0 at FIN. manually changing that should it occur again:
    if(co2levels == "PS_412ppm/" & l=="FI" & p=="P6"){
    veg_structure$CrownA <- as.numeric(veg_structure$CrownA)
    veg_structure$CrownA[ which(is.na(veg_structure$CrownA))] <- 1.4
    }
    
    veg_structure <- complete(veg_structure,Lon,Lat,Year,PFT,
                              fill = list(SID = 0, PID = 0, IID = 1, Age = 1, 
                                          Height = 0, Boleht = 0, Diam = 0, 
                                          CrownA = 0, DensI = 0, LAI = 0, 
                                          Csapw = 0, Chrtw = 0, Cleaf = 0, 
                                          Croot = 0, OS_ID = 0, ANPP = 0, CUE=0, Cwood_incr=0, BAI=0 ))
  
  

  
    
    ###extract some metadata to create the netcdf files later:
    
    Lat <- unique(veg_structure$Lat)
    Lon <- unique(veg_structure$Lon)
    PFT <- c(unique(veg_structure$PFT),8) # must at this point contain nrs 1-7,
                                          # complemented with 8 =grasses
    
    #head(veg_structure)
    
    sizeclasses <- c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 200, 400) # last size class here is used to 
    # make sure all possible sizes of >200 dbh are covered. the column will be callse ">=200" in later analysis.
    
    year_start <- min(veg_structure$Year)
    year_end   <- max(veg_structure$Year)
    sim_duration <- length(seq(year_start,year_end))
    
    # create some derived variables post simulation + get right units:
    # year to seconds : 
    year_to_seconds = 1/60*60*24*365 # 31556952  # <-- approximate value from online ; #60*60*24*365 
    seconds_to_year = 60*60*24*365
    # get the correct crown area/m  per individual: ( #patch area  1000m2 -> patch to m2)
    # CrownA = m2
    # want. m2/ha              m2/indiv          * to indiv/m2         * m2/ha   
    veg_structure$CrownA   <- veg_structure$CrownA * veg_structure$DensI * 10000
    
    
    #Basal area must be derived 
    print("derive additional variables from veg_structure output")
    #first get radius( DBH/2), then get area of circle (^2*pi), then  scale up to individuals
    #want m2 /ha           m                 to m2   * to indiv/m2         * m2/ha 
    #now created within the model
    veg_structure$BA  <- (veg_structure$Diam/2)^2*pi * veg_structure$DensI * 10000
    
    # want: cm               m                 * to cm   -> used in this way for sizeclass classifications
    veg_structure$Diam   <- veg_structure$Diam * 100
    
    # want:m2/m2                      m2        indiv/m2   
    veg_structure$LAI   <- veg_structure$LAI * veg_structure$DensI   
    
    # indivs /ha                              to indiv/m2     * m2/ha 
    veg_structure$nindivs_per_hectare  <- veg_structure$DensI * 10000 
    
    
    # sizeclass
    veg_structure <- veg_structure %>% 
      mutate(sizeclass = 
               case_when( Diam < 1.0 ~ 1,
                          Diam < 5.0 ~ 5,
                          Diam < 10.0 ~ 10,
                          Diam < 15.0 ~ 15,
                          Diam < 20.0 ~ 20,
                          Diam < 30.0 ~ 30,
                          Diam < 40.0 ~ 40,
                          Diam < 50.0 ~ 50,
                          Diam < 60.0 ~ 60,
                          Diam < 70.0 ~ 70,
                          Diam < 80.0 ~ 80,
                          Diam < 90.0 ~ 90,
                          Diam < 100.0 ~ 100,
                          Diam < 150.0 ~ 150,
                          Diam < 200.0 ~ 200,
                          Diam < 400.0 ~ 400, # for all dhb-classes >=200
               )
      )
    
    
    
    # correct for the fact that in the above Trees with Diam =0 ( so nonexistent trees) get assigned a sizeclass
    veg_structure[which(veg_structure$Diam==0),]$sizeclass <- 1
    
    print("sizeclasses allocated")
    
    #omit nonexistent trees:
    print("###")
    print(dim(veg_structure))
    
    # create an object that holds complete cases of sizeclass  
    # for same reasons as when the complete() has been used above for PFTs
    veg_structure.sizeclass  <- complete(veg_structure,Year,Lat,Lon,sizeclass,
                                         fill = list(PFT=1, SID = 0, PID = 0, IID = 1, Age = 0, 
                                                     Height = 0, Boleht = 0, Diam = 0, 
                                                     CrownA = 0, DensI = 0, LAI = 0, 
                                                     Csapw = 0, Chrtw = 0, Cleaf = 0, 
                                                     Croot = 0, OS_ID = 0, ANPP = 0, CDEPT=0, CUE=0,
                                                     Cwood_incr = 0,BA = 0, BAI = 0,nindivs_per_hectare = 0))
    
    if(max(veg_structure$Diam)>=400){
      print("diameter quite wide...>400")
    }
    
    print("zero-entries created")
    
    #netcdf metadata common to all files:
    lon <- ncdim_def("longitude", "degrees_east", Lon)
    lat <- ncdim_def("latitude", "degrees_north", Lat)
    pft  <- ncdim_def("pft", "-", PFT)
    sizeclass_dim <- ncdim_def("sizeclass","< X cm",sizeclasses,longname = "sizeclass_by_diameter_at_breastheight")
    time <- ncdim_def(name="Time",units="years", vals = seq(year_start,year_end))#, unlim=TRUE)
    mv <- -999 #missing value to use
    

    #############################################################################################
    #NOTE:  densindiv  is in indiv /m2 
    # to get n indivs per patch do * 1000
    
    #carbon mass in vegetation by PFT kg C m-2"
    #############################################################################################
    var_name = "cveg"
    tmp <- veg_structure %>%
      group_by(Year,PFT)
    
    cveg <- tmp %>% summarise(cveg = sum(Csapw,Chrtw,Cleaf,Croot)/npatch ) #n=n() 
  
    # Why not use cmass.out in the first place for this variable?!...
    cmass_in <- read.table(paste0(path.simdata,l,'/cmass_',p,'.out'),head=TRUE) 
    cmass_in <- merge_grasses(cmass_in) # the two grass PFTs are merged 
    # (if both present) and counted as "Grasses" in DBEN.

    cveg <- add_grassPFT(cveg,cmass_in)
    
    var_temp <- ncvar_def("cveg", "kg C m-2", dim=list(time,pft), 
                          longname="Carbon_mass_in_vegetation_by_PFT", mv,verbose=TRUE) 
    
 
    
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df= cveg,dim='PFT')
    

    #############################################################################################
    # no grasses needed here:
    var_name = "AGcwood"
    tmp <- veg_structure %>%
      group_by(Year)
    
    AGcwood <- tmp %>% summarise(AGcwood_tmp = (sum(Csapw,Chrtw)*0.75),
                                 AGcwood = sum(AGcwood_tmp)/npatch ) #n=n() # with this line as sum(AGcwood_tmp, Cleaf), this would be AGB
    
    AGcwood$AGcwood_tmp <- NULL
    
    #prepare metadata and .nc file
    var_temp <- ncvar_def("AGcwood", "kg C m-2", dim=list(time), 
                          longname="Aboveground_woody_biomas", mv,verbose=TRUE) 
    
    filename = paste0(file.processed.dir,paste("LPJGUESS",var_name,simulation=p,location=lname,sep="_"),'.nc')  
    ncnew <- nc_create(filename, list(var_temp))
    
    #write out into .nc file
    ncvar_put(nc = ncnew, varid = var_temp, vals = array(AGcwood$AGcwood), start=1,count = c(-1),verbose =  TRUE)
    
    nc_close(ncnew)
    #############################################################################################
    
    #############################################################################################
    # no grasses needed here:
    var_name = "AGB"
    tmp <- veg_structure %>%
      group_by(Year)
    
    AGB <- tmp %>% summarise(AGcwood_tmp = (sum(Csapw,Chrtw)*0.75),
                                 AGcwood = sum(AGcwood_tmp,Cleaf)/npatch ) #n=n() # 
    
    AGcwood$AGcwood_tmp <- NULL
    
    #prepare metadata and .nc file
    var_temp <- ncvar_def("AGB", "kg C m-2", dim=list(time), 
                          longname="Aboveground_biomas", mv,verbose=TRUE) 
    
    filename = paste0(file.processed.dir,paste("LPJGUESS",var_name,simulation=p,location=lname,sep="_"),'.nc')  
    ncnew <- nc_create(filename, list(var_temp))
    
    #write out into .nc file
    ncvar_put(nc = ncnew, varid = var_temp, vals = array(AGcwood$AGcwood), start=1,count = c(-1),verbose =  TRUE)
    
    nc_close(ncnew)
    #############################################################################################
    
    
    #Carbon mass in wood by PFT (cwood)
    #############################################################################################
    var_name="cwood"
    
    tmp <- veg_structure %>%
      group_by(Year,PFT)
    
 
    cwood <- tmp %>% summarise(cwood=sum(Csapw,Chrtw)/npatch)#n = n() 
    
    # must add grasses into this, even though they are 0 
    cwood_final <-  add_grasses_missing_from_veg_structure_file(PFT, cwood, "cwood")
  
    
    #prep metadata
    var_temp <- ncvar_def("cwood", "kg C m-2", dim=list(time,pft), 
                          longname="Carbon_mass_in_wood_by_PFT", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df=cwood_final,dim='PFT')
    rm(tmp_vec_struct)
    #############################################################################################
    
    # leaf area index by PFT (lai) 
    #############################################################################################
    var_name="lai"
    
    lai_in <- read.table(paste0(path.simdata,l,'/lai_',p,'.out'),head=TRUE) # path.simdata,l,'/lai_',p,'.out'
    
    #spotted wrong order of PFtnames 
    # Lon      Lat    Year    BINE    TeBS     IBS     C3G     C4G   Total
     PFTnames <- c("BINE","BNE", "IBS", "TeBS","TrIBE", "TrBE","TrBR","Grasses")
    
    lai_in <- merge_grasses(lai_in) # the two grass PFTs are merged 
                                   # (if both present) and counted as "Grasses" in DBEN.
    lai_all <- add_missing_PFTs(df = lai_in, PFTnames) # add PFTs that were not simulated, 
    # to complete the df for the final netcdf output.
    
    # rearrange df to fit with above pft tibble and therefore dimensions in netcdf file:
    laitmp <- as.data.frame(t(lai_all))
    
    #add year as column sizeclass and then add the stacked data:
    lainew <- data.frame(Year = sort(rep(year_start:year_end,times=length(PFT))),
                         PFT =  rep(PFT,length(year_start:year_end)),
                         gpp = stack(laitmp)[1])
    names(lainew)[3] <-"lai"
    
    #prep metadata
    var_temp <- ncvar_def("lai", "m2 m-2", dim=list(time,pft), 
                          longname="leaf area index", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = lainew,dim='PFT')
    
    #############################################################################################
    
    # Crown area by PFT (CA)
    #############################################################################################
    var_name="CA"
    
    tmp <- veg_structure %>% group_by(Year,PFT)
    
    CA <- tmp %>% summarise(CA = sum(CrownA)/npatch) #,n = n()
    
    # must add grasses into this, even though they are 0 
    CA_final <-  add_grasses_missing_from_veg_structure_file(PFT, CA, "CA")
    
    
    #prep metadata
    var_temp <- ncvar_def("CA","m2 ha-1", dim=list(time,pft), 
                          longname="Crown area", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = CA_final,dim='PFT')
    #############################################################################################
    
    #Carbon mass in wood by size class (cwood_size) ; kg C m-2
    #############################################################################################
    var_name = "cwood_size"
    
    #subset
    tmp <- veg_structure.sizeclass %>% group_by(Year,sizeclass)
    
    #select only relevant variables
    #unitconversion, take into account patch numbers
    cwood_size <- as.data.frame(tmp %>% summarise(cwood_size_sum=sum(Csapw,Chrtw)/npatch))
    
    cwood_size <-  add_missing_sizeclasses(sizeclasses, cwood_size, 'cwood_size_sum')
    
    #sanity check: all sizeclasses present?
    # for(i in year_start:year_end){
    #    print(length(unique(cwood_size[which(cwood_size$Year==i),]$sizeclass)))
    #  }
    # yes
    cwood_size$cwood_size_sum <- as.numeric(sprintf("%18.10f",cwood_size$cwood_size_sum))
    
    #prepare metadata:
    var_temp <- ncvar_def("cwood_size", "kg C m-2", dim=list(time,sizeclass_dim), 
                          longname="Carbon_mass_in_wood_by_size_class", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = cwood_size,dim='sizeclass')
    #############################################################################################
    
    #Stem number by size class (nstem_size)
    #############################################################################################
    var_name = "nstem_size"
    
    #subset
    tmp <- veg_structure.sizeclass %>% group_by(Year,sizeclass)
    
    # Because I used complete() above, sometimes sizeclasses have Diam == 0, which means that while there is a sizeclass  
    # entry, no tree exists. so using n() to count will actually look like as if a tree in that sizeclass is there.
    # i wonder whether using the complete() function above was useful at all..?
    # here is the solution to this actue problem for now:
    # keep only  sizeclasses with Diam != 0
    tmptmp <- tmp[which(tmp$Diam !=0),]
    # and count them.
    nstem <- as.data.frame(tmptmp %>% summarise(n = sum(nindivs_per_hectare)/npatch ))
    
    nstem <-  add_missing_sizeclasses(sizeclasses, nstem, 'n')
    
    #prepare metadata:
    var_temp <- ncvar_def("nstem_size", "count ha-1", dim=list(time,sizeclass_dim), 
                          longname="Stem_number_by_size_class", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = nstem,dim='sizeclass')
    #############################################################################################
    
    #Basal Area by PFT (BA) m2 ha-1
    #############################################################################################
    var_name = "BA"
    
    tmp <- veg_structure %>% group_by(Year,PFT)
    
    # check: done per Individual?
    #tmp[which(tmp2$IID==91),c("Year","IID","Diam","BA")]
    # ..it seems like it. good.
    # then, sum up by PFT:
    #unitconversion, take into account patch numbers
    BA <- as.data.frame(tmp %>% summarise(BA_sum =sum(BA)/npatch))#n = n()
    
    
    # must add grasses into this, even though they are 0 
    BA_final <-  add_grasses_missing_from_veg_structure_file(PFT, BA, "BA_sum")

    
    #tmp[which(tmp$IID==91),c("Year","IID","Diam","BA")]
    #prep metadata
    var_temp <- ncvar_def("BA", "m2 ha-1", dim=list(time,pft), 
                          longname="Basal area", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = BA_final,dim='PFT')
    #############################################################################################
    
    #Basal Area by PFT (BA) m2 ha-1 by sizeclass
    #not officially part of DBEN output - needed for global simulations later
    #############################################################################################
    var_name = "BA_size"
    #subset
    tmp <- veg_structure.sizeclass %>% group_by(Year,sizeclass)
    #unitconversion, take into account patch numbers
    BA_size <- as.data.frame(tmp %>% summarise(BA_sum =sum(BA)/npatch))#n = n()
    #tmp[which(tmp$IID==91),c("Year","IID","Diam","BA")]
    
    BA <-  add_missing_sizeclasses(sizeclasses, BA_size, 'BA_sum')
    
    #prep metadata
    var_temp <- ncvar_def("BA_size", "m2 ha-1", dim=list(time,sizeclass_dim), 
                          longname="Basal area by sizeclass", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = BA,dim='sizeclass')
    
    
    #############################################################################################
    
    #Mean PFT HEIGHT (height) 
    #############################################################################################
    var_name = "height"
    
    tmp <- veg_structure %>% group_by(Year,PFT) %>% mutate(
                  Height_95 = quantile(Height, c(.90),na.rm=TRUE) )
    
    #only continue dealing with those in the 95th height percentile
    tmp <- tmp[which(tmp$Height>=tmp$Height_95),]
    
    height <- tmp %>% group_by(Year,PFT) %>% summarise(height = mean(Height))

    # must add grasses into this, even though they are 0 
    height_final <-  add_grasses_missing_from_veg_structure_file(PFT, height, "height")
    
    #prep metadata
    var_temp <- ncvar_def("height", "m", dim=list(time,pft), 
                          longname="Mean PFT height in the 95% height percentile", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = height_final,dim='PFT')
    #############################################################################################
    
    #Woody biomass growth -'woody carbon increment' (WBgrowth)  "kg C m-2 yr-1"
    #############################################################################################
    var_name= "WBgrowth"
    
    # now using a new variable outputted from LPJGUESS:Cwood_incr
    WBgrowth <- veg_structure %>% group_by(Year,PFT) %>% summarise(WBgrowth = sum(Cwood_incr)/npatch) 
    #WBgrowth[which(WBgrowth$WBgrowth<0),]$WBgrowth <- 0.0
    
    
    # must add grasses into this, even though they are 0 
    WBgrowth_final <-  add_grasses_missing_from_veg_structure_file(PFT, WBgrowth, "WBgrowth")
   
    
    #prep metadata
    var_temp <- ncvar_def("WBgrowth", "kg C m-2 yr-1", dim=list(time,pft), 
                          longname="Woody biomass growth", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = WBgrowth_final,dim='PFT')
    #############################################################################################
    
    #Basal area growth, BAgrowth, m2 ha-1 yr-1
    #############################################################################################
    var_name = "BAgrowth"
    
    #BAgrowth must be derived as in below - actually it now has been turned into a model output. both seems to yield the same result
    BAgrowth <-  veg_structure %>% group_by(PFT,IID) %>% arrange(Year) %>% mutate(diff = BA - lag(BA, default = first(BA))) 
    
    # now using a new variable outputted from LPJGUESS:BAI
    BAgrowth <- veg_structure %>% group_by(Year,PFT) %>% summarise(BAgrowth = sum(BAI)/npatch) #n = n()
    
    # must add grasses into this, even though they are 0 
    BAgrowth_final <-  add_grasses_missing_from_veg_structure_file(PFT, BAgrowth, "BAgrowth")
    
    #prep metadata
    var_temp <- ncvar_def("BAgrowth", " m2 ha-1 yr-1", dim=list(time,pft), 
                          longname="Basal_area_growth", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = BAgrowth_final,dim='PFT')
    #############################################################################################
    
    
    #vegetation carbon dynamics 
    #NB: this does not include grasses! So will not add up to cveg.out. 
    #This variable is not necessary for DBEN
    #############################################################################################
    var_name = "cveg_dyn"   # NB- not actually required in the comparison. just added here for some tests.
    
    names(cveg)[3] <- "cveg_sum"
    
    #
    cveg_dyn <- cveg %>% group_by(PFT) %>% arrange(Year) %>% mutate(diff = cveg_sum - lag(cveg_sum, default = first(cveg_sum))) 

    #remove column to enable read in by function create_nc
    cveg_dyn$cveg_sum <- NULL
    
    #write nc
    var_temp <- ncvar_def("cveg_dyn", "kg C m-2 yr-1", dim=list(time,pft), 
                          longname="Carbon_mass_flux_in_vegetation_by_PFT", mv,verbose=TRUE) 
    
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df=cveg_dyn,dim='PFT')
    #############################################################################################
    
    #NB: the below fluxes are read in from individual files:
    
    ###density - needed to derive all stemmort-specific variables
    dens <- read.table(paste0(path.simdata,l,'/diam_dens','_',p,'.out'),head=TRUE)
    
    
    #Carbon Mass Flux lost from live wood due to mortality or other turnover process
    #cmort, kg C m-2 yr-1 # NB not DBEN units according to protocol, but they are uptimately the units used in DBEN plotting.
    #############################################################################################
    var_name = "cmort"
    print(var_name)
    closs <- read.table(paste0(path.simdata,l,'/closs_',p,'.out'),head=TRUE)
    
    closs <- read_and_arrange_file_fornetcdfoutput(closs,varname = var_name,LPJGUESSoutput.type = "carbon_flux")
    
   
    #prepare metadata:
    var_temp <- ncvar_def("cmort", "kg C m-2 yr-1", dim=list(time,sizeclass_dim),
                          longname="Carbon_mass_flux_lost_from_life_wood_due_to_mortality_or_other_turnover_processes_mass_in_wood_by_size_class", mv,verbose=TRUE) 
    
    # unitconversion is done in function read_and_arrange_file_fornetcfoutputs()
    # will be converted back in the DBEN-format script, but we asked for units in s-1, so LPJG will provide them like that.
   
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = closs,dim='sizeclass')
    
    #############################################################################################
    
    # Stem number Flux lost from vegetation due to all mortality mechanisms
    # stemmort, Count ha-1yr-1
    #############################################################################################
    var_name = "stemmort"
    print(var_name)
    stemmort_r <-  read.table(paste0(path.simdata,l,'/stemloss_',p,'.out'),head=TRUE)
    
    #takes care of units and everything else
    stemmort <- read_and_arrange_file_fornetcdfoutput(var = stemmort_r,varname = var_name, LPJGUESSoutput.type = "number_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("stemmort", "Count ha-1 yr-1", dim=list(time,sizeclass_dim), 
                          longname="Stem_number_Flux_lost_from_vegetation_due_tomortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = stemmort,dim='sizeclass')
    #############################################################################################
    
    # Carbon Mass Flux out of Atmosphere due to Gross Primary Production on Landgppkg C m-2s-1
    #############################################################################################
    var_name = "gpp" 
    
    gpp_in <- read.table(paste0(path.simdata,l,'/agpp_',p,'.out'),head=TRUE) #path.simdata,l,'/agpp_',p,'.out'
    gpp_in <- merge_grasses(gpp_in) # the two grass PFTs are merged 
    # (if both present) and counted as "Grasses" in DBEN.
    gpp_all <- add_missing_PFTs(df = gpp_in, PFTnames) # add PFTs that were not simulated, 
    # to complete the df for the final netcdf output.
    
    
    
    # rearrange df to fit with above pft tibble and therefore dimensions in netcdf file:
    gpptmp <- as.data.frame(t(gpp_all))
    
    #add year as column sizeclass and then add the stacked data:
    gppnew <- data.frame(Year = sort(rep(year_start:year_end,times=length(PFT))),
                         PFT =  rep(PFT,length(year_start:year_end)),
                         gpp = stack(gpptmp)[1])
    names(gppnew)[3] <-"gpp"
    
    #prep metadata
    
    var_temp <- ncvar_def("gpp",  "kg C m-2 yr-1", dim=list(time,pft), 
                          longname="Carbon_Mass_Flux_out_of_Atmosphere_due_to_Gross_Primary_Production_on_Land", mv,verbose=TRUE) 
  
    
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = gppnew,dim='PFT')
    rm(gpp_all,gpptmp,gppnew)
    #############################################################################################
    
    
    # Carbon Mass Flux out of Atmosphere due to Net Primary Production on Landnppkg C m-2 s-1
    # [TO DO ] good variable for sanity check, whether veg_structure ANPP and the one I am using here add up 
    # but as I am using the anpp.out value anyways, I am not going to check that now.
    #############################################################################################
    var_name = "npp" 
    
    npp <-  read.table(paste0(path.simdata,l,'/anpp_',p,'.out'),head=TRUE)  #path.simdata,l,'/anpp_',p,'.out'
    npp <- merge_grasses(npp)
    all <- add_missing_PFTs(df = npp, PFTnames) # add PFTs that were not simulated, 
                                 # to complete the df for the final netcdf output.
   
    # rearrange df to fit with above pft tibble and therefore dimensions in netcdf file:
    npptmp <- as.data.frame(t(all))
    
    #add year as column
    nppnew <- data.frame(Year = sort(rep(year_start:year_end,times=length(PFT))),
                         PFT =  rep(PFT,length(year_start:year_end)),
                         npp = stack(npptmp)[1])
    
    names(nppnew)[3] <- "npp"
    
    
    #prep metadata
    var_temp <- ncvar_def("npp",  "kg C m-2 yr-1", dim=list(time,pft), 
                          longname="Carbon_Mass_Flux_out_of_Atmosphere_due_to_Net_Primary_Production_on_Land", mv,verbose=TRUE) 
    
    #unitconversion  year to second:
    nppnew$npp <- nppnew$npp #* year_to_seconds 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation = p,
              location = lname,var_meta = var_temp,data_df =  nppnew,dim='PFT')
    rm(npptmp,nppnew,all)
    #############################################################################################
    
    
    #Carbon Mass Flux out of Atmosphere due to Net Biospheric Production on Land nbp kg C m-2s-1
    #############################################################################################
    var_name = "nbp" 
    
    nbp_in <- read.table(paste0(path.simdata,l,'/cflux_',p,'.out'),head=TRUE) #path.simdata,l,'/cflux_',p,'.out'
    
    
    nbp <- nbp_in[,c("Year","NEE")] 
    
    # sanity check
    #sum(nbp[1,c(4:15)])
    # ddf<- ncdim_def ("Time", "years",nbp$Year)
    var_temp <- ncvar_def("nbp", "kg C m-2 s-1", dim= list(time), 
                          longname="Carbon_Mass_Flux_out_of_Atmosphere_due_to_Net_Biospheric_Production",
                          mv,verbose=TRUE) 
    
    
    filename = paste0(file.processed.dir,paste(model_name,var_name,simulation=p,location=lname,sep="_"),'.nc')  
 
    ncnew <- nc_create(filename, list(var_temp))
    
    #unitconversion  year to second:
    nbp$NEE <- nbp$NEE * year_to_seconds 
    
    ncvar_put(nc = ncnew, varid = var_temp, vals = array(nbp$NEE), start=1,count = c(-1),verbose =  TRUE)
    # with lat lon
    #ncvar_
    #testing NEE:
    par(mfrow=c(2,2))
    plot(nbp_in$Veg,type="l",ylim =c(-1,1),main ="Veg")
    plot(nbp_in$Repr,type="l",col="green" ,main ="Repr")
    plot(nbp_in$Soil,type="l",col="brown")
    plot(nbp_in$NEE,type="l",col="grey")

    nc_close(ncnew)
    rm(nbp)
    #############################################################################################
    
    
    ############################################################################################################
    ############################################################################################################
    
    # create .nc files for cmort mechanisms
    
    
    #Carbon Mass Flux lost from live wood due to age-based mortality
    #cmort_age, kg C m-2 s-1
    #############################################################################################
    var_name = "cmort_age"
    print(var_name)
    cmort <- read.table(paste0(path.simdata,l,'/closs_age_',p,'.out'),head=TRUE) #
    
    cmort <- read_and_arrange_file_fornetcdfoutput(var = cmort,varname =  var_name ,LPJGUESSoutput.type = "carbon_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("cmort_age",  "kg C m-2 yr-1", dim=list(time,sizeclass_dim), 
                          longname="Carbon_Mass_Flux_lost_from_live_wood_due_to_age-based_mortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta = var_temp,data_df = cmort,dim='sizeclass')
    #############################################################################################
    
    #Carbon Mass Flux lost from live wood due to disturbance-caused mortality
    #cmort_dist, kg C m-2 s-1
    #############################################################################################
    var_name = "cmort_dist"
    print(var_name)
    
    cmort <- read.table(paste0(path.simdata,l,'/closs_dist_',p,'.out'),head=TRUE)
    
    cmort <- read_and_arrange_file_fornetcdfoutput(var = cmort,varname =  var_name ,LPJGUESSoutput.type = "carbon_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("cmort_dist",  "kg C m-2 yr-1", dim=list(time,sizeclass_dim), 
                          longname="Carbon_Mass_Flux_lost_from_live_wood_due_to_disturbance_mortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta = var_temp,data_df = cmort,dim='sizeclass')
    #############################################################################################
    
    #Carbon Mass Flux lost from live wood due to fire caused mortality
    #cmort_fire, kg C m-2 s-1
    #############################################################################################
    var_name = "cmort_fire"
    
    cmort <- read.table(paste0(path.simdata,l,'/closs_fire_',p,'.out'),head=TRUE)
    
    cmort <- read_and_arrange_file_fornetcdfoutput(var = cmort,varname =  var_name ,LPJGUESSoutput.type = "carbon_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("cmort_fire", "kg C m-2 yr-1", dim=list(time,sizeclass_dim), 
                          longname="Carbon_Mass_Flux_lost_from_live_wood_due_to_fire_mortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta = var_temp,data_df = cmort,dim='sizeclass')
    #############################################################################################
    
    #Carbon Mass Flux lost from live wood due to low growth-efficiency caused mortality
    #cmort_greff, kg C m-2 s-1
    #############################################################################################
    var_name = "cmort_greff"
    print(var_name)
    cmort <- read.table(paste0(path.simdata,l,'/closs_greff_',p,'.out'),head=TRUE)
    
    cmort <- read_and_arrange_file_fornetcdfoutput(var = cmort,varname =  var_name ,LPJGUESSoutput.type = "carbon_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("cmort_greff",  "kg C m-2 yr-1", dim=list(time,sizeclass_dim), 
                          longname="Carbon_Mass_Flux_lost_from_live_wood_due_to_low_growth_efficiency_mortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta = var_temp,data_df = cmort,dim='sizeclass')
    #############################################################################################
    
    #Carbon Mass Flux lost from live wood due to other mortality mechanisms
    #cmort_other, kg C m-2 s-1
    #############################################################################################
    var_name = "cmort_other"
    
    cmort <- read.table(paste0(path.simdata,l,'/closs_other_',p,'.out'),head=TRUE)
    
    cmort <- read_and_arrange_file_fornetcdfoutput(var = cmort,varname =  var_name ,LPJGUESSoutput.type = "carbon_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("cmort_other",  "kg C m-2 yr-1", dim=list(time,sizeclass_dim), 
                          longname="Carbon_Mass_Flux_lost_from_live_wood_due_other_mortality_mechanisms", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta = var_temp,data_df = cmort,dim='sizeclass')
    #############################################################################################
    
    #number of stems lost from live wood due to age based mortality
    #stemmort_age, count ha-1
    #############################################################################################
    var_name = "stemmort_age"
    
    stemmort <-  read.table(paste0(path.simdata,l,'/stemloss_age_',p,'.out'),head=TRUE)
    
    stemmort <- read_and_arrange_file_fornetcdfoutput(var = stemmort,varname =  var_name ,LPJGUESSoutput.type = "number_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("stemmort_age", "count ha-1", dim=list(time,sizeclass_dim), 
                          longname="Stem_number_flux_lost_from_live_wood_due_to_age-based_mortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = stemmort,dim='sizeclass')
    #############################################################################################
    
    #number of stems lost from live wood due to disturbance-caused mortality
    #stemmort_dist, count ha-1
    #############################################################################################
    var_name = "stemmort_dist"
    
    stemmort <-  read.table(paste0(path.simdata,l,'/stemloss_dist_',p,'.out'),head=TRUE)
    
    stemmort <- read_and_arrange_file_fornetcdfoutput(var = stemmort,varname =  var_name ,LPJGUESSoutput.type = "number_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("stemmort_dist", "count ha-1", dim=list(time,sizeclass_dim), 
                          longname="Stem_number_flux_lost_from_live_wood_due_to_disturbance_mortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = stemmort,dim='sizeclass')
    #############################################################################################
    
    
    #number of stems lost from live wood due to fire based mortality
    #stemmort_fire, count ha-1
    # fire is turned off in simulations, so should be 0 . doing: max(stemmort[,4:19]) yes.
    #############################################################################################
    var_name = "stemmort_fire"
    
    stemmort <-  read.table(paste0(path.simdata,l,'/stemloss_fire_',p,'.out'),head=TRUE)
    
    stemmort <- read_and_arrange_file_fornetcdfoutput(var = stemmort,varname =  var_name ,LPJGUESSoutput.type = "number_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("stemmort_fire", "count ha-1", dim=list(time,sizeclass_dim), 
                          longname="Stem_number_flux_lost_from_live_wood_due_to_fire_mortality", mv,verbose=TRUE) 
    #write nc file
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = stemmort,dim='sizeclass')
    #############################################################################################
    
    #number of stems lost from live wood due to low growth-efficiency caused mortality
    #stemmort_greff, count ha-1
    #############################################################################################
    var_name = "stemmort_greff"
    
    stemmort <-  read.table(paste0(path.simdata,l,'/stemloss_greff_',p,'.out'),head=TRUE)
    
    stemmort <- read_and_arrange_file_fornetcdfoutput(var = stemmort,varname =  var_name ,LPJGUESSoutput.type = "number_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("stemmort_greff", "count ha-1", dim=list(time,sizeclass_dim), 
                          longname = "Stem_number_flux_lost_from_live_wood_due_to_low_growth_efficienty_caused_mortality", mv,verbose=TRUE) 
    #write nc file 
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = stemmort,dim='sizeclass')
    #############################################################################################
    
    #number of stems lost from live wood due to other mortality mechanisms
    #stemmort_other, count ha-1
    #############################################################################################
    var_name = "stemmort_other"
    
    stemmort <-  read.table(paste0(path.simdata,l,'/stemloss_other_',p,'.out'),head=TRUE)
    
    stemmort <- read_and_arrange_file_fornetcdfoutput(var = stemmort,varname =  var_name ,LPJGUESSoutput.type = "number_flux")
    
    #prepare metadata:
    var_temp <- ncvar_def("stemmort_other", "count ha-1", dim=list(time,sizeclass_dim), 
                          longname = "Stem_number_flux_lost_from_live_wood_due_to_other_mortality_mechanisms", mv,verbose=TRUE) 
    #write nc file 
    create_nc(model_name= 'LPJGUESS',var_name = var_name,simulation=p,location = lname,var_meta=var_temp,data_df = stemmort,dim='sizeclass')
    #############################################################################################
    
    
    #############################################################################################
    
    
  }# simulation loop
}# location loop

print('done')


