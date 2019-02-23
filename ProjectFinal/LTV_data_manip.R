# PARAMS --------------------------------
if(TRUE){
    UNIT <- "weeks"
}

# LIBRARIES AND HELPERS ---------------------------------------------------------------
if(TRUE){
    library(broom)
    library(devtools)
    library(RMySQL)
    library(plotly)
    library(dplyr)
    library(lubridate)
    library(data.table)
    library(rstudioapi)
    library(RColorBrewer)
    library(futile.logger)
    library(roxygen2)
    library(hash)
    library(magrittr)
    
    #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    devtools::load_all("../dalib/")
    
}
# GET THE DATA ------------------------------------------------------------
if(TRUE){
    rev_flows_old = readRDS("~/Google Drive/CMP Data Analysis/Data/20170811_Techday_revFlows_2015Jul_2017Jul_.rds")
    rev_flows_new = readRDS("~/Google Drive/CMP Data Analysis/Data/Rev_flows_20170101_20171231.rds")
    rev_flows_new$segment = NULL
    rev_flows = rbind(rev_flows_old, rev_flows_new)
    members = readRDS("~/Google Drive/CMP Data Analysis/Data/Members_20150701_20171231.rds")
}

get_outs <- function(data){
        Outs <- data %>% 
                mutate(numRebill = ifelse(type == "trial", 0, ifelse(type == "conversion", 1, round(withinDays/30,0) ))) %>% 
                mutate(Outlayers = ifelse(type == "conversion" & numRebill > 1 ,"out","in")) %>% 
                dplyr::filter(Outlayers == "out") %>% 
                select(cmp_member_id, Outlayers)
}
get_groups <- function(data, outs){
        data %>%  
                mutate(numRebill = ifelse(type == "trial", 0, ifelse(type == "conversion", 1, round(withinDays/30,0) ))) %>% 
                left_join(outs)  %>% 
                dplyr::filter(is.na(Outlayers)) %>% 
                group_by(numRebill) %>% 
                summarize(Count = n_distinct(cmp_member_id)) %>% 
                mutate(CountNorm = Count/max(Count)*100, Diff = lag(Count,default=first(Count))-Count) 
}


# DATA MANIPULATION -------------------------------------------------------
if(TRUE){

max_date <- as.Date(max(rev_flows$ref_date))
    
    # join members info and keep only joins before 2016 Aug
    BaDoinkVR<- rev_flows %>%
        dplyr::filter(firstProduct == "BaDoinkVR",billedProduct == "BaDoinkVR") %>% 
        dalib::join_member_info_to_rev_flows(., members, UNIT) %>%
        dalib::create_product_type(.) %>%  
        dplyr::filter(join_date < max_date - 365)
        #get_scaling_users_sv(.)
    
    SV <- rev_flows %>%
            dplyr::filter(firstProduct == "SeekVerify",billedProduct == "SeekVerify") %>% 
            dalib::join_member_info_to_rev_flows(., members, UNIT) %>%
            dalib::create_product_type(.) %>%  
            dplyr::filter(root_join_date < max_date-365)
    
    max_date <- as.Date(max(rev_flows$ref_date))
    WH <- rev_flows %>%
            dplyr::filter(firstProduct == "WellHello",billedProduct == "WellHello") %>% 
            dalib::join_member_info_to_rev_flows(., members, UNIT) %>%
            dalib::create_product_type(.) %>%  
            dplyr::filter(root_join_date < max_date - 365)
    
    WH_outs <- get_outs(WH)
    WH_groups <- get_groups(WH, WH_outs)
    
    VR_outs <- get_outs(BaDoinkVR)
    VR_groups <- get_groups(BaDoinkVR, VR_outs)
    
    SV_outs <- get_outs(SV)
    SV_groups <- get_groups(SV, SV_outs)
    
    VR_rulesets <- BaDoinkVR %>% 
            mutate(join_date = convert_date(join_date),
                   join_month = floor_date(join_date, unit = "months")) %>% 
            group_by(join_month, ruleset_id) %>% 
            summarize(joins = n_distinct(cmp_member_id))
    
    # Every member s.t. conversion date happens after more than 1 month
    Outs <- BaDoinkVR %>% 
        mutate(numRebill = round(withinDays/30,0)) %>% 
        mutate(Outlayers = ifelse(type == "conversion" & numRebill > 1 ,"out","in")) %>% 
        dplyr::filter(Outlayers == "out") %>% 
        select(cmp_member_id, Outlayers)
    
    
    # Every member s.t. conversion date happens after more than 1 month
    Outs_SV <- SV %>% 
            mutate(numRebill = round(withinDays/30,0)) %>% 
            mutate(Outlayers = ifelse(type == "conversion" & numRebill > 1 ,"out","in")) %>% 
            dplyr::filter(Outlayers == "out") %>% 
            select(cmp_member_id, Outlayers)
    
    Outs_WH <- WH %>% 
            mutate(numRebill = round(withinDays/30,0)) %>% 
            mutate(Outlayers = ifelse(type == "conversion" & numRebill > 1 ,"out","in")) %>% 
            dplyr::filter(Outlayers == "out") %>% 
            select(cmp_member_id, Outlayers)
    
    # Drop Outlayers and groupby number of Rebill cycles
    get_groups <- function(data, outs){
            data %>%  
                mutate(numRebill = ifelse(type == "trial", 0, ifelse(type == "conversion", 1, round(withinDays/30,0) + 1))) %>% 
                left_join(outs)  %>% 
                dplyr::filter(is.na(Outlayers)) %>% 
                group_by(numRebill) %>% 
                summarize(Count = n_distinct(cmp_member_id)) %>% 
                mutate(CountNorm = Count/max(Count)*100, Diff = lag(Count,default=first(Count))-Count) 
    }
    # Drop Outlayers and groupby number of Rebill cycles
    SV <- SV %>%  
            mutate(numRebill = round(withinDays/30,0)) %>% 
            left_join(Outs_SV)  %>% 
            dplyr::filter(is.na(Outlayers)) %>% 
            group_by(numRebill) %>% 
            summarize(Count = n_distinct(cmp_member_id)) %>% 
            mutate(CountNorm = Count/max(Count)*100, Diff = lag(Count,default=first(Count))-Count) 
    
    WH_ <- WH %>%  
            mutate(numRebill = round(withinDays/30,0)) %>% 
            left_join(Outs_WH)  %>% 
            dplyr::filter(is.na(Outlayers)) %>% 
            group_by(numRebill) %>% 
            summarize(Count = n_distinct(cmp_member_id)) %>% 
            mutate(CountNorm = Count/max(Count)*100, Diff = lag(Count,default=first(Count))-Count) 
    
# STATS -----------------------------------------------------------------
    
    
    

}

# PLOT HISTOGRAM ------------------------------------------------------------

plot_ly() %>% 
    add_trace(data = WH_groups ,
              x = ~numRebill,
              y = ~Count,
              #color = ~billedProduct,
              type = "bar") %>% 
    layout(title = "Histogram BadoinkVR Active (Absolute)",
           xaxis = list(title = ""),
           yaxis = list(title = "Counts"  ) )

plot_ly() %>% 
    add_trace(data = WH_groups ,
              x = ~numRebill,
              y = ~CountNorm,
              #color = ~billedProduct,
              type = "bar") %>% 
    layout(title = "Histogram BadoinkVR Active (Normalized)",
           xaxis = list(title = ""),
           yaxis = list(title = "Counts"  ) )

plot_ly() %>% 
    add_trace(data = WH_groups ,
              x = ~numRebill,
              y = ~Diff,
              #color = ~billedProduct,
              type = "bar") %>% 
    layout(title = "BadoinkVR loses per cycle",
           xaxis = list(title = ""),
           yaxis = list(title = "Counts"  ) )


saveRDS(SV_groups[1:13,], "~/Google Drive/CMP Data Analysis/BayesProjectMaster/SV_Bayes.rds")
getwd()
