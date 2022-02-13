rm(list = ls())

pacman::p_load(data.table, dplyr, cepiigeodist, foreign, 
               sqldf, gravity, parallel, countrycode, 
               magrittr, Matrix, ggplot2, estimatr, 
               bannerCommenter, stringi, stringr, 
               tidyverse, haven, foreign, stargazer, R6,
               plm, lme4, beepr, prediction, clubSandwich, gridExtra)

############################################################################
############################################################################
###                                                                      ###
###                    SECTION 4: REGRESSION ANALYSIS                    ###
###                                                                      ###
############################################################################
############################################################################

#load data
dta4 = fread("dta4.csv")
dta_long = fread("dta_long.csv")

# create country pair variable
dta4$dyad = paste(dta4$iso1, "-",dta4$iso2)


# VERTICAL DEPTH

# 1. ADDING ALL PROVISIONS TOGETHER, REGARDLESS OF SUB-CATEGORY
dta4$vertical_all = dta4$depth

# Section 4.0: Functions:
# Fixed effects model
fixed_lm = R6Class(classname = "fixed_lm",
                      private = list(
                        ..x = NULL
                      ),
                      public = list(
                        
                        # declare objects named after DV variable names
                        depth = NULL,
                        breadth = NULL,
                        welfare = NULL, 
                        creation = NULL,
                        breadth_2 = NULL,
                        commitment = NULL,
                        AD_depth = NULL,
                        CP_depth = NULL,
                        CVD_depth = NULL,
                        ET_depth = NULL,
                        ENV_depth = NULL,
                        LM_depth = NULL,
                        INV_depth = NULL,
                        IPR_depth = NULL,
                        MIG_depth = NULL,
                        MOC_depth = NULL,
                        PP_depth = NULL,
                        ROR_depth = NULL,
                        SER_depth = NULL,
                        SPS_depth = NULL,
                        STE_depth = NULL,
                        SUB_depth = NULL,
                        TBT_depth = NULL,
                        TF_depth = NULL,
                        vertical_depth = NULL,
                        
                        
                        # initialize my function, specifying which formula to use
                        initialize = function(x) {
                          data = dta4
                          for (i in DV_variables) {
                            
                            formula = paste("log(Value) ~", i, "+ polity2_A + polity2_B + log(dist) + log(population_A) + log(population_B) + log(GDP_A) + log(GDP_B)")
                            
                            if (x == "time") {
                              
                              fit = plm(formula, data, effect = x, model = "within", index = c("year"))
                              
                              
                            } else if (x == "twoways") {
                              
                  
                              fit = plm(formula, data, effect = x, model = "within", index = c("dyad", "year"))
                              
                            } else {
                              
                              stop("ERROR: choose valid model specification")
                              
                            }
                            

                            
                            # create temporary list
                            summary = summary(fit)
                            fit_summary = list(fit, summary)
                            
                            # create object inside self
                            self[[paste(i)]] = fit_summary
                            
                            private$..x = x
                            
                          }
                          
                        },
                        
                        class = T))

manual_function = function(DV) {
  formula = paste("log(Value) ~", DV, "+ log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B ")
  fit = plm(formula, data = dta4, effect = "twoways", model = "within", index = c("dyad", "year"))
  return(fit)
}


dta4$gdp_dist_A = dta4$GDP_A / dta4$dist
dta4$gdp_dist_B = dta4$GDP_B / dta4$dist

# model inspection

######################## ANALYSIS  ####################

# scope
######
dta4$scope <- rowSums(dta_long[, c(
"AD_prov_05",
"AD_prov_06",
"AD_prov_07",
"AD_prov_08",
"AD_prov_09",
"AD_prov_10",
"AD_prov_11",
"AD_prov_12",
"AD_prov_13",
"AD_prov_14",
"AD_prov_15",
"AD_prov_16",
"AD_prov_17",
"AD_prov_18",
"AD_prov_19",
"AD_prov_20",
"AD_prov_21",
"AD_prov_37",
"AD_prov_01",
"CP_prov_04",
"CP_prov_05",
"CP_prov_06",
"CP_prov_07",
"CP_prov_08",
"CP_prov_09",
"CP_prov_10",
"CP_prov_11",
"CVD_prov_04",
"CVD_prov_05",
"CVD_prov_10",
"CVD_prov_14",
"Env_prov_03",
"Env_prov_04",
"Env_prov_05",
"Env_prov_06",
"Env_prov_07",
"Env_prov_08",
"Env_prov_09",
"Env_prov_10",
"Env_prov_11",
"Env_prov_12",
"Env_prov_49",
"Inv_prov_02",
"Inv_prov_03",
"Inv_prov_04",
"Inv_prov_10",
"Inv_prov_21",
"Inv_prov_34",
"Inv_prov_43",
"Inv_prov_54",
"Inv_prov_65",
"Inv_prov_66",
"Inv_prov_67",
"Mig_prov_21",
"Mig_prov_22",
"MoC_prov_02",
"MoC_prov_03",
"MoC_prov_04",
"MoC_prov_05",
"MoC_prov_06",
"MoC_prov_07",
"MoC_prov_08",
"PP_prov_01",
"PP_prov_03",
"Ser_prov_25",
"Ser_prov_26",
"Ser_prov_27",
"Ser_prov_35",
"Ser_prov_41",
"Ser_prov_42",
"Ser_prov_43",
"Ser_prov_48",
"Ser_prov_50",
"Ser_prov_51",
"Ser_prov_55",
"Ser_prov_56",
"Ser_prov_57",
"Ser_prov_58",
"SPS_prov_01",
"SPS_prov_03",
"SPS_prov_04",
"SPS_prov_59",
"STE_prov_03",
"STE_prov_04",
"STE_prov_05",
"STE_prov_06",
"STE_prov_07",
"STE_prov_08",
"STE_prov_09",
"STE_prov_10",
"STE_prov_11",
"STE_prov_12",
"Sub_prov_33",
"TBT_prov_12",
"TBT_prov_23")])

 # SCOPE
########

# enforcement 
##################
dta4$enforcement <- rowSums(dta_long[, c(
  "AD_prov_38",
  "AD_prov_39",
  "Env_prov_13",
  "Env_prov_14",
  "Env_prov_15",
  "Env_prov_16",
  "Inv_prov_61",
  "Inv_prov_62",
  "Inv_prov_63",
  "IPR_prov_106",
  "IPR_prov_108",
  "IPR_prov_11",
  "IPR_prov_110",
  "IPR_prov_111",
  "IPR_prov_112",
  "IPR_prov_113",
  "IPR_prov_114",
  "IPR_prov_115",
  "IPR_prov_116",
  "IPR_prov_118",
  "IPR_prov_119",
  "IPR_prov_12",
  "IPR_prov_120",
  "IPR_prov_121",
  "IPR_prov_122",
  "IPR_prov_123",
  "IPR_prov_124",
  "IPR_prov_125",
  "IPR_prov_126",
  "IPR_prov_128",
  "IPR_prov_129",
  "Mig_prov_25",
  "MoC_prov_103",
  "MoC_prov_104",
  "MoC_prov_105",
  "MoC_prov_107",
  "MoC_prov_108",
  "MoC_prov_109",
  "MoC_prov_11",
  "MoC_prov_110",
  "PP_prov_02",
  "PP_prov_92",
  "PP_prov_93",
  "PP_prov_94",
  "PP_prov_95",
  "RoR_prov_21",
  "Ser_prov_04",
  "STE_prov_45",
  "STE_prov_46",
  "STE_prov_47",
  "STE_prov_48",
  "STE_prov_49",
  "Sub_prov_21",
  "Sub_prov_22",
  "Sub_prov_23",
  "Sub_prov_24",
  "Sub_prov_25",
  "Sub_prov_26",
  "Sub_prov_27",
  "TBT_prov_21",
  "TBT_prov_22",
  "TBT_prov_24",
  "TBT_prov_25" )])
############

# cooperation
#############

dta4$cooperation <- rowSums(dta_long[, c(
"CP_prov_02",
"CP_prov_27",
"CP_prov_28",
"CP_prov_29",
"CP_prov_30",
"Env_prov_17",
"Env_prov_46",
"Env_prov_47",
"Env_prov_48",
"Inv_prov_52",
"Inv_prov_53",
"IPR_prov_130",
"IPR_prov_132",
"IPR_prov_65",
"LM_prov_15",
"Mig_prov_20",
"PP_prov_09",
"SPS_prov_56",
"SPS_prov_57",
"SPS_prov_58",
"STE_prov_50",
"Sub_prov_30",
"TBT_prov_26",
"TBT_prov_27",
"TBT_prov_28",
"TF_prov_18",
"TF_prov_26",
"TF_prov_27",
"TF_prov_37",
"TF_prov_38",
"TF_prov_39",
"TF_prov_40",
"TF_prov_49" )])
##########

# condition
#########
dta4$condition <- rowSums(dta_long[, c(
  "CP_prov_12",
  "CP_prov_14",
  "ET_prov_12",
  "ET_prov_16",
  "Inv_prov_12",
  "Inv_prov_29",
  "IPR_prov_28",
  "IPR_prov_33",
  "IPR_prov_34",
  "IPR_prov_40",
  "IPR_prov_41",
  "IPR_prov_42",
  "IPR_prov_43",
  "IPR_prov_47",
  "IPR_prov_49",
  "IPR_prov_50",
  "IPR_prov_51",
  "IPR_prov_53",
  "IPR_prov_54",
  "IPR_prov_55",
  "IPR_prov_63",
  "IPR_prov_73",
  "IPR_prov_78",
  "IPR_prov_82",
  "IPR_prov_83",
  "Mig_prov_12",
  "Mig_prov_13",
  "Mig_prov_14",
  "PP_prov_07",
  "RoR_prov_07",
  "RoR_prov_09",
  "RoR_prov_31",
  "Ser_prov_05",
  "Ser_prov_28",
  "Ser_prov_29",
  "Ser_prov_31",
  "Ser_prov_32",
  "Ser_prov_33",
  "Ser_prov_34",
  "Ser_prov_36",
  "Ser_prov_37",
  "Ser_prov_38",
  "SPS_prov_05",
  "SPS_prov_06",
  "SPS_prov_10",
  "SPS_prov_11",
  "SPS_prov_13",
  "SPS_prov_16",
  "SPS_prov_17",
  "SPS_prov_18",
  "SPS_prov_19",
  "SPS_prov_20",
  "SPS_prov_21",
  "SPS_prov_22",
  "SPS_prov_24",
  "SPS_prov_26",
  "SPS_prov_27",
  "SPS_prov_32",
  "STE_prov_28",
  "STE_prov_29",
  "STE_prov_30",
  "STE_prov_31",
  "STE_prov_32",
  "STE_prov_33",
  "STE_prov_34",
  "STE_prov_35",
  "Sub_prov_03",
  "Sub_prov_04",
  "Sub_prov_05",
  "TBT_prov_03",
  "TBT_prov_04",
  "TBT_prov_05",
  "TBT_prov_06",
  "TBT_prov_07",
  "TBT_prov_09",
  "TBT_prov_10",
  "TBT_prov_11",
  "TBT_prov_13",
  "TBT_prov_14",
  "TBT_prov_15",
  "TBT_prov_30",
  "TBT_prov_31",
  "TBT_prov_32",
  "TBT_prov_33",
  "TBT_prov_34",
  "TF_prov_44",
  "TF_prov_47",
  "RoR_prov_05",
  "RoR_prov_06",
  "RoR_prov_15",
  "RoR_prov_18" )])
###########

# transparency
#########
dta4$transparency <- rowSums(dta_long[, c(
  "CP_prov_23",
  "AD_prov_31",
  "AD_prov_36",
  "CVD_prov_09",
  "CVD_prov_11",
  "ET_prov_20",
  "ET_prov_21",
  "ET_prov_22",
  "ET_prov_23",
  "Inv_prov_56",
  "Inv_prov_57",
  "Inv_prov_60",
  "IPR_prov_109",
  "IPR_prov_20",
  "IPR_prov_21",
  "IPR_prov_22",
  "IPR_prov_23",
  "IPR_prov_24",
  "IPR_prov_25",
  "IPR_prov_26",
  "IPR_prov_27",
  "IPR_prov_39",
  "IPR_prov_52",
  "IPR_prov_56",
  "IPR_prov_61",
  "Mig_prov_08",
  "Mig_prov_09",
  "Mig_prov_11",
  "PP_prov_59",
  "PP_prov_60",
  "PP_prov_61",
  "PP_prov_88",
  "PP_prov_89",
  "PP_prov_90",
  "PP_prov_91",
  "Ser_prov_06",
  "Ser_prov_07",
  "Ser_prov_08",
  "Ser_prov_09",
  "Ser_prov_10",
  "Ser_prov_11",
  "Ser_prov_12",
  "Ser_prov_17",
  "Ser_prov_18",
  "Ser_prov_59",
  "Ser_prov_60",
  "Ser_prov_61",
  "Ser_prov_64",
  "SPS_prov_34",
  "SPS_prov_35",
  "SPS_prov_36",
  "SPS_prov_37",
  "SPS_prov_38",
  "SPS_prov_39",
  "SPS_prov_40",
  "SPS_prov_41",
  "SPS_prov_42",
  "SPS_prov_43",
  "STE_prov_40",
  "STE_prov_41",
  "STE_prov_42",
  "STE_prov_43",
  "STE_prov_44",
  "Sub_prov_16",
  "Sub_prov_17",
  "Sub_prov_18",
  "Sub_prov_20",
  "TBT_prov_16",
  "TBT_prov_17",
  "TBT_prov_18",
  "TF_prov_01",
  "TF_prov_02",
  "TF_prov_03",
  "TF_prov_04",
  "TF_prov_05",
  "TF_prov_06",
  "TF_prov_07",
  "TF_prov_08" )])
########
    
# environment
dta4$environment <- dta4$ENV_depth

# labor
dta4$labor <- dta4$LM_depth

# periphery
dta4$periphery <- dta4$ROR_depth + dta4$CVD_depth + dta4$AD_depth + dta4$PP_depth + dta4$TBT_depth + dta4$SPS_depth + dta4$STE_depth + dta4$SUB_depth + dta4$CP_depth


########### DESCRIPTIVE STATISTICS 2
descriptive_data2 = dta4[, c(8, 21, 22, 23, 24, 25, 26, 71:79)]

stargazer(descriptive_data2, type = "html", title = "Descriptive Statistics", out = "descriptives2.html")


# ======================= run the regression models
vertical_all = manual_function("vertical_all")
summary(vertical_all)

# scope
scope = manual_function("scope")
summary(scope)

# enforcement
enforcement = manual_function("enforcement")
summary(enforcement)

# cooperation
cooperation = manual_function("cooperation")
summary(cooperation)

# condition
condition = manual_function("condition")
summary(condition)

# transparency
transparency = manual_function("transparency")
summary(transparency)

# environment
environment = manual_function("environment")
summary(environment)

# labor
labor = manual_function("labor")
summary(labor)

# periphery
periphery = manual_function("periphery")
summary(periphery)



#predict.out.plm function:
#######
predict.out.plm<-function(
  estimate,
  formula,
  data,
  model="fd",
  pname="y",
  pindex=NULL,
  levelconstr=T
){
  # estimate=e.fe
  # formula=f
  # data=d
  # model="within"
  # pname="y"
  # pindex=NULL
  # levelconstr=T
  #get index of panel data
  if (is.null(pindex) && class(data)[1]=="pdata.frame") {
    pindex<-names(attributes(data)$index)
  } else {
    pindex<-names(data)[1:2]
  }
  if (class(data)[1]!="pdata.frame") { 
    data<-pdata.frame(data)
  }
  #model frame
  mf<-model.frame(formula,data=data)
  #model matrix - transformed data
  mn<-model.matrix(formula,mf,model)
  
  #define variable names
  y.t.hat<-paste0(pname,".t.hat")
  y.l.hat<-paste0(pname,".l.hat")
  y.l<-names(mf)[1]
  
  #transformed data of explanatory variables 
  #exclude variables that were droped in estimation
  n<-names(estimate$aliased[estimate$aliased==F])
  i<-match(n,colnames(mn))
  X<-mn[,i]
  
  #predict transformed outcome with X * beta
  # p<- X %*% coef(estimate)
  p<-crossprod(t(X),coef(estimate))
  colnames(p)<-y.t.hat
  
  if (levelconstr==T){
    #old dataset with original outcome
    od<-data.frame(
      attributes(mf)$index,
      data.frame(mf)[,1]
    )
    rownames(od)<-rownames(mf) #preserve row names from model.frame
    names(od)[3]<-y.l
    
    #merge old dataset with prediciton
    nd<-merge(
      od,
      p,
      by="row.names",
      all.x=T,
      sort=F
    )
    nd$Row.names<-as.integer(nd$Row.names)
    nd<-nd[order(nd$Row.names),]
    
    #construct predicted level outcome for FD estiamtions
    if (model=="fd"){
      #first observation from real data
      i<-which(is.na(nd[,y.t.hat]))
      nd[i,y.l.hat]<-NA
      nd[i,y.l.hat]<-nd[i,y.l]
      #fill values over all years
      ylist<-unique(nd[,pindex[2]])[-1]
      ylist<-as.integer(as.character(ylist))
      for (y in ylist){
        nd[nd[,pindex[2]]==y,y.l.hat]<-
          nd[nd[,pindex[2]]==(y-1),y.l.hat] + 
          nd[nd[,pindex[2]]==y,y.t.hat]
      }
    } 
    if (model=="within"){
      #group means of outcome
      gm<-aggregate(nd[, pname], list(nd[,pindex[1]]), mean)
      gl<-aggregate(nd[, pname], list(nd[,pindex[1]]), length)
      nd<-cbind(nd,groupmeans=rep(gm$x,gl$x))
      #predicted values + group means
      nd[,y.l.hat]<-nd[,y.t.hat] + nd[,"groupmeans"]
    } 
    if (model!="fd" && model!="within") {
      stop('funciton works only for FD and FE estimations')
    }
  }
  #results
  results<-p
  if (levelconstr==T){
    results<-list(results,nd)
    names(results)<-c("p","df")
  }
  return(results)
}
#########


# output vertical depth table:
stargazer(vertical_all, scope, enforcement, cooperation, condition, transparency, 
          title = "Table 4: Vertical Depth Models",
          column.separate = c(1,1,1,1,1,1),
          covariate.labels = c("All Provisions", "Scopes and Definitions", "Enforcement Mechanisms", "Cooperation", "Conditions and Obligations", "Transparency", "GDP A (log)","GDP B (log)","Population A (log)","Population B (log)","Polity A","Polity B"),
          dep.var.labels = "Trade Volumes in US Dollars (thousands)",
          model.numbers = FALSE,
          order = c(1:12),
          type = "html",
          style = "ajps", 
          out = "vertical.html")


# output horizontal depth table:
stargazer(environment, labor, periphery, 
          title = "Table 5: Horizontal Depth Models",
          column.separate = c(1,1,1),
          covariate.labels = c("Environmental Law", "Labor Market", "Periphery", "GDP A (log)","GDP B (log)","Population A (log)","Population B (log)","Polity A","Polity B"),
          dep.var.labels = "Trade Volumes in US Dollars (thousands)",
          model.numbers = FALSE,
          order = c(1:12),
          type = "html",
          style = "ajps", 
          out = "horizontal.html")

# robustness checks
# vertical_all
vertical_all_robust = plm(formula = log(Value) ~ vertical_all + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
vertical_all_test = phtest(vertical_all, vertical_all_robust, model = "within", method = "chisq")

# scope
scope_robust = plm(formula = log(Value) ~ scope + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
scope_test = phtest(scope, scope_robust, model = "within", method = "chisq")

# enforcement
enforcement_robust = plm(formula = log(Value) ~ enforcement + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
enforcement_test = phtest(enforcement, enforcement_robust, model = "within", method = "chisq")

# cooperation
cooperation_robust = plm(formula = log(Value) ~ cooperation + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
cooperation_test = phtest(cooperation, cooperation_robust, model = "within", method = "chisq")

# condition
condition_robust = plm(formula = log(Value) ~ condition + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
condition_test = phtest(condition, condition_robust, model = "within", method = "chisq")

# transparency
transparency_robust = plm(formula = log(Value) ~ transparency + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
transparency_test = phtest(transparency, transparency_robust, model = "within", method = "chisq")

# environment
environment_robust = plm(formula = log(Value) ~ environment + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
environment_test = phtest(environment, environment_robust, model = "within", method = "chisq")

# labor
labor_robust = plm(formula = log(Value) ~ labor + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
labor_test = phtest(labor, labor_robust, model = "within", method = "chisq")

# periphery
periphery_robust = plm(formula = log(Value) ~ periphery + log(gdp_dist_A) + log(gdp_dist_B) + log(population_A) + log(population_B) + polity2_A + polity2_B, data = dta4, model = "random", index = c("dyad", "year"))
periphery_test = phtest(periphery, periphery_robust, model = "within", method = "chisq")

h_tests = as.data.frame(rbind(vertical_all_test, scope_test, enforcement_test, cooperation_test, condition_test, 
                    transparency_test, environment_test, labor_test, periphery_test))

names = rownames(h_tests)
h_tests = sapply(h_tests, as.character)
h_tests = h_tests[, c(1:3)]
rownames(h_tests) <- names

pdf("data12_gridExtra.pdf")       # Export PDF
grid.table(h_tests)
dev.off()

# typical country example
# environment provisions impact
trade_environment_0 = 1.075*log(400000000000/5000) + 0.978*log(400000000000) +
  0.038*log(50000000) + 0.306*log(50000000) + -0.001*5 + 0.008*5 + -0.004*1 + mean(fixef(environment))

trade_environment_1 = 1.075*log(400000000000/5000) + 0.978*log(400000000000) +
  0.038*log(50000000) + 0.306*log(50000000) + -0.001*5 + 0.008*5 + -0.004*2 + mean(fixef(environment))

(exp(trade_environment_0) - exp(trade_environment_1)) * 1000

# labor market
trade_labor_0 = 1.075*log(400000000000/5000) + 0.978*log(400000000000) +
  0.042*log(50000000) + 0.309*log(50000000) + -0.001*5 + 0.008*5 + -0.014*1 + mean(fixef(labor))

trade_labor_1 = 1.075*log(400000000000/5000) + 0.978*log(400000000000) +
  0.042*log(50000000) + 0.309*log(50000000) + -0.001*5 + 0.008*5 + -0.014*2 + mean(fixef(labor))

(exp(trade_labor_0) - exp(trade_labor_1)) * 1000

# periphery
trade_periphery_0 = 1.074*log(400000000000/5000) + 0.977*log(400000000000) +
  0.046*log(50000000) + 0.313*log(50000000) + -0.001*5 + 0.008*5 + -0.0001*1 + mean(fixef(periphery))

trade_periphery_1 = 1.074*log(400000000000/5000) + 0.977*log(400000000000) +
  0.046*log(50000000) + 0.313*log(50000000) + -0.001*5 + 0.008*5 + -0.0001*2 + mean(fixef(periphery))

(exp(trade_periphery_0) - exp(trade_periphery_1)) * 1000