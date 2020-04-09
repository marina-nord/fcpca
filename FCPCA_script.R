# This is the FCPCA script file
#
# retrieving data
fcpca_data <- read.csv(file="fcpca_data.csv", header=TRUE, sep=",")
#
# checking class bias
table(fcpca_data$arch_exit_any)    # any leadership changes
table(fcpca_data$arch_exit_reg)    # regular leadership changes
table(fcpca_data$arch_exit_irreg)  # irregular leadership changes
table(fcpca_data$regime_change)    # regime changes
table(fcpca_data$democratization)  # autocracy-to-democracy transitions
table(fcpca_data$aa_transition)    # autocracy-to-autocracy transitions
#
# descriptive statistics
#
library(psych)
rename_fcpca_data <- fcpca_data
names(rename_fcpca_data) <- c("ccode", "cname", "year", "ccodealp", "cname_year", "ccodealp_year", "ccodecow", "p_durable", "p_polity2", "fh_polity2", "arch_exit_any", "arch_exit_reg", "arch_exit_irreg", "regime_change", "prevrc", "democratization", "aa_transition", "duration", "banking", "banking_lag", "banking_start", "currency", "currency_lag", "currency_start", "default", "default_lag", "default_start", "default_ext", "default_ext_lag", "default_dom", "default_dom_lag", "anyfincrisis", "anyfincrisis_lag", "anyfincrisis_start", "bc_twin", "bc_twin_lag", "cb_twin", "cb_twin_lag", "twin_bc", "twin_bc_lag", "bd_twin", "bd_twin_lag", "db_twin", "db_twin_lag", "twin_bd", "twin_bd_lag", "twin_bd_b", "twin_bd_d", "twin_bd_b_lag", "twin_bd_d_lag", "cd_twin", "cd_twin_lag", "dc_twin", "dc_twin_lag", "twin_cd", "twin_cd_lag", "twin_any", "twin_any_lag", "triple", "triple_lag", "wr_regtype", "military", "personal", "party", "wr_monarchy", "gwf_regimetype", "gwf_party", "gwf_personal", "gwf_military", "gwf_monarch", "gdppcgr", "gdppcgr_lag", "gdppcgr2", "pwt_gdppcgr", "chga_demo", "chga_hinst", "mad_gdppc", "mad_gdppcgr", "vdem_corr", "vdem_index", "vdem_index_lag", "wdi_gdpgr", "wdi_gdppcgr", "resources", "resources_lag", "latam", "mideast", "africa", "asia", "southeastasia", "europe", "exussr")
des = describe(rename_fcpca_data, fast=TRUE, omit=TRUE)
des <- des[-c(1, 2, 3, 4, 5, 6, 7, 8, 10, 20, 21, 23, 24, 26, 27, 29, 31, 33, 34, 36, 38, 40, 42, 44, 46, 47, 48, 49, 50, 52, 54, 56, 58, 60, 61, 65, 66, 67, 68, 69, 70, 72, 73, 74, 75, 76, 77, 78, 79, 81, 82, 83, 85), -c(1, 7, 8)]
# print(des, digits=3)
#
library(stargazer)
#
# creating table 6
#
stargazer(des, title="Table 6. Descriptive statistics", summary=FALSE, type="html", out="paper1_table6.doc")
#
# pooled probit models 
#
# 52, 54, 56
# 36, 38, 40
turnover_list <- c(11, 12, 13, 14, 16, 17)
crisis_list <- c(33, 20, 23, 26, 58, 40, 46, 56, 60)
#
# creating empty lists for final results
{
  PC_name=c("LC_","RLC_","ILC_","RC_","D_","AA_")
  FC_name=c("a", "b","c","d","twa", "twbc","twbd","twcd","tr")
  for (t in 1:6) {
    for (y in 1:9) {
      xxx=paste0(PC_name[t],FC_name[y])
      assign(xxx,vector("list",5))
    }
  }
}
# pooled probit models for leadership change (any, regular, irregular): with and without gdppcgr
#
for (t in 1:3) {
  for (y in 1:9) {
    z=paste0("model_",t,y)
    q=getElement(fcpca_data, names(fcpca_data[turnover_list[t]]))
    i=getElement(fcpca_data, names(fcpca_data[crisis_list[y]]))
    glm_res <- glm (q ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + i, family=binomial(link="probit"), data=fcpca_data)
    names(glm_res$coefficients)[10]=names(fcpca_data[crisis_list[y]])
    assign(z, glm_res)
    w=paste0("model_",t+3,y)
    glm_res2 <- glm (q ~ vdem_index + resources + duration + prevrc + party + military + personal + i, family=binomial(link="probit"), data=fcpca_data)
    names(glm_res2$coefficients)[9]=names(fcpca_data[crisis_list[y]])
    assign(w, glm_res2)
  }
}
#
# saving results
{
  for (t in 1:3) {
    for (y in 1:9) {
      xxx=paste0(PC_name[t],FC_name[y])
      xx=get(xxx)
      m=paste0('model_',t,y)
      mm=get(m)
      xx[[1]]=c(summary(mm)$coefficients[10], summary(mm)$coefficients[10,4], summary(mm)$coefficients[10,2])
      assign(xxx,xx)
      m=paste0('model_',t+3,y)
      mm=get(m)
      xx[[2]]=c(summary(mm)$coefficients[9], summary(mm)$coefficients[9,4], summary(mm)$coefficients[9,2])
      assign(xxx,xx)
    }
  }
}
#
# creating tables 1, 7a, 7b, 9a, 10a, 11a 
#
library(texreg)
extract.pglm <- function (model, include.nobs = TRUE, include.loglik = TRUE, ...) {
  s <- summary(model, ...)
  coefficient.names <- rownames(s$estimate)
  coefficients <- s$estimate[, 1]
  standard.errors <- s$estimate[, 2]
  significance <- s$estimate[, 4]
  loglik.value <- s$loglik
  n <- nrow(model$model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    gof <- c(gof, loglik.value)
    gof.names <- c(gof.names, "Log-Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                     se = standard.errors, pvalues = significance, gof.names = gof.names, 
                     gof = gof, gof.decimal = gof.decimal)
  return(tr)
}
setMethod("extract", signature = className("maxLik", "maxLik"), definition = extract.pglm)
#
htmlreg(list(model_11, model_12, model_13, model_14, model_15, model_16, model_17, model_18, model_19), file="paper1_table1.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 1. Leadership change and financial crises</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_21, model_22, model_23, model_24, model_25, model_26, model_27, model_28, model_29), file="paper1_table7a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 7a. Regular leadership change and financial crises</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_31, model_32, model_33, model_34, model_35, model_36, model_37, model_38, model_39), file="paper1_table7b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 7b. Irregular leadership change and financial crises</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_41, model_42, model_43, model_44, model_45, model_46, model_47, model_48, model_49), file="paper1_table9a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 9a. Leadership change and financial crises (without gdppcgr)</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_51, model_52, model_53, model_54, model_55, model_56, model_57, model_58, model_59), file="paper1_table10a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 10a. Regular leadership change and financial crises (without gdppcgr)</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_61, model_62, model_63, model_64, model_65, model_66, model_67, model_68, model_69), file="paper1_table11a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 11a. Irregular leadership change and financial crises (without gdppcgr)</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
#
# pooled probit models for any regime change: with and without gdppcgr
#
for (y in 1:9) {
    t=4
    z=paste0("model_",t-3,y)
    q=getElement(fcpca_data, names(fcpca_data[turnover_list[t]]))
    i=getElement(fcpca_data, names(fcpca_data[crisis_list[y]]))
    glm_res <- glm (q ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + i, family=binomial(link="probit"), data=fcpca_data)
    names(glm_res$coefficients)[10]=names(fcpca_data[crisis_list[y]])
    assign(z, glm_res)
    w=paste0("model_",t-2,y)
    glm_res2 <- glm (q ~ vdem_index + resources + duration + prevrc + party + military + personal + i, family=binomial(link="probit"), data=fcpca_data)
    names(glm_res2$coefficients)[9]=names(fcpca_data[crisis_list[y]])
    assign(w, glm_res2)
}
#
# saving results
{
  for (y in 1:9) {
    t=4
    xxx=paste0(PC_name[t],FC_name[y])
    xx=get(xxx)
    m=paste0('model_',t-3,y)
    mm=get(m)
    xx[[1]]=c(summary(mm)$coefficients[10], summary(mm)$coefficients[10,4], summary(mm)$coefficients[10,2])
    assign(xxx,xx)
    m=paste0('model_',t-2,y)
    mm=get(m)
    xx[[2]]=c(summary(mm)$coefficients[9], summary(mm)$coefficients[9,4], summary(mm)$coefficients[9,2])
    assign(xxx,xx)
  }
}
#
# creating tables 3 and 12a
#
htmlreg(list(model_11, model_12, model_13, model_14, model_15, model_16, model_17, model_18, model_19), file="paper1_table3.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 3. Regime change and financial crises</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_21, model_22, model_23, model_24, model_25, model_26, model_27, model_28, model_29), file="paper1_table12a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 12a. Regime change and financial crises (without gdppcgr)</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
#
# pooled probit models for autocracy-to-autocracy and autocracy-to-democracy transitions: with and without gdppcgr
#
for (t in 5:6) {
  for (y in 1:9) {
    z=paste0("model_",t-4,y)
    q=getElement(fcpca_data, names(fcpca_data[turnover_list[t]]))
    i=getElement(fcpca_data, names(fcpca_data[crisis_list[y]]))
    glm_res <- glm (q ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + i, family=binomial(link="probit"), data=fcpca_data)
    names(glm_res$coefficients)[10]=names(fcpca_data[crisis_list[y]])
    assign(z, glm_res)
    w=paste0("model_",t-2,y)
    glm_res2 <- glm (q ~ vdem_index + resources + duration + prevrc + party + military + personal + i, family=binomial(link="probit"), data=fcpca_data)
    names(glm_res2$coefficients)[9]=names(fcpca_data[crisis_list[y]])
    assign(w, glm_res2)
  }
}
#
# saving results
{
  for (t in 5:6) {
    for (y in 1:9) {
      xxx=paste0(PC_name[t],FC_name[y])
      xx=get(xxx)
      m=paste0('model_',t-4,y)
      mm=get(m)
      xx[[1]]=c(summary(mm)$coefficients[10], summary(mm)$coefficients[10,4], summary(mm)$coefficients[10,2])
      assign(xxx,xx)
      m=paste0('model_',t-2,y)
      mm=get(m)
      xx[[2]]=c(summary(mm)$coefficients[9], summary(mm)$coefficients[9,4], summary(mm)$coefficients[9,2])
      assign(xxx,xx)
    }
  }
}
#
# creating tables 8a, 8b, 13a, 14a
#
htmlreg(list(model_11, model_12, model_13, model_14, model_15, model_16, model_17, model_18, model_19), file="paper1_table8a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 8a. Democratization and financial crises</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_21, model_22, model_23, model_24, model_25, model_26, model_27, model_28, model_29), file="paper1_table8b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 8b. Autocracy-to-autocracy transitions and financial crises</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_31, model_32, model_33, model_34, model_35, model_36, model_37, model_38, model_39), file="paper1_table13a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 13a. Democratization and financial crises (without gdppcgr)</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_41, model_42, model_43, model_44, model_45, model_46, model_47, model_48, model_49), file="paper1_table14a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 14a. Autocracy-to-autocracy transitions and financial crises (without gdppcgr)</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
#
# pooled probit models for democratization and domestic vs. external default
#
model_14 <- glm (democratization ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + default_dom, family=binomial(link="probit"), data=fcpca_data)
model_24 <- glm (democratization ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + default_ext, family=binomial(link="probit"), data=fcpca_data)
#
# creating figure 3 for democratization and domestic vs. external default
#
library("jtools")
library("ggplot2")
fig3 = plot_summs(model_14, model_24, scale=TRUE,  legend.title="Pooled probit models", model.names=c('domestic default', 'external default')) +
labs(title="Figure 3. Democratization and domestic vs. external default")
fig3
#
# pooled probit models for transitions to democracy/autocracy and twin banking and debt crises
# twin_bd_b (49)
#
model_17 <- glm (democratization ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + bd_twin, family=binomial(link="probit"), data=fcpca_data)
model_27 <- glm (aa_transition ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + bd_twin, family=binomial(link="probit"), data=fcpca_data)
#
# pooled probit models for transitions to democracy/autocracy and twin debt and banking crises
# twin_bd_d (50)
#
model_37 <- glm (democratization ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + db_twin, family=binomial(link="probit"), data=fcpca_data)
model_47 <- glm (aa_transition ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + db_twin, family=binomial(link="probit"), data=fcpca_data)
#
# creating figure 2 
#
library("jtools")
library("ggplot2")
fig2 = plot_summs(model_17, model_27, model_37, model_47, scale=TRUE, legend.title="Models", model.names=c('to democracy | banking/debt', 'to autocracy | banking/debt', 'to democracy | debt/banking', 'to autocracy | debt/banking'), colors=c('#5dbcd2', '#ffaa6b', '#5dbcd2', '#ffaa6b'), ci_level = .95) +
  labs(title="Figure 2. Transitions to democracy/autocracy and twin banking/debt crises") +
  theme(plot.title = element_text(size=12, hjust=0))
fig2
#
#
#
#
# Additional models for robustness checks
#
# random effects models for leadership change (any, regular, irregular), regime change (any, autocracy-to-autocracy, democratization)
#
library("stats")
library("pglm")
#
for (t in 1:6) {
  for (y in 1:9) {
    z=paste0("model_",t,y)
    print(z)
    mydata <- fcpca_data
    mydata$q <- getElement(fcpca_data, names(fcpca_data[turnover_list[t]]))
    print(names(fcpca_data[turnover_list[t]]))
    mydata$i <- getElement(fcpca_data, names(fcpca_data[crisis_list[y]]))
    print(names(fcpca_data[crisis_list[y]]))
    glm_res <- pglm (q ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + i, family=binomial(link="probit"), model="random", index=c("cname", "year"), data=mydata)
    names(glm_res$estimate)[10]=names(fcpca_data[crisis_list[y]])
    assign(z, glm_res)
  }
}
#
# saving results
#
{
  for (t in 1:6) {
    for (y in 1:9) {
      xxx=paste0(PC_name[t],FC_name[y])
      xx=get(xxx)
      m=paste0('model_',t,y)
      mm=get(m)
      xx[[3]]=c(summary(mm)$estimate[10], summary(mm)$estimate[10,4], summary(mm)$estimate[10,2])
      assign(xxx,xx)
    }
  }
}
#
# creating tables 9b, 10b, 11b, 12b, 13b, 14b
#
htmlreg(list(model_11, model_12, model_13, model_14, model_15, model_16, model_17, model_18, model_19), file="paper1_table9b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 9b. Leadership change and financial crises: Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18,19), single.row=TRUE)
htmlreg(list(model_21, model_22, model_23, model_24, model_25, model_26, model_27, model_28, model_29), file="paper1_table10b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 10b. Regular leadership change and financial crises: Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18,19), single.row=TRUE)
htmlreg(list(model_31, model_32, model_33, model_34, model_35, model_36, model_37, model_38, model_39), file="paper1_table11b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 11b. Irregular leadership change and financial crises: Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18,19), single.row=TRUE)
htmlreg(list(model_41, model_42, model_43, model_44, model_45, model_46, model_47, model_48, model_49), file="paper1_table12b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 12b. Regime change and financial crises: Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18,19), single.row=TRUE)
htmlreg(list(model_51, model_52, model_53, model_54, model_55, model_56, model_57, model_58, model_59), file="paper1_table13b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 13b. Democratization and financial crises: Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18,19), single.row=TRUE)
htmlreg(list(model_61, model_62, model_63, model_64, model_65, model_66, model_67, model_68, model_69), file="paper1_table14b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 14b. Autocracy-to-autocracy transitions and financial crises: Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18,19), single.row=TRUE)
#
# random effects models with regional dummies for leadership change (any, regular, irregular), regime change (any, autocracy-to-autocracy, democratization)
#
for (t in 1:6) {
  for (y in 1:9) {
    z=paste0("model_",t,y)
    print(z)
    mydata <- fcpca_data
    mydata$q <- getElement(fcpca_data, names(fcpca_data[turnover_list[t]]))
    print(names(fcpca_data[turnover_list[t]]))
    mydata$i <- getElement(fcpca_data, names(fcpca_data[crisis_list[y]]))
    print(names(fcpca_data[crisis_list[y]]))
    glm_res <- pglm (q ~ gdppcgr + vdem_index + resources + duration + prevrc + party + military + personal + southeastasia + mideast + africa + latam + exussr + i, family=binomial(link="probit"), model="random", index=c("cname", "year"), data=mydata)
    names(glm_res$estimate)[15]=names(fcpca_data[crisis_list[y]])
    assign(z, glm_res)
  }
}
#
# saving results
#
{
  for (t in 1:6) {
    for (y in 1:9) {
      xxx=paste0(PC_name[t],FC_name[y])
      xx=get(xxx)
      m=paste0('model_',t,y)
      mm=get(m)
      xx[[4]]=c(summary(mm)$estimate[15], summary(mm)$estimate[15,4], summary(mm)$estimate[15,2])
      assign(xxx,xx)
    }
  }
}
#
# creating tables for random effects models with regional dummies
#
htmlreg(list(model_11, model_12, model_13, model_14, model_15, model_16, model_17, model_18, model_19), file="paper1_table9c.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 9c. Leadership change and financial crises: Probit, RE with regional dummies</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17,18,19,20,21,22,23,24), single.row=TRUE)
htmlreg(list(model_21, model_22, model_23, model_24, model_25, model_26, model_27, model_28, model_29), file="paper1_table10c.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 10c. Regular leadership change and financial crises: Probit, RE with regional dummies</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17,18,19,20,21,22,23,24), single.row=TRUE)
htmlreg(list(model_31, model_32, model_33, model_34, model_35, model_36, model_37, model_38, model_39), file="paper1_table11c.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 11c. Irregular leadership change and financial crises: Probit, RE with regional dummies</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17,18,19,20,21,22,23,24), single.row=TRUE)
htmlreg(list(model_41, model_42, model_43, model_44, model_45, model_46, model_47, model_48, model_49), file="paper1_table12c.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 12c. Regime change and financial crises: Probit, RE with regional dummies</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17,18,19,20,21,22,23,24), single.row=TRUE)
htmlreg(list(model_51, model_52, model_53, model_54, model_55, model_56, model_57, model_58, model_59), file="paper1_table13c.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 13c. Democratization and financial crises: Probit, RE with regional dummies</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17,18,19,20,21,22,23,24), single.row=TRUE)
htmlreg(list(model_61, model_62, model_63, model_64, model_65, model_66, model_67, model_68, model_69), file="paper1_table14c.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 14c. Autocracy-to-autocracy transitions and financial crises: Probit, RE with regional dummies</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15,17,18,19,20,21,22,23,24), single.row=TRUE)
#
#
# fixed effects logit models
#
library("survival")
#
# models for irregular leadership change (any, regular, irregular), regime change (any, autocracy-to-democracy, autocracy-to-autocracy)
#
for (t in 1:6) {
  for (y in 1:9) {
    z=paste0("model_",t,y)
    q=getElement(fcpca_data, names(fcpca_data[turnover_list[t]]))
    i=getElement(fcpca_data, names(fcpca_data[crisis_list[y]]))
    c_res <- clogit (q ~ gdppcgr + resources + duration + prevrc + party + military + personal + i + strata(ccode), data=fcpca_data, method=c("efron"))
    names(c_res$coefficients)[8]=names(fcpca_data[crisis_list[y]])
    assign(z, c_res)
  }
}
#
# saving results
#
{
  for (t in 1:6) {
    for (y in 1:9) {
      xxx=paste0(PC_name[t],FC_name[y])
      xx=get(xxx)
      m=paste0('model_',t,y)
      mm=get(m)
      xx[[5]]=c(summary(mm)$coefficients[8], summary(mm)$coefficients[8,5], summary(mm)$coefficients[8,3])
      assign(xxx,xx)
    }
  }
}
#
# creating tables for logit, FE models for leadership change (any, regular, irregular) and regime change (any, autocracy-to-democracy, autocracy-to-autocracy)
#
htmlreg(list(model_11, model_12, model_13, model_14, model_15, model_16, model_17, model_18, model_19), file="paper1_table9d.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 9d. Leadership change and financial crises: Logit, FE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_21, model_22, model_23, model_24, model_25, model_26, model_27, model_28, model_29), file="paper1_table10d.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 10d. Regular leadership change and financial crises: Logit, FE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_31, model_32, model_33, model_34, model_35, model_36, model_37, model_38, model_39), file="paper1_table11d.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 11d. Irregular leadership change and financial crises: Logit, FE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_41, model_42, model_43, model_44, model_45, model_46, model_47, model_48, model_49), file="paper1_table12d.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 12d. Regime change and financial crises: Logit, FE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_51, model_52, model_53, model_54, model_55, model_56, model_57, model_58, model_59), file="paper1_table13d.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 13d. Democratization and financial crises: Logit, FE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
htmlreg(list(model_61, model_62, model_63, model_64, model_65, model_66, model_67, model_68, model_69), file="paper1_table14d.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"), caption.above=TRUE, caption="<b>Table 14d. Autocracy-to-autocracy transitions and financial crises: Logit, FE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Clustered standard errors in brackets. <br> Significance levels: *** p<0.01, ** p<0.05, * p<0.1", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
#
# merging all results
#
model_types <- c('Pooled Probit', 'Pooled Probit (GDPPCGR dropped)', 'Probit, RE', 'Probit, RE (with regional dummies)','Logit, FE')
{
  PC_name=c("LC_","RLC_","ILC_","RC_","D_","AA_")
  PCname=c("LC","RLC","ILC","RC","D","AA")
  FC_name=c("a","b","c","d","twa","twbc","twbd","twcd","tr")
  for (t in 1:6) {
    for (y in 1:9) {
      xxx=paste0(PC_name[t],FC_name[y])
      xx=get(xxx)
      q=c()
      for (f in 1:5) {
        if (xx[[f]][2]<0.01) {
          zz=paste0(format(round(xx[[f]][1],3),nsmall=3),'***')
          zzz=paste0('(',format(round(xx[[f]][3],3),nsmall=3),')')
          z=paste(zz, zzz)
          q=c(q,z)
        }
        else if (xx[[f]][2]<0.05) {
          zz=paste0(format(round(xx[[f]][1],3),nsmall=3),'**')
          zzz=paste0('(',format(round(xx[[f]][3],3),nsmall=3),')')
          z=paste(zz, zzz)
          q=c(q,z)
        }
        else if (xx[[f]][2]<0.10) {
          zz=paste0(format(round(xx[[f]][1],3),nsmall=3),'*')
          zzz=paste0('(',format(round(xx[[f]][3],3),nsmall=3),')')
          z=paste(zz, zzz)
          q=c(q,z)
        }
        else {
          zz=paste0(format(round(xx[[f]][1],3),nsmall=3))
          zzz=paste0('(',format(round(xx[[f]][3],3),nsmall=3),')')
          z=paste(zz, zzz)
          q=c(q,z)
        }
      }
      kk=paste0(PCname[t],FC_name[y])
      assign(kk,q)
    }
  }
}
LC_res <- data.frame(model_types, LCa, LCb, LCc, LCd, LCtwa, LCtwbc, LCtwbd, LCtwcd, LCtr)
RLC_res <- data.frame(model_types, RLCa, RLCb, RLCc, RLCd, RLCtwa, RLCtwbc, RLCtwbd, RLCtwcd, RLCtr)
ILC_res <- data.frame(model_types, ILCa, ILCb, ILCc, ILCd, ILCtwa, ILCtwbc, ILCtwbd, ILCtwcd, ILCtr)
RC_res <- data.frame(model_types, RCa, RCb, RCc, RCd, RCtwa, RCtwbc, RCtwbd, RCtwcd, RCtr)
D_res <- data.frame(model_types, Da, Db, Dc, Dd, Dtwa, Dtwbc, Dtwbd, Dtwcd, Dtr)
AA_res <- data.frame(model_types, AAa, AAb, AAc, AAd, AAtwa, AAtwbc, AAtwbd, AAtwcd, AAtr)
#
# assigning new column names names
{
  new_names <- c('model', 'any', 'banking', 'currency', 'default', 'twin_any', 'twin_bc', 'twin_bd', 'twin_cd', 'triple')
  names(LC_res)=new_names
  names(RLC_res)=new_names
  names(ILC_res)=new_names
  names(RC_res)=new_names
  names(D_res)=new_names
  names(AA_res)=new_names
}
#
stargazer(LC_res, title="Table 2a. Leadership change: estimated coefficients of the independent variables", rownames=FALSE, summary=FALSE, type="html", out="paper1_table2a.doc")
stargazer(RLC_res, title="Table 2b. Regular leadership change: estimated coefficients of the independent variables", rownames=FALSE, summary=FALSE, type="html", out="paper1_table2b.doc")
stargazer(ILC_res, title="Table 2c. Irregular leadership change: estimated coefficients of the independent variables", rownames=FALSE, summary=FALSE, type="html", out="paper1_table2c.doc")
stargazer(RC_res, title="Table 4. Regime change: estimated coefficients of the independent variables", rownames=FALSE, summary=FALSE, type="html", out="paper1_table4.doc")
stargazer(D_res, title="Table 5a. Autocracy-to-democracy: estimated coefficients of the independent variables", rownames=FALSE, summary=FALSE, type="html", out="paper1_table5a.doc")
stargazer(AA_res, title="Table 5b. Autocracy-to-autocracy: estimated coefficients of the independent variables", rownames=FALSE, summary=FALSE, type="html", out="paper1_table5b.doc")
#