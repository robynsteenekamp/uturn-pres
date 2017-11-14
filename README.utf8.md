---
title: "Recommendations and Analytics to Uturn Ministries"
author: "Robyn Steenekamp"
date: "`r Sys.Date()`"
output: github_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE,warning = FALSE, error = FALSE, message = FALSE, cache = TRUE)

```

# Introduction

# Research questions

1. **Duration sensitivity: ** how long a particular client visits the service centre before transitioning from needs-based services to volition-based or rehabilitative services. 
2. **Frequency sensitivity: ** how many times a client is seen before they transistion from needs-based services to volition-based or rehabilitative services.
3. **Effect of age: **Is there is difference in long-term outcomes for older clients versus younger clients?
4. **Effect of gender: **Is there is difference in long-term outcomes for males and females.
5. Ultimately can we discern causality of long term success from the data?


# Data

After cleaning data: `r dim(instance_full)[[1]]` activity entries from `r dim(client_full)[[1]]` clients. 

NOTE:

1. There are `r dim(anti_client_full1)[[1]]` clients in the database with no record of attending U-turn activities. 

2. There are `r dim(anti_inst)[[1]]` activity entries from `r dim(anti_client_full)[[1]]` clients who are not in the database - suggested that this could be from old U-turn clients who are now staff members and therefore do not have a `client_id`.

## Clients
<!-- compare client_control with client ito missing data and demographics-->
Because of such a high instance of once-off clients we restrict the client space to include only those who have records of two or more activities. The remaining "once-off" clients are used as controls.

```{r update_ci, eval = TRUE}

client_info_full <- client_info_full %>% dplyr::mutate(TREAT=ifelse(CLIENT_ID%in%client_names,1,ifelse(CLIENT_ID%in%client_control_names,0,NA))

```


Hierarchical clustering (mcquitty method) to arrange rows according to missingness.


```{r dat_vis1, eval = TRUE}

vis_miss(client, cluster=TRUE, sort_miss =TRUE)
client_sub1 <- client %>%dplyr::select(CLIENT_ID, GENDER, RACE, AGE)
client_sub1 <- na.omit(client_sub1)
client_names_sub1 <- client_sub1$CLIENT_ID
instance_sub1 <- instance %>% dplyr::filter(CLIENT_ID %in% client_names_sub1)

```

`r length(client_names_sub1)` U-turn clients remain with complete demographic information. These clients collectively had `r dim(instance_sub1)[[1]]` U-turn records.

Took the following into consideration:

1. Activity type
2. Duration of activity
3. Frequency of activity


### C1 - Activity type

Let C = {All U-turn clients}. The following subsets of C exist:

* S = {U-turn clients who have accessed *at least 2* service products}
* P = {U-turn clients who have attended either Addiction rehab or Life Change programs}
* FA = {U-turn clients who have Focus Area reports}


```{r venn, eval = TRUE}
# Venn diagram S FA P
venn.plot0 <- draw.triple.venn(area1 = nS,area2 = nFA,area3 = nP,n12 = nS.FA,n23=nFA.P,n13=nS.P,n123=nS.FA.P,category = c("S", "FA","P"),fill = c("blue", "red", "green"),rotation=3,lty = "blank",cex = 2,
cat.cex = 2,cat.col = c("blue", "red", "green"),euler.d = TRUE,scaled=FALSE)
grid.newpage()

Q1<-client_info%>%filter(C1==1)
Q2<-client_info%>%filter(C1==2)
Q3<-client_info%>%filter(C1==3)
Q4<-client_info%>%filter(C1==4)

rm(list=c("nFA","nFA.P","nP","nS","nS.FA","nS.FA.P","nS.P","P","S.FA","S.FA.P","S.P","FA","FA.P"))

```

Clients are partitioned into unequally sized mutually exclusive subsets.

* Q1 = S and not(P) and not(FA)
* Q2 = S and not(P) and FA
* Q3 = S and P and not(FA)
* Q4 = S and P and FA

```{r part, eval = TRUE}
prop.table(table(client_info$C1))
```



### C2 - Service activity duration



```{r act_period, eval = FALSE}

x <- list(title = "Total active period (in months)")
y <- list(title = "Frequency")

temp <- client_info 
p1 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~temp$END_MONTH) %>%
  layout(xaxis=x,yaxis=y)
p1

rm(list=c("temp","x","y","p1"))

Q1<-client_info%>%filter(C1==1)
Q2<-client_info%>%filter(C1==2)
Q3<-client_info%>%filter(C1==3)
Q4<-client_info%>%filter(C1==4)

AP_1 <- client_info %>% filter(CLIENT_ID %in% Q1$CLIENT_ID)
AP_2 <- client_info %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)
AP_3 <- client_info %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)
AP_4 <- client_info %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)

p <- plot_ly(y=~AP_1$END_MONTH, alpha=0.5,type="box",name="Q1")%>%
  add_trace(y =~AP_2$END_MONTH,name="Q2")%>%
  add_trace(y =~AP_3$END_MONTH,name="Q3")%>%
  add_trace(y =~AP_4$END_MONTH,name="Q4")%>%
  layout(xaxis=list(title="Partition"),yaxis=list(title="Active period (in months)"))

p

client_info <- client_info %>% dplyr::mutate(C1=ifelse(C1==1,ifelse(END_MONTH>=64,1,0),C1))

Q0<-client_info%>%filter(C1==0)
Q1<-client_info%>%filter(C1==1)
Q2<-client_info%>%filter(C1==2)
Q3<-client_info%>%filter(C1==3)
Q4<-client_info%>%filter(C1==4)

AP_0 <- client_info %>% filter(CLIENT_ID %in% Q0$CLIENT_ID)
AP_1 <- client_info %>% filter(CLIENT_ID %in% Q1$CLIENT_ID)
AP_2 <- client_info %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)
AP_3 <- client_info %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)
AP_4 <- client_info %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)

p <- plot_ly(y=~AP_0$END_MONTH, alpha=0.5,type="box",name="Q0")%>%
  add_trace(y =~AP_1$END_MONTH,name="Q1")%>%
  add_trace(y =~AP_2$END_MONTH,name="Q2")%>%
  add_trace(y =~AP_3$END_MONTH,name="Q3")%>%
  add_trace(y =~AP_4$END_MONTH,name="Q4")%>%
  layout(xaxis=list(title="Partition"),yaxis=list(title="Active period (in months)"))

p

```

### C3 - Service activity frequency


```{r serv_hist_20, eval = FALSE}

x <- list(title = "Number of services accessed")
y <- list(title = "Frequency")

temp <- cumn_df_full %>% filter(as.numeric(CUMN) <= 20)
p1 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~temp$CUMN) %>%
  layout(xaxis=x,yaxis=y)
p1

rm(list=c("temp","x","y","p1"))
```

```{r serv_hist, eval = FALSE}

x <- list(title = "Number of services accessed")
y <- list(title = "Frequency")

temp <- cumn_df
p1 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~temp$CUMN) %>%
  layout(xaxis=x,yaxis=y)
p1

rm(list=c("temp","x","y","p1"))
```

```{r cum_box, eval = FALSE}
C_0 <- cumn_df %>% filter(CLIENT_ID %in% Q0$CLIENT_ID) %>% select(CUMN)
C_1 <- cumn_df %>% filter(CLIENT_ID %in% Q1$CLIENT_ID) %>% select(CUMN)
C_2 <- cumn_df %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)%>% select(CUMN)
C_3 <- cumn_df %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)%>% select(CUMN)
C_4 <- cumn_df %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)%>% select(CUMN)

p <- plot_ly(y=~C_0$CUMN, alpha=0.5,type="box",name="Q0")%>%
  add_trace(y =~C_1$CUMN,name="Q1")%>%
  add_trace(y =~C_2$CUMN,name="Q2")%>%
  add_trace(y =~C_3$CUMN,name="Q3")%>%
  add_trace(y =~C_4$CUMN,name="Q4")%>%
  layout(xaxis=list(title="Partition"),yaxis=list(title="Total number of services accessed"))

p

rm(list=c("AP_1","AP_2","AP_3","AP_4"))
```


### Notes
- heat matrix for correlation of vars


## Services

A breakdown of service types is shown below. Life

### S1 - Service product motivation

Basic-needs, volition, rehabilitative
    
### S2 - Phase 

I, II, III, IV
    
### S3 - Status

Activities of Daily Living, Housing, Education, Employment, Motivation and Relationships, Community connectivity, Self-Efficacy, Legal, Medical, Mental Health, Substances, Financial

### S4 - Points of service

### Notes

<!-- <!-- Service data in wide / stacked form -->
<!-- ```{r eval = T, warning = F, error = F, message = F, cache = T} -->
<!-- knitr::kable(temp1[1:10,]) -->
<!-- ``` -->
<!-- ### Basic needs dataframe -->
<!-- ```{r eval = T, warning = F, error = F, message = F, cache = T} -->
<!-- knitr::kable(MOT1[1:10,]) -->
<!-- ``` -->
<!-- ### Time series - split by SP motivation -->
<!-- ```{r eval = T, echo = F, warning = F, error = F, message = F, cache = T} -->
<!-- samp_c_list <- list("0031300002KBs04AAD","003a000001SXg0ZAAT","0033000001AkZ6YAAV") -->
<!-- p_list = list() -->
<!-- i=1 -->
<!-- for (samp_c in samp_c_list) -->
<!-- { -->
<!--   m1 <- MOT1_C %>% filter(CLIENT_ID == samp_c) -->
<!--   M1 <- m1[,-1] -->
<!--   m2 <- MOT2_C %>% filter(CLIENT_ID == samp_c) -->
<!--   M2 <- m2[,-1] -->
<!--   m3 <- MOT3_C %>% filter(CLIENT_ID == samp_c) -->
<!--   M3 <- m3[,-1] -->
<!--   time<-1:98 -->
<!--   df<-data.frame(time, t(rbind(M1,M2,M3))) -->
<!--   t_end <- CLIENT_INFO %>% filter(CLIENT_ID == samp_c) -->
<!--   t_end <- ceiling(t_end$ACTIVE_PERIOD)+4 -->
<!--   p<- plot_ly(df, x=~time, y=~X1, name="Motivation 1",type="scatter",mode="lines+markers")%>% -->
<!--     add_trace(y=~X2,name="Motivation 2",mode="lines+markers")%>% -->
<!--     add_trace(y=~X3,name="Motivation 3",mode="lines+markers")%>% -->
<!--     layout(xaxis=list(range=c(0,t_end),title="Time (months since first contact)"),yaxis=list(title="Frequency")) -->
<!--   p_list[[i]]=p -->
<!--   i=i+1 -->
<!-- } -->


<!-- p_list[[1]]#;p_list[[2]];p_list[[3]] -->

<!-- rm(list=c("m1","m2","m3","M1","M2","M3","i","p_list")) -->

<!-- ``` -->

<!-- ### Averages -->
<!-- ```{r eval = T, echo = F, warning = F, error = F, message = F, cache = T} -->

<!-- ACT_df <- ACT[,1:2] -->
<!-- ACT_df[,2] <- apply(ACT[,2:ncol(ACT)],1,mean,na.rm=TRUE) -->
<!-- ACT_df <- data.frame("CLIENT_ID"=ACT_df[,1],"y"=ACT_df[,2]) -->


<!-- MOT1_df <- MOT1[,1:2] -->
<!-- MOT1_df[,2] <- apply(MOT1[,2:ncol(MOT1)],1,mean,na.rm=TRUE) -->
<!-- MOT1_df <- data.frame("CLIENT_ID"=MOT1_df[,1],"y"=MOT1_df[,2]) -->

<!-- MOT2_df <- MOT2[,1:2] -->
<!-- MOT2_df[,2] <- apply(MOT2[,2:ncol(MOT2)],1,mean,na.rm=TRUE) -->
<!-- MOT2_df <- data.frame("CLIENT_ID"=MOT2_df[,1],"y"=MOT2_df[,2]) -->

<!-- MOT3_df <- MOT3[,1:2] -->
<!-- MOT3_df[,2] <- apply(MOT3[,2:ncol(MOT3)],1,mean,na.rm=TRUE) -->
<!-- MOT3_df <- data.frame("CLIENT_ID"=MOT3_df[,1],"y"=MOT3_df[,2]) -->
<!-- ``` -->

<!-- ```{r eval = T, warning = F, error = F, message = F, cache = T} -->

<!-- AP_1 <- ACT_df %>% filter(CLIENT_ID %in% Q1$CLIENT_ID) %>% select(y) -->
<!-- AP_2 <- ACT_df %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)%>% select(y) -->
<!-- AP_3 <- ACT_df %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)%>% select(y) -->
<!-- AP_4 <- ACT_df %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)%>% select(y) -->

<!-- p <- plot_ly(y=~AP_1$y, alpha=0.5,type="box",name="Q1")%>% -->
<!--   add_trace(y =~AP_2$y,name="Q2")%>% -->
<!--   add_trace(y =~AP_3$y,name="Q3")%>% -->
<!--   add_trace(y =~AP_4$y,name="Q4")%>% -->
<!--   layout(xaxis=list(title="Partition"),yaxis=list(title="Average length of active periods (days)")) -->

<!-- p -->

<!-- rm(list=c("AP_1","AP_2","AP_3","AP_4")) -->
<!-- ``` -->

<!-- ```{r eval = T, warning = F, error = F, message = F, cache = T} -->

<!-- AP_1 <- ACT_df %>% filter(CLIENT_ID %in% Q1$CLIENT_ID) %>% select(y) -->
<!-- AP_2 <- ACT_df %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)%>% select(y) -->
<!-- AP_3 <- ACT_df %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)%>% select(y) -->
<!-- AP_4 <- ACT_df %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)%>% select(y) -->

<!-- p <- plot_ly(y=~AP_1$y, alpha=0.5,type="box",name="Q1")%>% -->
<!--   add_trace(y =~AP_2$y,name="Q2")%>% -->
<!--   add_trace(y =~AP_3$y,name="Q3")%>% -->
<!--   add_trace(y =~AP_4$y,name="Q4")%>% -->
<!--   layout(xaxis=list(title="Partition"),yaxis=list(title="Average length of active periods (days)")) -->

<!-- p -->

<!-- rm(list=c("AP_1","AP_2","AP_3","AP_4")) -->
<!-- ``` -->

<!-- ```{r eval = T, warning = F, error = F, message = F, cache = T} -->

<!-- AP_1 <- MOT1_df %>% filter(CLIENT_ID %in% Q1$CLIENT_ID) %>% select(y) -->
<!-- AP_2 <- MOT1_df %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)%>% select(y) -->
<!-- AP_3 <- MOT1_df %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)%>% select(y) -->
<!-- AP_4 <- MOT1_df %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)%>% select(y) -->

<!-- p <- plot_ly(y=~AP_1$y, alpha=0.5,type="box",name="Q1")%>% -->
<!--   add_trace(y =~AP_2$y,name="Q2")%>% -->
<!--   add_trace(y =~AP_3$y,name="Q3")%>% -->
<!--   add_trace(y =~AP_4$y,name="Q4")%>% -->
<!--   layout(xaxis=list(title="Partition"),yaxis=list(title="Total count of basic needs services accessed")) -->

<!-- p -->

<!-- rm(list=c("AP_1","AP_2","AP_3","AP_4")) -->
<!-- ``` -->

<!-- ```{r eval = T, warning = F, error = F, message = F, cache = T} -->

<!-- AP_1 <- MOT2_df %>% filter(CLIENT_ID %in% Q1$CLIENT_ID) %>% select(y) -->
<!-- AP_2 <- MOT2_df %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)%>% select(y) -->
<!-- AP_3 <- MOT2_df %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)%>% select(y) -->
<!-- AP_4 <- MOT2_df %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)%>% select(y) -->

<!-- p <- plot_ly(y=~AP_1$y, alpha=0.5,type="box",name="Q1")%>% -->
<!--   add_trace(y =~AP_2$y,name="Q2")%>% -->
<!--   add_trace(y =~AP_3$y,name="Q3")%>% -->
<!--   add_trace(y =~AP_4$y,name="Q4")%>% -->
<!--   layout(xaxis=list(title="Partition"),yaxis=list(title="Total MOT2 services accessed")) -->

<!-- p -->

<!-- rm(list=c("AP_1","AP_2","AP_3","AP_4")) -->
<!-- ``` -->

<!-- ```{r eval = T, warning = F, error = F, message = F, cache = T} -->
<!-- AP_1 <- MOT3_df %>% filter(CLIENT_ID %in% Q1$CLIENT_ID) %>% select(y) -->
<!-- AP_2 <- MOT3_df %>% filter(CLIENT_ID %in% Q2$CLIENT_ID)%>% select(y) -->
<!-- AP_3 <- MOT3_df %>% filter(CLIENT_ID %in% Q3$CLIENT_ID)%>% select(y) -->
<!-- AP_4 <- MOT3_df %>% filter(CLIENT_ID %in% Q4$CLIENT_ID)%>% select(y) -->

<!-- p <- plot_ly(y=~AP_1$y, alpha=0.5,type="box",name="Q1")%>% -->
<!--   add_trace(y =~AP_2$y,name="Q2")%>% -->
<!--   add_trace(y =~AP_3$y,name="Q3")%>% -->
<!--   add_trace(y =~AP_4$y,name="Q4")%>% -->
<!--   layout(xaxis=list(title="Partition"),yaxis=list(title="Total MOT3 services accessed")) -->

<!-- p -->

<!-- rm(list=c("AP_1","AP_2","AP_3","AP_4")) -->
<!-- ``` -->

<!-- -->


## Program

### P1 - Rehabilitation level

Using information from the **service** and **program** relations I created a hierarchical scoring system for monitoring clients' rehabilitation.

- **1: Follow up**
    + Service activity where PRODUCT_NAME == "P3 Follow-up Meeting"
    
- **2: Accommodation hunting**
    + Service activity where PRODUCT_NAME == "P3 Accommodation Hunting"

- **3: Graduated from Life Change**
    + Service activity where PRODUCT_NAME == "P2 Exit Interview" 
    + Program entry for Life Change where REASON_FOR_LEAVING == "Graduation Exit"

- **4: Entry to Life Change**
    + Program entry for Life Change where REASON_FOR_LEAVING == "Still enrolled in program"
    
- **5: Referred to Life Change**
    + Service activity where PRODUCT_NAME == "P1 Referral to Life Change"

- **6: Graduated from Addiction Rehab**
    + Service activity where PRODUCT_NAME == "P1 1/2 Addiction rehab graduation" 
    + Program entry for Addiction Rehab where REASON_FOR_LEAVING == "Graduation Exit"

- **7: Entry to Addiction Rehab**
    + Program entry for Addiction Rehab where REASON_FOR_LEAVING == "Still enrolled in program"

- **8: Referral to Addiction Rehab**
    + Service activity where PRODUCT_NAME == "P2 Referral to 1st Phase"
    + Service activity where PRODUCT_NAME == "P1 Referral to Addiction Rehab"

- **0: Relapse**
    + Program entry for Addiction Rehab / Life Change where REASON_FOR_LEAVING == "AWOL" / "Resigned before completion"

- **-1: No engagement in program activities**
    + Before client begins rehabilitative process
    
```{r eval = TRUE}

instance_copy <- instance%>%group_by(CLIENT_ID)%>%arrange(CLIENT_ID,START_DATE) %>% dplyr::mutate(Y3=ifelse(row_number()==1,-1,NA))%>% dplyr::mutate(Y3=ifelse(INST_TYPE==1,ifelse(PRODUCT_NAME=="P1 Referral to Addiction Rehab",8,ifelse(PRODUCT_NAME=="P1 Referral to Life Change",5,ifelse(PRODUCT_NAME=="P2 Referral to 1st Phase",8,ifelse(PRODUCT_NAME=="P1 1/2 Addiction rehab graduation",6,ifelse(PRODUCT_NAME=="P2 Exit Interview",3,ifelse(PRODUCT_NAME=="P3 Accommodation Hunting",2,ifelse(PRODUCT_NAME=="P3 Follow-up Meeting",1,Y3))))))),ifelse(INST_TYPE==2,ifelse(PROG_NAME=="Addiction Rehab",ifelse(REASON_FOR_LEAVING%in%c("AWOL","Resigned before completion"),0,ifelse(REASON_FOR_LEAVING=="Graduation Exit",6,7)),ifelse(REASON_FOR_LEAVING%in%c("AWOL","Resigned before completion"),0,ifelse(REASON_FOR_LEAVING=="Graduation Exit",3,4))),Y3)))%>% dplyr::mutate(Y3=na.locf(Y3))
```


```{r last_stat, eval = TRUE}
last_status<-instance_copy %>% group_by(CLIENT_ID) %>%top_n(1,START_DATE)

last_status_P<-instance_copy %>%filter(CLIENT_ID%in%P$CLIENT_ID)%>% group_by(CLIENT_ID) %>%top_n(1,START_DATE)

table(last_status$Y3)
table(last_status_P$Y3)
```




### P2 - Addicition or not
### Notes






## Correlation matrices:
```{r corr_mat1, eval = TRUE}

temp2 <- temp1 %>% group_by(CLIENT_ID) %>% dplyr::summarise(R1=sum(NEED),R2=sum(VOL),R3=sum(RE),N1=sum(S3_DUMMY_1),N2=sum(S3_DUMMY_2),N3=sum(S3_DUMMY_3),TW=max(T_WEEKS))

d1 <- left_join(temp2,Y1[,c(1,5,6,7,9,17)],by="CLIENT_ID")
corr <- cor(na.omit(d1[vapply(d1, is.numeric, logical(1))]))
p <- plot_ly(x = rownames(corr), y = colnames(corr), z = corr) %>%
  add_heatmap() %>%
  colorbar(limits = c(-1, 1))

corr <- cor(na.omit(temp1[vapply(temp1, is.numeric, logical(1))]))
p <- plot_ly(x = rownames(corr), y = colnames(corr), z = corr) %>%
  add_heatmap() %>%
  colorbar(limits = c(-1, 1))
p

```

## Multi-dimensional scaling
```{r eval = FALSE}

temp3 <- left_join(Y1[,c(1,17)],temp2,by="CLIENT_ID")
x1 <- na.omit(temp3[,-1])
y1 <- na.omit(temp3[,2])
rf1<-randomForest(x=x1,importance=TRUE)
MDSplot(rf1, y1)
MDSplot(rf1, y1,k=3)
```




# Chi squared tests


I have two groups of buyers, A and B, and I want to test whether the difference between the percentage of them who would buy a product is significant.

Group A: 271 out of 2520 bought the product (10.8%) and 2,249 didn't buy.
Group B: 1,073,839 out of 41,873,457 bought the product (2.6%) and 40,799,618 didn't buy.

* Cramer's V (measure of effect size)
    - risk difference: (271/2520) - (1073839/41873457) = 0.08189482
    - risk ratio: (271/2520) / (1073839/41873457) = 4.19342
    - odds ratio: (271/2249) / (1073839/40799618) = 4.578221


# Clustering

Create dummy variables for:


## Daisy

# Multidimensional scaling
Multidimensional scaling (MDS) is a means of visualizing the level of similarity  of individual cases of a dataset. It refers to a set of related ordination techniques used in information visualization, in particular to display the information contained in a distance matrix. 

1. Principal Coordinates Analysis / Classical  multidimensional scaling

2. Metric multidimensional scaling
    + "stress" function
    
There are several steps in conducting MDS research:

1. Formulating the problem – What variables do you want to compare? How many variables do you want to compare? What purpose is the study to be used for?

2. Obtaining input data – For example: Respondents are asked a series of questions. For each product pair, they are asked to rate similarity (usually on a 7-point Likert scale from very similar to very dissimilar). The first question could be for Coke/Pepsi for example, the next for Coke/Hires rootbeer, the next for Pepsi/Dr Pepper, the next for Dr Pepper/Hires rootbeer, etc. The number of questions is a function of the number of brands and can be calculated as {\displaystyle Q=N(N-1)/2} Q=N(N-1)/2 where Q is the number of questions and N is the number of brands. This approach is referred to as the “Perception data : direct approach”. There are two other approaches. There is the “Perception data : derived approach” in which products are decomposed into attributes that are rated on a semantic differential scale. The other is the “Preference data approach” in which respondents are asked their preference rather than similarity.

3. Running the MDS statistical program – Software for running the procedure is available in many statistical software packages. Often there is a choice between Metric MDS (which deals with interval or ratio level data), and Nonmetric MDS[5] (which deals with ordinal data).

4. Decide number of dimensions – The researcher must decide on the number of dimensions they want the computer to create. The more dimensions, the better the statistical fit, but the more difficult it is to interpret the results.

5. Mapping the results and defining the dimensions – The statistical program (or a related module) will map the results. The map will plot each product (usually in two-dimensional space). The proximity of products to each other indicate either how similar they are or how preferred they are, depending on which approach was used. How the dimensions of the embedding actually correspond to dimensions of system behavior, however, are not necessarily obvious. Here, a subjective judgment about the correspondence can be made (see perceptual mapping).

6. Test the results for reliability and validity – Compute R-squared to determine what proportion of variance of the scaled data can be accounted for by the MDS procedure. An R-square of 0.6 is considered the minimum acceptable level.[citation needed] An R-square of 0.8 is considered good for metric scaling and .9 is considered good for non-metric scaling. Other possible tests are Kruskal’s Stress, split data tests, data stability tests (i.e., eliminating one brand), and test-retest reliability.

7. Report the results comprehensively – Along with the mapping, at least distance measure (e.g., Sorenson index, Jaccard index) and reliability (e.g., stress value) should be given. It is also very advisable to give the algorithm (e.g., Kruskal, Mather), which is often defined by the program used (sometimes replacing the algorithm report), if you have given a start configuration or had a random choice, the number of runs, the assessment of dimensionality, the Monte Carlo method results, the number of iterations, the assessment of stability, and the proportional variance of each axis (r-square).


## Correspondece analysis

## Multiple correspondence analysis

## Multiple factor analysis

Multiple Factor Analysis for mixed data available in the FactoMineR R package (AFDM()). If your variables can be considered as structured subsets of descriptive attributes, then Multiple Factor Analysis (MFA()) is also an option.

## Principal component analysis

* Kernel PCA

## Isomaps

## Agnes

## SOM


# Support vector machine
* Unlabeled

# Hello



```{r eval = FALSE}



REF_ADD <- service %>% filter(PRODUCT_NAME=="P1 Referral to Addiction Rehab")%>%group_by(CLIENT_ID)%>%dplyr::arrange(CLIENT_ID,START_DATE)%>%dplyr::mutate(I=1,n_attempt=cumsum(I))
REF_LC <- service %>% filter(PRODUCT_NAME=="P1 Referral to Life Change")%>%group_by(CLIENT_ID)%>%distinct(SI_ID, .keep_all=TRUE)%>%arrange(CLIENT_ID,START_DATE)%>%dplyr::mutate(tot=n())

GRAD_ADD1 <-program %>% filter(REASON_FOR_LEAVING=="Graduation Exit" & PROG_NAME=="Addiction Rehab")%>%distinct(CLIENT_ID)
GRAD_LC1 <-program %>% filter(REASON_FOR_LEAVING=="Graduation Exit" & PROG_NAME=="Life change")%>%distinct(CLIENT_ID)

GRAD_ADD2 <-service %>% filter(PRODUCT_NAME=="P1 1/2 Addiction rehab graduation")%>%distinct(CLIENT_ID)
GRAD_LC2 <-service %>% filter(PRODUCT_NAME=="P2 Exit Interview")%>%distinct(CLIENT_ID)

#grad1 union grad2
GRAD_ADD <- dplyr::union(GRAD_ADD1,GRAD_ADD2)
GRAD_LC <- dplyr::union(GRAD_LC1,GRAD_LC2)

GRAD_ADD_I <- dplyr::intersect(GRAD_ADD1,GRAD_ADD2)
GRAD_LC_I <- dplyr::intersect(GRAD_LC1,GRAD_LC2)
which(GRAD_ADD1 %in% GRAD_ADD2)
all(GRAD_ADD2 %in% GRAD_ADD1)
all(GRAD_LC1 %in% GRAD_LC2)
all(GRAD_LC2 %in% GRAD_LC1)

all(GRAD_ADD1 %in% GRAD_ADD2)
all(GRAD_ADD2 %in% GRAD_ADD1)
all(GRAD_LC1 %in% GRAD_LC2)
all(GRAD_LC2 %in% GRAD_LC1)
```

### Data / outcome consistency

<!--Check data consistency-->
```{r eval = FALSE}

#Create more program variables

p1 <- program %>% group_by(CLIENT_ID, PROG_NAME)%>% distinct(PA_ID, .keep_all=TRUE)
cast1 <- dcast(p1, CLIENT_ID ~ PROG_NAME, length, value.var="PA_ID")
names(cast1) <- c("CLIENT_ID","ADDICTION_REHAB","LIFE_CHANGE")
x <- list(title = "Number of entries to program")
y <- list(title = "Frequency")
p <- plot_ly(alpha=0.6) %>%
    add_histogram(x=~cast1$ADDICTION_REHAB, name="Addiction rehab",fill = 'tozeroy',color=I("lightblue"))%>%
  add_histogram(x=~cast1$LIFE_CHANGE, name="Life change",fill = 'tozeroy',color=I("lightgreen"))%>%
  layout(barmode="overlay",xaxis=x,yaxis=y)
p
# p2<- left_join(p1, last_seen[,-3],by="CLIENT_ID")
# p3 <-p2 %>% group_by(PA_ID) %>% mutate(REF=ifelse(PROGRAM_ID=="a1o13000001x8opAAA",ifelse(CLIENT_ID %in% REF_LC$CLIENT_ID,1,0),ifelse(CLIENT_ID %in% REF_ADD$CLIENT_ID,1,0)),T1=ifelse(REF==0,NA,))

```

### More stuff 

- Relapse=="P2 Referral to 1st Phase"
- Check for data consistency
- Identify clients "still enrolled"-> possibly lost to follow up?

```{r eval = TRUE}

x <- list(title = "Year")
y <- list(title = "Frequency")

temp<-dcast(program, YEAR ~ REASON_FOR_LEAVING, length, value.var="PA_ID")
  colnames(temp) <- c("YEAR","AWOL","GRAD","RESIGNED","ENROLLED")

p <- plot_ly(temp, x=~YEAR, y=~AWOL, colors="Spectral",type="bar",name="AWOL",alpha=0.2) %>%
  add_trace(y = ~GRAD,name="Graduation") %>%
  add_trace(y = ~RESIGNED,name="Resigned before completion") %>%
    add_trace(y = ~ENROLLED,name="Still enrolled in program") %>%
  layout(xaxis = x, yaxis = y, barmode = 'stack')
p
```



