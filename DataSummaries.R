
# final data set
HospitalDRGMedicare <- read_excel("10Hospitals.xlsx")

summary(as.factor(HospitalDRGMedicare$hospitalID))

# There are no 5 DRGS that have data for all 10 hospitals, so I will be using 1 DRG code that is found in all 10, 3 DRG codes that are found in 9, and 1 that is found in 8 hospitals.
inCommon <- HospitalDRGMedicare %>% group_by(DRG) %>% summarize(count=n_distinct(hospitalID))
inCommon10 <- inCommon %>% filter(count==10)
inCommon10
inCommon9 <- inCommon %>% filter(count>=9)
inCommon9
inCommon8 <- inCommon %>% filter(count>=8)
inCommon8

Hospital5DRG <- HospitalDRGMedicare %>% filter(DRG==460 | DRG==467 | DRG==470 | DRG==483 | DRG==638)
summary(as.factor(Hospital5DRG$hospitalID))

# Carney only has 2, so I am going to include 3 more DRG codes
# 177 - RESPIRATORY INFECTIONS AND INFLAMMATIONS WITH MCC
# 193 - SIMPLE PNEUMONIA AND PLEURISY WITH MCC
# 309 - CARDIAC ARRHYTHMIA AND CONDUCTION DISORDERS WITH CC

Hospital8DRG <- HospitalDRGMedicare %>% filter(DRG==177 | DRG==193 | DRG==309 | DRG==460 | DRG==467 | DRG==470 | DRG==483 | DRG==638)
summary(as.factor(Hospital8DRG$hospitalID))

# 5. Summarize data for hospitals
# can't summarize all 10 hospitals because they don't all have the same DRG codes
# average DRG codes for DRG Codes 460, 467, 470, 483, 638
# only hospitals that have all 5 above are Beth Israel, BMC, Brigham, Duke Regional, Duke University, MGH, and Tufts

hospitalAll5 <- Hospital5DRG %>% filter(hospitalID != "Carney" & hospitalID != "NC" & hospitalID != "NEBaptist") %>%
  filter(DRG==460 | DRG==467 | DRG==470 | DRG==483 | DRG==638)

hospitalTable <- hospitalAll5 %>%
  group_by(hospitalID) %>% summarize(meanGross = mean(grossCharge,na.rm=TRUE), meanNeg=mean(negotiatedPayment,na.rm=TRUE), meanMed=mean(medicarePayment,na.rm=TRUE))
print(hospitalTable)

stacked_df <- stack(hospitalAll5)

boxplot(meanGross ~ , data = stacked_df)

# 6. Summarize specific procedures
byDRG <- Hospital8DRG %>% group_by(DRG) %>% summarize(avgGross = mean(grossCharge,na.rm=TRUE), avgNeg = mean(negotiatedPayment,na.rm=TRUE), avgMedicare = mean(medicarePayment,na.rm=TRUE))
ggplot(byDRG, aes(x=DRG,y=avgGross, color='gross')) + geom_point() +
  geom_point(data=byDRG, aes(x=DRG,y=avgNeg,color='negotiated')) +
  geom_point(data=byDRG, aes(x=DRG,y=avgMedicare,color='medicare')) +
  ggtitle("Gross Charge and Average Negotiated and Medicare Payments by DRG Codes") +
  xlab("DRG Code") + ylab("$ Amount")

boxplot(grossCharge ~ DRG, data = Hospital8DRG)
boxplot(Hospital8DRG$DRG ~ Hospital8DRG$grossCharge)

boxplot(Hospital8DRG, col = rainbow(ncol(Hospital8DRG)))

byDRGrange <- Hospital8DRG %>% mutate(grossCharge=ifelse(grossCharge==0,NA,grossCharge)) %>% mutate(negotiatedPayment=ifelse(negotiatedPayment==0,NA,negotiatedPayment)) %>% mutate(medicarePayment=ifelse(medicarePayment==0,NA,medicarePayment))%>%
  group_by(DRG) %>% summarize(rangeGross=range(grossCharge,na.rm=TRUE), rangeNeg=range(negotiatedPayment,na.rm=TRUE), rangeMedicare=range(medicarePayment,na.rm=TRUE))
ggplot(byDRGrange, aes(x=DRG,y=rangeGross, color='Range of gross charges')) + geom_point() +
  geom_point(data=byDRGrange, aes(x=DRG,y=rangeNeg,color='Range of negotiated payments')) +
  geom_point(data=byDRGrange, aes(x=DRG,y=rangeMedicare,color='Range of Medicare payments')) +
  ggtitle("Gross Charge and Average Negotiated and Medicare Payment Ranges by DRG Codes") +
  xlab("DRG Code") + ylab("$ Amount")

# line graph doesn't really make sense here

# 7. Explain

# 8. Market share
# focusing on 470 because it is the only DRG code that all 10 hospitals have data for
BostonDRG470 <- read_csv("~/Desktop/ECON372FINAL/BostonDRG470.csv")
DurhamDRG470 <- read_csv("~/Desktop/ECON372FINAL/DurhamDRG470.csv")

summary(as.factor(BostonDRG470$Rndrng_Prvdr_Org_Name))
sum(BostonDRG470$Tot_Dschrgs)
BostonDRG470 <- BostonDRG470 %>% mutate(marketTotal = sum(BostonDRG470$Tot_Dschrgs))

summary(as.factor(DurhamDRG470$Rndrng_Prvdr_Org_Name))
sum(DurhamDRG470$Tot_Dschrgs)
DurhamDRG470 <- DurhamDRG470 %>% mutate(marketTotal = sum(DurhamDRG470$Tot_Dschrgs))

marketShare <- rbind(BostonDRG470,DurhamDRG470) %>% mutate(marketShare = Tot_Dschrgs/marketTotal)

only470 <- read_excel("~/Desktop/ECON372FINAL/10Hospitals.xlsx") %>% filter(DRG==470) %>% mutate(marketShare=medicareDischarges/medicareMarketDischarges)

# GRAPH FOR 8
ggplot(only470, aes(x=marketShare,y=negotiatedPayment, color=hospitalID)) + geom_point() +
  ggtitle("Negotiated Payments for DRG Code 470 by Market Shares") +
  xlab("Market Share") + ylab("Negotiated Payment ($)")

bostonShare <- marketShare %>% filter(Rndrng_Prvdr_City=="Boston")
durhamShare <- marketShare %>% filter(Rndrng_Prvdr_City=="Durham")

# pie charts to add in appendix
ggplot(bostonShare, aes(x="", y=marketShare, fill=Rndrng_Prvdr_Org_Name)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle("Market Shares of Hospitals in Boston for DRG Code 470")

ggplot(durhamShare, aes(x="", y=marketShare, fill=Rndrng_Prvdr_Org_Name)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle("Market Shares of Hospitals in Durham for DRG Code 470")

# 9. Compliance

byCompliance <- HospitalDRGMedicare %>% group_by(compliance,DRG) %>% summarize(meanMedPay = mean(medicarePayment,na.rm=TRUE))

ggplot(byCompliance, aes(x=DRG,y=meanMedPay,color=compliance)) + geom_point() +
  ggtitle("Mean Medicare Payment by DRG and Compliance") +
  xlab("DRG Code") + ylab("Mean Medicare Payment ($)")






