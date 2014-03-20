##GitHub testing the push and pull.
###RELATIVE RISK WITH CI###
  ##Probability, Risk Difference, 
  ##Relative Risk and Odds Ratio for Binary Samples
  ##function, with samples s1 and s2 for 
  ##total ts1 and ts2
rr <- function(s1=NULL,s2=NULL,ts1=NULL,ts2=NULL){
  print("INPUT")
  matrix1<-matrix(c(s1,s2,ts1,ts2), nrow=1, byrow=T)
  colnames(matrix1)<-c("s1","s2","ts1","ts2")
  print(matrix1)
  #p1 hat 1
  p1 <- s1/ts1
  #p2 hat 2
  p2 <- s2/ts2
  #Risk difference
  RD <- p1-p2
  #RR hat or risk ratio
  RR <- p1/p2
  print("OUTPUT")
  matrix2<-matrix(c(p1,p2,RD,RR),nrow=1,byrow=T)
  colnames(matrix2)<-c("p1","p2","RD","RR")
  print(matrix2)
  #Paired CI for p(hat)
  xp1 <- (2*(sqrt(p1*((1-p1)/ts1))))
  print(xp1/2)
  xp2 <- (2*(sqrt(p2*((1-p2)/ts2))))
  CIlop1 <- round(p1-xp1, digit=3)
  CIhip1 <- round(p1+xp1, digit=3)
  CIlop2 <- round(p2-xp2, digit=3)
  CIhip2 <- round(p2+xp2, digit=3)
  print(paste("probability (P1) = ", p1,", 95% CI [", CIlop1, "~", CIhip1,"]"))
  print(paste("probability (P2) = ", p2,", 95% CI [", CIlop2, "~", CIhip2,"]"))
  z <- p1-p2/((xp1/2)-(xp2/2))
  print(paste("z-value for p(hat) =",z))
  ##Risk difference (RD)
  xp5 <- (2*(sqrt(((p1*((1-p1)/ts1)))+(p2*((1-p2)/ts2)))))
  CIRDlo <- RD - xp5
  CIRDhi <- RD + xp5
  CIloRD <- round(RD - xp5, digit=3)
  CIhiRD <- round(RD + xp5, digit=3)
  print(paste("risk difference (RD) = ", RD,", 95% CI [",CIloRD,"~", CIhiRD,"]"))
  ##RR hat or risk ratio
  lnRR <- log(RR)
  x3<-(2*(sqrt(((1/s1-1/ts1)+(1/s2-1/ts2)))))
  lnRRCIlo <- lnRR - x3
  lnRRCIhi <- lnRR + x3
  RR1 <- (exp(lnRR))
  RR2 <- round(RR1, digit=3)
  RRCIlo <- (exp(lnRRCIlo))
  RRCIhi <- (exp(lnRRCIhi))
  CIloRR <- round(RRCIlo, digit=3)
  CIhiRR <- round(RRCIhi, digit=3)
  print(paste("relative risk (RR) = ", RR2,", 95% CI [",CIloRR,"~", CIhiRR,"]"))
  #Odds (hat) 
  odd1 <- (p1/(1-p1))
  odd2 <- (p2/(1-p2))
  odd1a <- round(odd1, digit=3)
  odd2a <- round(odd2, digit=3)
  print(paste("odd1=",odd1a,"odd2=",odd2a))
  #odds ration
  OR <- (odd1/odd2)
  lnOR <- log(OR)
  x4<-(2*(sqrt((1/s1)+(1/s2)+(1/(ts1-s1))+(1/(ts2-s2)))))
  lnORCIlo <- lnOR-x4
  lnORCIhi <- lnOR+x4
  OR1 <- (exp(lnOR))
  OR2 <- round(OR1, digit=3)
  ORCIlo <- (exp(lnORCIlo))
  ORCIhi <- (exp(lnORCIhi))
  CIloOR <- round(ORCIlo, digit=3)
  CIhiOR <- round(ORCIhi, digit=3)
  #return()
  print(paste("odds ratio (OR) = ", OR2,", 95% CI [",CIloOR,"~", CIhiOR,"]"))
  
}
#test sample set
rr(142, 251, 8900, 8900)
