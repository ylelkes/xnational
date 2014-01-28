source("../../func.R")
uk <- data.frame(foreign::read.spss("../Update data and provisionsal syntax/survey datasets/UK/STAN0039_OUTPUT.sav"))

uk[uk[,1:ncol(uk)]>100]=NA
uk$caseid <- 1:nrow(uk)
uk$pid <- uk$party
##########################
zero1(uk$therm1) -> uk$conservative
zero1(uk$therm2) -> uk$labour
zero1(uk$therm3) -> uk$libdem

#number of seats: 650
#conservative 306
#labour 258
#lib dem 57

#weights for cons 258/(57+258)
#weights for labour 306/(306+57)

# Weighted Affective Polarization Scores
uk$affpolcon <- (1-.82)*(uk$conservative-uk$libdem)+.82*(uk$conservative-uk$labour)
uk$affpollab <- (57/(306+57))*(uk$labour-uk$libdem)+(306/(306+57))*(uk$labour-uk$conservative)
uk$affpollibdem <-((258/(306+258))*(uk$libdem-uk$conservative)+(306/(306+258))*(uk$libdem-uk$conservative))

uk$affpol <- NA
uk$affpol[which(uk$pid=="Conservative Party")] <- uk$affpolcon[which(uk$pid=="Conservative Party")]
uk$affpol[which(uk$pid=="Labour Party")] <- uk$affpollab[which(uk$pid=="Labour Party")]
uk$affpol[which(uk$pid=="Liberal Democrat Party")] <- uk$affpollibdem[which(uk$pid=="Liberal Democrat Party")]

############# Weighted Ideological Polarization Scores
#immigration limits
uk$immi.ideopolcon <- (1-.82)*(abs(abs(uk$H10_D5d-uk$H10_D5a)-abs(uk$H10_D5d-uk$H10_D5b)))+.82*(abs(abs(uk$H10_D5d-uk$H10_D5a)-abs(uk$H10_D5d-uk$H10_D5c)))
uk$immi.ideopolab <- (57/(306+57))*(abs(abs(uk$H10_D5d-uk$H10_D5c)-abs(uk$H10_D5d-uk$H10_D5b)))+(306/(306+57))*(abs(abs(uk$H10_D5d-uk$H10_D5c)-abs(uk$H10_D5d-uk$H10_D5a)))
uk$immi.ideopollibdem <- (258/(306+258))*(abs(abs(uk$H10_D5d-uk$H10_D5b)-abs(uk$H10_D5d-uk$H10_D5c)))+(306/(306+258))*(abs(abs(uk$H10_D5d-uk$H10_D5b)-abs(uk$H10_D5d-uk$H10_D5a)))

uk$immiideopol <- NA
uk$immiideopol[which(uk$pid=="Conservative Party")] <- uk$immi.ideopolcon[which(uk$pid=="Conservative Party")]
uk$immiideopol[which(uk$pid=="Labour Party")] <- uk$immi.ideopolab[which(uk$pid=="Labour Party")]
uk$immiideopol[which(uk$pid=="Liberal Democrat Party")] <- uk$immi.ideopollibdem[which(uk$pid=="Liberal Democrat Party")]

#########
#Spending 
uk$spend.ideopolcon <- (1-.82)*(abs(abs(uk$H11_D6d-uk$H11_D6a)-abs(uk$H11_D6d-uk$H11_D6b)))+.82*(abs(abs(uk$H11_D6d-uk$H11_D6a)-abs(uk$H11_D6d-uk$H11_D6c)))
uk$spend.ideopolab <- (57/(306+57))*(abs(abs(uk$H11_D6d-uk$H11_D6c)-abs(uk$H11_D6d-uk$H11_D6b)))+(306/(306+57))*(abs(abs(uk$H11_D6d-uk$H11_D6c)-abs(uk$H11_D6d-uk$H11_D6a)))
uk$spend.ideopollibdem <- (258/(306+258))*(abs(abs(uk$H11_D6d-uk$H11_D6b)-abs(uk$H11_D6d-uk$H11_D6c)))+(306/(306+258))*(abs(abs(uk$H11_D6d-uk$H11_D6b)-abs(uk$H11_D6d-uk$H11_D6a)))

uk$spendideopol <- NA
uk$spendideopol[which(uk$pid=="Conservative Party")] <- uk$spend.ideopolcon[which(uk$pid=="Conservative Party")]
uk$spendideopol[which(uk$pid=="Labour Party")] <- uk$spend.ideopolab[which(uk$pid=="Labour Party")]
uk$spendideopol[which(uk$pid=="Liberal Democrat Party")] <- uk$spend.ideopollibdem[which(uk$pid=="Liberal Democrat Party")]

##############
uk$bank.ideopolcon <- (1-.82)*(abs(abs(uk$H12_D7d-uk$H12_D7a)-abs(uk$H12_D7d-uk$H12_D7b)))+.82*(abs(abs(uk$H12_D7d-uk$H12_D7a)-abs(uk$H12_D7d-uk$H12_D7c)))
uk$bank.ideopolab <- (57/(306+57))*(abs(abs(uk$H12_D7d-uk$H12_D7c)-abs(uk$H12_D7d-uk$H12_D7b)))+(306/(306+57))*(abs(abs(uk$H12_D7d-uk$H12_D7c)-abs(uk$H12_D7d-uk$H12_D7a)))
uk$bank.ideopollibdem <- (258/(306+258))*(abs(abs(uk$H12_D7d-uk$H12_D7b)-abs(uk$H12_D7d-uk$H12_D7c)))+(306/(306+258))*(abs(abs(uk$H12_D7d-uk$H12_D7b)-abs(uk$H12_D7d-uk$H12_D7a)))

uk$bankideopol <- NA
uk$bankideopol[which(uk$pid=="Conservative Party")] <- uk$bank.ideopolcon[which(uk$pid=="Conservative Party")]
uk$bankideopol[which(uk$pid=="Labour Party")] <- uk$bank.ideopolab[which(uk$pid=="Labour Party")]
uk$bankideopol[which(uk$pid=="Liberal Democrat Party")] <- uk$bank.ideopollibdem[which(uk$pid=="Liberal Democrat Party")]

cor(with(uk,data.frame(spendideopol,bankideopol,immiideopol,affpol)),use="pairwise.complete.obs")
uk$party_strength
lm(affpol~spendideopol+bankideopol+immiideopol+party_strength,uk)