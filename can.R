can <- foreign::read.spss("../Update data and provisionsal syntax/survey datasets/Canada/MCGL0001_OUTPUT2.sav",use.missings = T,to.data.frame=T,trim_values=T)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
can$caseid <- 1:nrow(can)
## Remove missing values in thermometers
can[can[,1:ncol(can)]>100]=NA
can$caseid <- 1:nrow(can)
##pid
can$pid <- can$VB11s
##strength of party id
zero1(can$therm1) -> can$liberals
zero1(can$therm2) ->can$conservatives
zero1(can$therm3) ->can$ndp
#cons 143
#libs 77
#ndp 37

can$libaffpol <- with(can, (143/(37+143))*(liberals-conservatives)+(37/(37+143))*(liberals-ndp))
can$consaffpol <- with(can, (77/(37+77))*(conservatives-liberals)+(37/(37+77))*(conservatives-ndp))
can$ndpaffpol <- with(can, (77/(77+143))*(ndp-liberals)+(143/(77+143))*(ndp-conservatives))

can$affpol <- NA
can$affpol[which(can$pid=='The Liberal Party')] <- can$libaffpol[which(can$pid=='The Liberal Party')]
can$affpol[which(can$pid=='The Conservative Party')] <- can$consaffpol[which(can$pid=='The Conservative Party')]
can$affpol[which(can$pid=='The New Democratic Party')] <- can$ndpaffpol[which(can$pid=='The New Democratic Party')]

###
can$bank.ideopolliberals <- (143/(37+143))*(abs(abs(can$scale1e-can$scale1a)-abs(can$scale1e-can$scale1b)))+(37/(37+143))*(abs(abs(can$scale1e-can$scale1a)-abs(can$scale1e-can$scale1d)))
can$bank.ideopolconservatives <-  (77/(37+77))*(abs(abs(can$scale1e-can$scale1b)-abs(can$scale1e-can$scale1a)))+ (37/(37+77))*(abs(abs(can$scale1e-can$scale1b)-abs(can$scale1e-can$scale1d)))
can$bank.ideopolndp <- (77/(77+143))*(abs(abs(can$scale1e-can$scale1d)-abs(can$scale1e-can$scale1a)))+(143/(77+143))*(abs(abs(can$scale1e-can$scale1d)-abs(can$scale1e-can$scale1b)))

can$bank.ideopol <- NA
can$bank.ideopol[which(can$pid=='The Liberal Party')]=can$bank.ideopolliberals[which(can$pid=='The Liberal Party')]
can$bank.ideopol[which(can$pid=='The Conservative Party')]=can$bank.ideopolconservatives[which(can$pid=='The Conservative Party')]
can$bank.ideopol[which(can$pid=='The New Democratic Party')]=can$bank.ideopolndp[which(can$pid=='The New Democratic Party')]
#################
###
can$health.ideopolliberals <- (143/(37+143))*(abs(abs(can$scale2e-can$scale2a)-abs(can$scale2e-can$scale2b)))+(37/(37+143))*(abs(abs(can$scale2e-can$scale2a)-abs(can$scale2e-can$scale2d)))
can$health.ideopolconservatives <-  (77/(37+77))*(abs(abs(can$scale2e-can$scale2b)-abs(can$scale2e-can$scale2a)))+ (37/(37+77))*(abs(abs(can$scale2e-can$scale2b)-abs(can$scale2e-can$scale2d)))
can$health.ideopolndp <- (77/(77+143))*(abs(abs(can$scale2e-can$scale2d)-abs(can$scale2e-can$scale2a)))+(143/(77+143))*(abs(abs(can$scale2e-can$scale2d)-abs(can$scale2e-can$scale2b)))

can$health.ideopol <- NA
can$health.ideopol[which(can$pid=='The Liberal Party')]=can$health.ideopolliberals[which(can$pid=='The Liberal Party')]
can$health.ideopol[which(can$pid=='The Conservative Party')]=can$health.ideopolconservatives[which(can$pid=='The Conservative Party')]
can$health.ideopol[which(can$pid=='The New Democratic Party')]=can$health.ideopolndp[which(can$pid=='The New Democratic Party')]
#################
can$energy.ideopolliberals <- (143/(37+143))*(abs(abs(can$scale3e-can$scale3a)-abs(can$scale3e-can$scale3b)))+(37/(37+143))*(abs(abs(can$scale3e-can$scale3a)-abs(can$scale3e-can$scale3d)))
can$energy.ideopolconservatives <-  (77/(37+77))*(abs(abs(can$scale3e-can$scale3b)-abs(can$scale3e-can$scale3a)))+ (37/(37+77))*(abs(abs(can$scale3e-can$scale3b)-abs(can$scale3e-can$scale3d)))
can$energy.ideopolndp <- (77/(77+143))*(abs(abs(can$scale3e-can$scale3d)-abs(can$scale3e-can$scale3a)))+(143/(77+143))*(abs(abs(can$scale3e-can$scale3d)-abs(can$scale3e-can$scale3b)))

can$energy.ideopol <- NA
can$energy.ideopol[which(can$pid=='The Liberal Party')]=can$energy.ideopolliberals[which(can$pid=='The Liberal Party')]
can$energy.ideopol[which(can$pid=='The Conservative Party')]=can$energy.ideopolconservatives[which(can$pid=='The Conservative Party')]
can$energy.ideopol[which(can$pid=='The New Democratic Party')]=can$energy.ideopolndp[which(can$pid=='The New Democratic Party')]

cor(with(can,data.frame(energy.ideopol,health.ideopol,bank.ideopol,affpol)),use="pairwise.complete.obs")
