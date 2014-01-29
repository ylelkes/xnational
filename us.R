us <- data.frame(foreign::read.spss("../Update data and provisionsal syntax/survey datasets/US/STAN0040_OUTPUT.sav"))
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

us[us[,1:ncol(us)]>100]=NA
us$caseid <- 1:nrow(us)
us$pid <- trim(us$pid3)


us$democrats <- zero1(us$therm1)
us$republicans <- zero1(us$therm2)

us$affpol <- NA 
us$affpol[which(us$pid=='Democrat')] <- us$democrats[which(us$pid=='Democrat')] -us$republicans[which(us$pid=='Democrat')] 
us$affpol[which(us$pid=='Republican')] <- us$republicans[which(us$pid=='Republican')] -us$democrats[which(us$pid=='Republican')] 


us$environmentpol <-abs(abs(us$scale1c-us$scale1b)-abs(us$scale1c-us$scale1a))
us$bankingpol <-abs(abs(us$scale2c-us$scale2b)-abs(us$scale2c-us$scale2a))
us$healthcarepol <-abs(abs(us$scale3c-us$scale3b)-abs(us$scale3c-us$scale3a))

cor(data.frame(us$environmentpol,us$bankingpol,us$healthcarepol,us$affpol),use="pairwise.complete.obs")


