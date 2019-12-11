

#############################################################################
# s275_stack.r
#
# Stack annual S-275 datasets (1996-2018).
# S275 files from
# https://www.dropbox.com/sh/dbkw661evhd046o/AADvUOVcKPro_6t7vpBWlB0ka?dl=0
#
# Author:  James Cowan
# Created: 2017-12-18
# Revised: 2018-09-03, 2019-12-11
#############################################################################



#############################################################################
#               Define function to clean files.
#############################################################################

s275_clean <- function(x, y) {

  # Convert names to upper case.
  x$fname <- toupper(x$fname)
  x$mname <- toupper(x$mname)
  x$lname <- toupper(x$lname)

  # School year.
  x$syear <- y

  # School district and building.
  if(y<2012) {
    x$codistid <- paste(str_pad(x$cou, 2, pad="0"),
                        str_pad(x$dis, 3, pad="0"), sep="")
  }
  if(y>=2012) {
    x$codistid <- str_pad(x$codistid,5,pad="0")
  }
  x$bldg <- str_pad(as.character(x$bldgn), 4, pad="0")

  # Convert duty codes to character.
  x$droot <- as.character(x$droot)
  x$dsufx <- as.character(x$dsufx)

  # Sex.
  x$male <- x$sex=="M"

  # Race/ethnicity/Hispanic origin.
  x$ethnic <- toupper(x$ethnic)
  if(y<2011){
    x$race <- factor(x$ethnic, levels=c("A","B","H","I","W","M"),
                     labels=c("Asian", "Black", "Hispanic","American Indian",
                              "White","Multiracial"))
  }
  if(y>=2011){
    x$race <- gsub("\\s", "", x$ethnic)
    x$race[which(nchar(x$ethnic)>1)] <- "M"
    x$race[which(x$hispanic=="Y")] <- "H"
    x$race <- factor(x$race, levels=c("A","B","H","I","W","M"),
                     labels=c("Asian", "Black", "Hispanic","American Indian",
                              "White","Multiracial"))
  }

  # Grade level.
  x$grade <- toupper(x$grade)
  if(y>=1995 & y<=2010){
    x$grade <- recode(x$grade, P="Preschool", K="Kindergarten", E="Primary",
                      M="Primary", S="Secondary") %>%
      factor(levels=c("Preschool","Kindergarten","Primary","Secondary"),
             labels=c("Preschool","Kindergarten",
                      "Primary (1-6)","Secondary (7-12)"))
  }
  if(y==2011){
    x$grade <- recode(x$grade, P="Preschool", K="Kindergarten", E="Primary",
                      F="Primary", M="Primary", S="Secondary") %>%
      factor(levels=c("Preschool","Kindergarten","Primary","Secondary"),
             labels=c("Preschool","Kindergarten",
                      "Primary (1-6)","Secondary (7-12)"))
  }
  if(y==2012 | y==2013){
    x$grade <- recode(x$grade, P="Preschool", K="Kindergarten", E="Primary",
                      F="Primary", M="Secondary", H="Secondary") %>%
      factor(levels=c("Preschool","Kindergarten","Primary","Secondary"),
             labels=c("Preschool","Kindergarten",
                      "Primary (1-6)","Secondary (7-12)"))
  }
  if(y>=2014){
    x$grade <- recode(x$grade, P="Preschool", K="Kindergarten", E="Primary",
                      `1`="Primary", `2`="Primary", `3`="Primary", `4`="Primary",
                      `5`="Primary", `6`="Primary", M="Secondary", H="Secondary") %>%
      factor(levels=c("Preschool","Kindergarten","Primary","Secondary"),
             labels=c("Preschool","Kindergarten",
                      "Primary (1-6)","Secondary (7-12)"))
  }


  # CBRTN.
  x$cbrtn <- toupper(x$cbrtn) %>%
    factor(levels=c("C","B","R","T","N"),
           labels=c("Continuing","Beginning","Re-Entering","Transferring","New"))

  # Degree and completion year.
  x$hdeg <- toupper(x$hdeg) %>%
    factor(levels=c("B","H","G","M","D","V","S"),
           labels=c("Bachelors","Hold-harmless BA","BA 135+","Masters",
                    "Doctorate","Vocational","Special"))
  if(y==2011) {
    x$hyear <- trimws(x$hyear)
    x <- mutate(x, hyear=ifelse(hyear=="B0","2010",hyear))
    x$hyear <- as.numeric(x$hyear)
  }

  # Rescale variables.
  if (y<2001) {
    x$exp     <- x$exp/10
    x$acred   <- x$acred/10
    x$icred   <- x$icred/10
    x$bcred   <- x$bcred/10
    x$vcred   <- x$vcred/10
    x$certfte <- x$certfte/1000
    x$clasfte <- x$clasfte/1000
    x$assfte  <- x$assfte/1000
    x$asshpy  <- x$asshpy/100
  }

  x <- select(x, cert, syear, codistid,
              lname, fname, mname, male, race, hdeg, hyear,
              acred, icred, bcred, vcred, exp, cbrtn,
              certfte, clasfte, tfinsal,
              bldg, prog, act, droot, dsufx, grade, assfte, asshpy)

  return(x)
}

#############################################################################
#               Import and clean annual files.
#############################################################################

# 1995-1996
s275.96 <- read.csv("./intermediate/1995-1996S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname, fname, mname, cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(1996)

# 1996-1997
s275.97 <- read.csv("./intermediate/1996-1997S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname, fname, mname, cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(1997)

# 1997-1998
s275.98 <- read.csv("./intermediate/1997-1998S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname, fname, mname, cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(1998)

# 1998-1999
s275.99 <- read.csv("./intermediate/1998-1999S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname, fname, mname, cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(1999)

# 1999-2000
s275.00 <- read.csv("./intermediate/1999-2000S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname, fname, mname, cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2000)

# 2000-2001
s275.01 <- read.csv("./intermediate/2000-2001S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert=Cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2001)

# 2001-2002
s275.02 <- read.csv("./intermediate/2001-2002S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2002)

# 2002-2003
s275.03 <- read.csv("./intermediate/2002-2003S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2003)

# 2003-2004
s275.04 <- read.csv("./intermediate/2003-2004S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2004)

# 2004-2005
s275.05 <- read.csv("./intermediate/2004-2005S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2005)

# 2005-2006
s275.06 <- read.csv("./intermediate/2005-2006S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2006)

# 2006-2007
s275.07 <- read.csv("./intermediate/2006-2007S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2007)

# 2007-2008
s275.08 <- read.csv("./intermediate/2007-2008S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2008)

# 2008-2009
s275.09 <- read.csv("./intermediate/2008-2009S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2009)

# 2009-2010
s275.10 <- read.csv("./intermediate/2009-2010S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2010)

# 2010-2011
s275.11 <- read.csv("./intermediate/2010-2011S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE, strip.white=TRUE) %>%
  select(cou, dis, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2011)

# 2011-2012
s275.12 <- read.csv("./intermediate/2011-2012S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2012)

# 2012-2013
s275.13 <- read.csv("./intermediate/2012-2013S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2013)

# 2013-2014
s275.14 <- read.csv("./intermediate/2013-2014S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2014)

# 2014-2015
s275.15 <- read.csv("./intermediate/2014-2015S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2015)

# 2015-2016
s275.16 <- read.csv("./intermediate/2015-2016S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2016)

# 2016-2017
s275.17 <- read.csv("./intermediate/2016-2017S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2017)

# 2017-2018
s275.18 <- read.csv("./intermediate/2017-2018S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2018)

# 2018-2019
s275.19 <- read.csv("./intermediate/2018-2019S275FinalForPublic.csv",
                    header=TRUE, skipNul=TRUE,
                    stringsAsFactors=FALSE) %>%
  select(codistid=codist, lname=LastName, fname=FirstName, mname=MiddleName,
         cert, sex, ethnic=race, hispanic,
         hdeg, hyear, acred, icred, bcred, vcred, exp, certfte, clasfte,
         tfinsal, cbrtn, prog, act, droot, dsufx, grade, bldgn, assfte, asshpy) %>%
  s275_clean(2019)


#############################################################################
# Merge datasets.
#############################################################################
s275.dat <- bind_rows(s275.96, s275.97, s275.98, s275.99, s275.00, s275.01,
                      s275.02, s275.03, s275.04, s275.05, s275.06, s275.07,
                      s275.08, s275.09, s275.10, s275.11, s275.12, s275.13,
                      s275.14, s275.15, s275.16, s275.17, s275.18, s275.19) %>%
  arrange(cert, syear, codistid, bldg)
s275.district <- select(s275.dat, cert, syear, codistid, lname, fname, mname, 
                        male, race, hdeg, hyear, exp, certfte, clasfte, 
                        tfinsal, cbrtn) %>% distinct()
s275.assignment <- select(s275.dat, cert, syear, codistid, bldg, 
                          prog, act, droot, dsufx, grade, assfte)
rm(s275.dat, s275.96, s275.97, s275.98, s275.99, s275.00, s275.01,
   s275.02, s275.03, s275.04, s275.05, s275.06, s275.07,
   s275.08, s275.09, s275.10, s275.11, s275.12, s275.13,
   s275.14, s275.15, s275.16, s275.17, s275.18, s275.19)
                      

# Deflate salaries using annual PCE data.
# Index 2012=100, Seasonally Adjusted
pcepi <- fredr("DPCERG3A086NBEA") %>% mutate(syear = year(date) + 1)
s275.district <- left_join(s275.district, pcepi, by="syear") %>%
  mutate(tfinsal12 = tfinsal * (100 / value)) %>%
  select(-series_id, -date)
rm(pcepi)

# Fix incorrectly coded years.
xtabs(~hyear, s275.district)
s275.district <- mutate(s275.district,
                        hyear=ifelse(hyear==0 & is.na(hdeg)==TRUE,NA,hyear),
                        hyear=ifelse((hyear==1900 | hyear==1915) &
                                       is.na(hdeg)==TRUE,NA,hyear),
                        hyear=ifelse(hyear==0 & is.na(hdeg)==FALSE,2000,hyear),
                        hyear=ifelse(hyear>0 & hyear<100, hyear+1900, hyear),
                        hyear=ifelse(hyear>=100 & hyear<=1900, NA, hyear),
                        hyear=ifelse(hyear>=2020, NA, hyear))
xtabs(~hyear, s275.district)


s275.district <- mutate(s275.district,
                        cert=trimws(cert),
                        lname=trimws(lname),
                        fname=trimws(fname),
                        mname=trimws(mname))
s275.assignment <- mutate(s275.assignment,
                          cert=trimws(cert),
                          prog=trimws(prog),
                          act=trimws(act),
                          droot=trimws(droot),
                          dsufx=trimws(dsufx))

save(s275.district, file="./output/s275_district_1996_2019.RData")
save(s275.assignment, file="./output/s275_assignment_1996_2019.RData")
rm(list = ls())

