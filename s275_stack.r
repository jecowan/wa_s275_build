

#############################################################################
# s275_stack.r
#
# Stack annual S-275 datasets (1996-2021).
# S275 files from
# https://www.dropbox.com/sh/dbkw661evhd046o/AADvUOVcKPro_6t7vpBWlB0ka?dl=0
#
# Author:  James Cowan
# Created: 2017-12-18
# Revised: 2018-09-03, 2019-12-11, 2022-09-27
#############################################################################


library(tidyverse)
library(fredr)
library(lubridate)

# Download raw S-275 data from Dropbox. ----
options(timeout=240)
download.file("https://www.dropbox.com/sh/r878qc2pp6te4uo/AAB6ws2kdcWUMHtrw4v8l6FPa?dl=1",
              "./input/s-275-1990.zip")
download.file("https://www.dropbox.com/sh/u81mhks8uo7jblr/AABj9Wykr02i2vvWgkZEHGKOa?dl=1",
              "./input/s-275-2000.zip")
download.file("https://www.dropbox.com/sh/97z54ms8ph4n6xq/AACXS81X3h2bR4EHdHyNKYOya?dl=1",
              "./input/s-275-2010.zip")
download.file("https://www.dropbox.com/sh/hbizk99mjhbqsyx/AAB4jC1ehsiVJvsvTHX_Lahma?dl=1",
              "./input/s-275-2020.zip")

unzip(zipfile = "./input/s-275-1990.zip", exdir = "./input/s-275-1990")
unzip(zipfile = "./input/s-275-2000.zip", exdir = "./input/s-275-2000")
unzip(zipfile = "./input/s-275-2010.zip", exdir = "./input/s-275-2010")
unzip(zipfile = "./input/s-275-2020.zip", exdir = "./input/s-275-2020")

file.remove("./input/s-275-1990.zip")
file.remove("./input/s-275-2000.zip")
file.remove("./input/s-275-2010.zip")
file.remove("./input/s-275-2020.zip")


# Define function to clean files. ----
s275_clean <- function(file, y) {
  
  # Import
  x = read.csv(file, header = TRUE, skipNul = TRUE,
               stringsAsFactors = FALSE, strip.white = TRUE)
  names(x) = tolower(names(x))
  
  # Convert names to upper case.
  if(y >= 2001) {
    x = x %>%
      rename(lname = lastname, 
             fname = firstname, 
             mname = middlename)
  }
  x$fname = toupper(x$fname)
  x$mname = toupper(x$mname)
  x$lname = toupper(x$lname)

  # School district and building.
  if(y<2012) {
    x$codistid = paste(str_pad(x$cou, 2, pad="0"),
      str_pad(x$dis, 3, pad="0"), sep="")
  }
  if(y>=2012) {
    x = x %>% rename(codistid = codist)
    x$codistid = str_pad(x$codistid,5,pad="0")
  }
  x$bldg = str_pad(as.character(x$bldgn), 4, pad="0")
  
  # Convert duty codes to character.
  x$droot = as.character(x$droot)
  x$dsufx = as.character(x$dsufx)
  
  # CBRTN.
  x$cbrtn = toupper(x$cbrtn) %>%
    factor(levels=c("C", "B", "R", "T", "N"),
      labels = c("Continuing", "Beginning", "Re-Entering", 
                 "Transferring", "New"))
  
  # Degree and completion year.
  x$hdeg = toupper(x$hdeg) %>%
    factor(levels=c("B","H","G","M","D","V","S"),
      labels=c("Bachelors","Hold-harmless BA","BA 135+","Masters",
        "Doctorate","Vocational","Special"))
  if(y==2011) {
    x$hyear = trimws(x$hyear)
    x = mutate(x, hyear=ifelse(hyear=="B0", "2010", hyear))
    x$hyear = as.numeric(x$hyear)
  }
  
  # Rescale variables.
  if (y<2001) {
    x = mutate(x, 
      exp = exp/ 10,
      acred = acred / 10,
      icred = icred / 10,
      bcred = bcred / 10,
      vcred = vcred / 10,
      certfte = certfte / 1000,
      clasfte = clasfte / 1000,
      assfte = assfte / 1000,
      asshpy = asshpy / 100)
  }
  
  # Staff mix factor.
  if(y <= 1999){
    x = mutate(x, camix = camix / 100000)
  }
  if(y == 2000){
    x = mutate(x, camix = camix1s / 100000)
  }
  if(y >= 2001 & y <= 2003) {
    x = mutate(x, camix = camix1s)
  }
  if(y == 2004) {
    x = mutate(x, camix = camix1sa)
  }
  if(y >= 2005 & y <= 2007) {
    x = mutate(x, camix = camix1sb)
  }
  if(y >= 2008) {
    x = mutate(x, camix = camix1)
  }
  
  # Select variables to retain.
  if (y < 2011) {
    x = x %>%
      select(cert, codistid,
             lname, fname, mname, sex, ethnic, hdeg, hyear,
             acred, icred, bcred, vcred, exp, cbrtn, camix,
             certfte, clasfte, tfinsal,
             bldg, prog, act, droot, dsufx, grade, assfte, asshpy)
  }
  if (y >= 2011) {
    x = x %>%
      select(cert, codistid,
             lname, fname, mname, sex, race, hispanic, hdeg, hyear,
             acred, icred, bcred, vcred, exp, cbrtn, camix,
             certfte, clasfte, tfinsal,
             bldg, prog, act, droot, dsufx, grade, assfte, asshpy)
  }
  x = x %>% add_column(syear = y)
  
  # Remove empty space from string variables.
  x = x %>% mutate(x,
                   cert=trimws(cert),
                   lname=trimws(lname),
                   fname=trimws(fname),
                   mname=trimws(mname),
                   prog=trimws(prog),
                   act=trimws(act),
                   droot=trimws(droot),
                   dsufx=trimws(dsufx),
                   grade=trimws(grade))
  
  return(x)
}


# Import and clean annual files. ----
syear = 1996:2021
files = c("./input/s-275-1990/1995-1996S275FinalForPublic.csv",
          "./input/s-275-1990/1996-1997S275FinalForPublic.csv",
          "./input/s-275-1990/1997-1998S275FinalForPublic.csv",
          "./input/s-275-1990/1998-1999S275FinalForPublic.csv",
          "./input/s-275-1990/1999-2000S275FinalForPublic.csv",
          "./input/s-275-2000/2000-2001S275FinalForPublic.csv",
          "./input/s-275-2000/2001-2002S275FinalForPublic.csv",
          "./input/s-275-2000/2002-2003S275FinalForPublic.csv",
          "./input/s-275-2000/2003-2004S275FinalForPublic.csv",
          "./input/s-275-2000/2004-2005S275FinalForPublic.csv",
          "./input/s-275-2000/2005-2006S275FinalForPublic.csv",
          "./input/s-275-2000/2006-2007S275FinalForPublic.csv",
          "./input/s-275-2000/2007-2008S275FinalForPublic.csv",
          "./input/s-275-2000/2008-2009S275FinalForPublic.csv",
          "./input/s-275-2000/2009-2010S275FinalForPublic.csv",
          "./input/s-275-2010/2010-2011S275FinalForPublic.csv",
          "./input/s-275-2010/2011-2012S275FinalForPublic.csv",
          "./input/s-275-2010/2012-2013S275FinalForPublic.csv",
          "./input/s-275-2010/2013-2014S275FinalForPublic.csv",
          "./input/s-275-2010/2014-2015S275FinalForPublic.csv",
          "./input/s-275-2010/2015-2016S275FinalForPublic.csv",
          "./input/s-275-2010/2016-2017S275FinalForPublic.csv",
          "./input/s-275-2010/2017-2018S-275FinalForPublic.csv",
          "./input/s-275-2010/2018-2019S-275FinalForPublic.csv",
          "./input/s-275-2010/2019-2020S-275FinalForPublic.csv",
          "./input/s-275-2020/2020-2021S-275FinalForPublic.csv")
s275_dat = bind_cols(file = files, y = syear) %>%
  pmap(s275_clean) %>%
  bind_rows() %>%
  group_by(syear) %>%
  nest()

# Save stacked data. 
save(s275_dat, file="./output/s275_stacked.RData")


# Create district and assignment files. ----
s275_dat = s275_dat %>%
  unnest(cols = c(data)) %>%
  bind_rows()

# Drop personnel without certificate numbers.
s275_dat = filter(s275_dat, cert != "")

# Sex.
s275_dat = s275_dat %>%
  mutate(male = ifelse(sex == "M", 1, 0)) %>%
  select(-sex)

# Race/ethnicity/Hispanic origin.
xtabs(~ethnic, s275_dat, addNA = TRUE)
xtabs(~race, s275_dat, addNA = TRUE)
xtabs(~hispanic, s275_dat, addNA = TRUE)

s275_dat = s275_dat %>%
  mutate(ethnic = toupper(ethnic)) %>%
  mutate(race = if_else(syear < 2011, ethnic, race)) %>%
  mutate(race = gsub("\\s", "", race)) %>%
  mutate(race = gsub("P", "A", race)) %>%
  mutate(race = gsub("([A-Z])\\1+", "\\1", race)) %>%
  mutate(race = if_else(nchar(race) > 1, "M", race)) %>%
  mutate(race = if_else(hispanic == "Y", "H", race, race)) %>%
  mutate(race = factor(race, 
                       levels=c("A","B","H","I","W","M"),
                       labels=c("Asian", "Black", "Hispanic", "American Indian",
                                "White", "Multiracial"))) %>%
  select(-ethnic, -hispanic)
xtabs(~race + syear, s275_dat, addNA = TRUE)


# Create district dataset with teacher characteristics. ----
s275_district = s275_dat %>%
  select(cert, syear, codistid, lname, fname, 
  male, race, hdeg, hyear, exp, certfte, clasfte, 
  tfinsal, cbrtn, camix) %>% distinct()

# Resolve multiple observations per district/year using means of
# experience and salary.
s275_district = s275_district %>%
  group_by(cert, syear, codistid) %>%
  mutate(tfinsal = mean(tfinsal),
    exp = mean(exp),
    camix = mean(camix)) %>% distinct() %>%
  group_by(cert, syear, codistid) %>%
  mutate(numobs = n()) %>%
  filter(numobs == 1) %>% select(-numobs) %>% ungroup()

# Deflate salaries using annual PCE data.
# Index 2012=100, Seasonally Adjusted
pcepi = fredr("DPCERG3A086NBEA") %>% mutate(syear = year(date) + 1)
s275_district = s275_district %>%
  left_join(pcepi, by="syear") %>%
  mutate(tfinsal12 = tfinsal * (100 / value)) %>%
  select(-series_id, -date, -value, -realtime_start, -realtime_end)
rm(pcepi)

# Fix incorrectly coded years.
xtabs(~hyear, s275_district)
s275_district = s275_district %>%
  mutate(hyear=ifelse(hyear==0 & is.na(hdeg)==TRUE,NA,hyear),
  hyear=ifelse((hyear==1900 | hyear==1915) &
      is.na(hdeg)==TRUE,NA,hyear),
  hyear=ifelse(hyear==0 & is.na(hdeg)==FALSE,2000,hyear),
  hyear=ifelse(hyear>0 & hyear<100, hyear+1900, hyear),
  hyear=ifelse(hyear>=100 & hyear<=1900, NA, hyear),
  hyear=ifelse(hyear>=2040, NA, hyear))
xtabs(~hyear, s275_district)


# Create assignment dataset with job information. ----
s275_assignment = select(s275_dat, cert, syear, codistid, bldg, 
  prog, act, droot, dsufx, grade, assfte)

# Recode grade level.
s275_assignment = s275_assignment %>%
  mutate(grade = toupper(grade))
xtabs(~syear + grade, s275_assignment)

s275_assignment = s275_assignment %>%
  mutate(grade = ifelse(syear <= 2010, 
                        recode(grade, P="Preschool", K="Kindergarten", 
                               E="Primary (1-6)", M="Primary (1-6)", 
                               S="Middle/Secondary (7-12)"), 
                        grade)) %>%
  mutate(grade = ifelse(syear == 2011, 
                        recode(grade, P="Preschool", K="Kindergarten", 
                               E="Primary (1-6)", 
                               F="Primary (1-6)",
                               M="Primary (1-6)", 
                               S="Middle/Secondary (7-12)"), 
                        grade)) %>%
  mutate(grade = ifelse(syear %in% 2012:2013,
                        recode(grade, P="Preschool", K="Kindergarten", 
                               E="Primary (1-6)", 
                               F="Primary (1-6)",
                               M="Middle/Secondary (7-12)",
                               H="Middle/Secondary (7-12)",
                               S="Middle/Secondary (7-12)"), 
                        grade)) %>%
  mutate(grade = ifelse(syear >= 2014, 
                        recode(grade, P="Preschool", K="Kindergarten", E="Primary (1-6)",
                               `1`="Primary (1-6)", `2`="Primary (1-6)", 
                               `3`="Primary (1-6)", `4`="Primary (1-6)",
                               `5`="Primary (1-6)", `6`="Primary (1-6)", 
                               F="Primary (1-6)",
                               M="Middle/Secondary (7-12)", 
                               H="Middle/Secondary (7-12)",
                               `7`="Middle/Secondary (7-12)",
                               `8`="Middle/Secondary (7-12)",
                               S="Middle/Secondary (7-12)"),
                        grade))

xtabs(~syear + grade, s275_assignment)
s275_assignment = s275_assignment %>%
  mutate(grade = ifelse(grade %in% c("Preschool", "Kindergarten",
                                     "Primary (1-6)", 
                                     "Middle/Secondary (7-12)"), grade, "")) %>%
  mutate(grade = factor(grade, levels = c("Preschool", "Kindergarten",
                                          "Primary (1-6)", 
                                          "Middle/Secondary (7-12)")))
xtabs(~syear + grade, s275_assignment)


save(s275_district, file="./output/s275_district.RData")
save(s275_assignment, file="./output/s275_assignment.RData")

rm(s275_assignment, s275_dat, s275_district)


