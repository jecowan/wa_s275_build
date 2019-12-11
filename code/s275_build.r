
#############################################################################
# s275_build.r
#
# Stack public-use S-275 files. 
# See https://github.com/jecowan/s275_build for codebook
# and descriptions of data sources.
# 
# Author:  James Cowan
# Created: 2019-12-11
#############################################################################


library(tidyverse)
library(lubridate)
library(stringr)
library(haven)
library(forcats)
library(fredr)
fredr_set_key("54d9f391adc59e5aa34488a9e89a4a04")

source("./code/download_files.r")
source("./code/s275_stack.r")
