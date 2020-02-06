# 0_initialize.r
#
# Created 20190319

library(arm)
library(broom)
library(broom.mixed)
library(car)
library(conflicted)
library(corrplot)
library(feather)
library(geepack)
library(ggExtra)
library(ggstatsplot)
library(ggplot2)
library(gridExtra)
library(here)
library(lme4)
library(lubridate) 
library(paletteer)
library(performance)
library(readxl)
library(skimr)
library(tidyr)
library(tidyverse)
library(varhandle)

set.seed(1)
conflict_prefer('here', 'here')

source(here('r', '0_set_up_environment.r'))
source(here('r', '1_import.r'))
source(here('r', '2_preprocess.r'))
source(here('r', '3_model.r'))

#source(here('r', '3_fit_models.r'))
#source(here('r', '4_export.r'))

message('Run successfully.')
