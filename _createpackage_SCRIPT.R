library(devtools)

use_git()

# Write functions

load_all()
use_r("seasAM_FUN")

# commit Git

check()

# 1.10 Edit DESCRIPTION - ToDo ongoing

use_mit_license()

# 1.12 document() - ToDo ongoing
document()

# 1.16 use_package()
use_package("lmom")
use_package("lfstat")
use_package("copula")
use_package("EnvStats")
use_package("FAdist")
use_package("xtable")

# 11  Dependencies: In Practice
# 11.4 Package is listed in Imports
usethis::use_package_doc()
usethis::use_import_from("lmom", "pelwei")

# Declare global variables used in the examples (or as default values of Funs):
utils::globalVariables(c("AM", "x1"))


# Making Binary Data Available
use_data(xa, xb, xc, xd)


###
#Now to GitHub:
use_github(private=TRUE)


# Finally install from github:
devtools::install_github("glaaha/mixdist")
