library(devtools)
use_git()

# Write functions

load_all()

# commit Git

check()

# 1.10 Edit DESCRIPTION - ToDo

use_mit_license()

# 1.12 document() - ToDo
document()

# 1.16 use_package()
use_package("lmom")
use_package("lfstat")

# 11  Dependencies: In Practice
# 11.4 Package is listed in Imports
usethis::use_package_doc()
usethis::use_import_from("lmom", "pelwei")

