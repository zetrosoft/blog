#!/bin/bash

# build site
#--------------------

# printf
printf '\n\nBuilding site...'
printf '\n--------------------\n\n'

# remove existing documentations
rm -rf public

# build documentation
Rscript -e 'blogdown::build_site()'
