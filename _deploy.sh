#!/bin/sh

Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"
rm -rf ./docs/slides/*.Rmd
rm -rf ./docs/worksheets/*.html
rm -rf ./docs/datasets/*.R
touch ./docs/.nojekyll
git add ./docs/

