#!/bin/bash
set -o errexit
R -e 'devtools::document()'
cd ..
rm -rf animint2-release
cp -r animint2 animint2-release
rm animint2-release/data/*
grep 'docType{data}' animint2-release/man/*.Rd|sed 's/:.*//'|xargs rm
for data in UStornadoes WorldBank worldPop generation.loci breakpoints intreg FluView;do
    cp animint2/data/$data.RData animint2-release/data
    cp animint2/man/$data.Rd animint2-release/man
done
for data in diamonds economics faithfuld luv_colours midwest mpg msleep presidential seals txhousing; do
    cp animint2/data/$data.rda animint2-release/data
    cp animint2/man/$data.Rd animint2-release/man
done
cp animint2/data/economics_long.rda animint2-release/data
cp animint2/man/animint2-gganimintproto.Rd animint2-release/man
cp animint2/man/graphical-units.Rd animint2-release/man
egrep -v 'VignetteBuilder|RSelenium' animint2/DESCRIPTION > animint2-release/DESCRIPTION
rm animint2-release/tests/testthat/helper-HTML.R
rm animint2-release/tests/testthat/test-compiler-chunk-vars.R
rm animint2-release/tests/testthat/test-compiler-ghpages.R
rm animint2-release/vignettes/animint2.Rmd #to save disk space
cat <<EOF > animint2-release/tests/testthat.R 
library(testthat)
test_check("animint2", filter="compiler")
EOF
PKG_TGZ=$(R CMD build animint2-release|grep building|sed "s/.*\(animint2.*.tar.gz\).*/\1/")
echo built $PKG_TGZ so now we INSTALL 
R CMD INSTALL $PKG_TGZ
R CMD check --as-cran $PKG_TGZ
