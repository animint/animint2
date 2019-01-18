#!/bin/bash
set -o errexit
cd ..
rm -rf animint2-release
cp -r animint2 animint2-release
rm animint2-release/data/*
grep 'docType{data}' animint2-release/man/*.Rd|sed 's/:.*//'|xargs rm
for data in UStornadoes WorldBank worldPop generation.loci breakpoints intreg FluView;do
    cp animint2/data/$data.RData animint2-release/data
    cp animint2/man/$data.Rd animint2-release/man
done
grep -v RSelenium animint2/DESCRIPTION > animint2-release/DESCRIPTION
cat <<EOF > animint2-release/tests/testthat.R 
library(testthat)
test_check("animint2", filter="compiler")
EOF
PKG_TGZ=$(R CMD build animint2-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R CMD INSTALL $PKG_TGZ
R CMD check --as-cran $PKG_TGZ
