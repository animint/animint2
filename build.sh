#!/bin/bash
set -o errexit
cd ..
rm -rf animint2-release
cp -r animint2 animint2-release
grep -v RSelenium animint2/DESCRIPTION > animint2-release/DESCRIPTION
cat <<EOF > animint2-release/tests/testthat.R 
library(testthat)
test_check("animint2", filter="compiler")
EOF
PKG_TGZ=$(R CMD build animint2-release|grep building|sed "s/.*\(animint2.*.tar.gz\).*/\1/")
R CMD INSTALL $PKG_TGZ
R CMD check --as-cran $PKG_TGZ
