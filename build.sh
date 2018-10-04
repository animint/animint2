#!/bin/bash
set -o errexit
cd ..
rm -rf animint2-release
cp -r animint2 animint2-release
grep -v RSelenium animint2/DESCRIPTION > animint2-release/DESCRIPTION
rm -rf animint2-release/tests/testthat/*
cp animint2/tests/testthat/test-compiler* animint2-release/tests/testthat
grep -v RSelenium animint2/tests/testthat.R | sed 's/Sys.getenv("TEST_SUITE")/"compiler"/' > animint2-release/tests/testthat.R 
PKG_TGZ=$(R CMD build animint2-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R CMD INSTALL $PKG_TGZ
R CMD check --as-cran $PKG_TGZ
