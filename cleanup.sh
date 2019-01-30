#!/bin/bash
#
#
#::::: Purpose ::::: 
# Cleanup script for effmass package.
#
#::::: Author :::::
# Contact :: Rajesh Prashanth Anandavadivel
# Email   :: <rajeshprasanth@rediffmail.com>
# First change :: Sun Jan 20 14:49:38 IST 2019
#
#::::: Change Log :::::
#Sun Jan 20 14:49:38 IST 2019 -- First commit
#
echo "Please wait....Cleaning up the package"
find . -name *.mod *.o *.x *~ -exec rm -rf {} \;
echo "Clean up done!!"
