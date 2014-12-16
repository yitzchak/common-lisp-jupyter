#! /bin/sh

echo "* Removing temp files *"

find . -type f -name "*~" -exec rm -vf {} \; 

