#!/bin/sh
for n in CharSet.re StateSet.re CharMapStateSet.re CharSetMapStateSet.re StateMapCharSetMapStateSet.re ; do echo "module ${n%.re} = {" ; sed 's/^/  /' $n ; echo "};" ; echo ; done
