#!/bin/bash

# Author: Dimitri Sabadie <dimitri.sabadie@gmail.com>
# 2015

if [ $# -lt 2 ]; then
  echo "Usage: ./stack-haddock-upload.sh NAME VERSION"
  exit 1
fi

dist=`stack path --dist-dir --stack-yaml ./stack.yaml 2> /dev/null`

echo -e "\033[1;36mGenerating documentation...\033[0m"
stack haddock 2> /dev/null

if [ "$?" -eq "0" ]; then
  docdir=$dist/doc/html
  cd $docdir || exit
  doc=$1-$2-docs
  echo -e "Compressing documentation from \033[1;34m$docdir\033[0m for \033[1;35m$1\033[0m-\033[1;33m$2\033[1;30m"
  cp -r $1 $doc
  tar -c -v -z --format=ustar -f $doc.tar.gz $doc
  echo -e "\033[1;32mUploading to Hackage...\033[0m"
  read -p "Hackage username: " username
  read -p "Hackage password: " -s password
  echo ""
  curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@$doc.tar.gz" "https://$username:$password@hackage.haskell.org/package/$1-$2/docs"
  exit $?
else
  echo -e "\033[1;31mNot in a stack-powered project\033[0m"
fi
