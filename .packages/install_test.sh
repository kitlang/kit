#!/usr/bin/env bash

## Grab the version from package.yml
VERSION=`grep -r version: package.yaml | cut -d "'" -f2`

if [ "$TRAVIS_BRANCH" == "dev" ]; then
  FILE_VERSION=$VERSION~prerelease
fi

if [ "$TRAVIS_BRANCH" == "master" ]; then
  FILE_VERSION=$VERSION
fi

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 379CE192D401AB61
echo "deb https://dl.bintray.com/kitplummer/kit bionic universe" | sudo tee -a /etc/apt/sources.list
sudo apt update
apt search kitlang
sudo apt install kitlang=$FILE_VERSION-$TRAVIS_BUILD_NUMBER
kitc --version
