#!/usr/bin/env bash

## Grab the version from package.yml
VERSION=`grep -r version: package.yaml | cut -d "'" -f2`

if [ "$TRAVIS_BRANCH" == "dev" ]; then
  FILE_VERSION=$VERSION~prerelease
  echo "deb https://dl.bintray.com/kitplummer/kitlang-prerelease-ubuntu bionic universe" | sudo tee -a /etc/apt/sources.list.d/kitlang-prereleases.list
fi

if [ "$TRAVIS_BRANCH" == "master" ]; then
  FILE_VERSION=$VERSION
  echo "deb https://dl.bintray.com/kitplummer/kitlang-stable-ubuntu bionic universe" | sudo tee -a /etc/apt/sources.list.d/kitlang-stable.list
fi

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 379CE192D401AB61
sudo apt update
apt search kitlang
sudo apt install kitlang=$FILE_VERSION-$TRAVIS_BUILD_NUMBER
kitc --version
