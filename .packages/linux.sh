#!/usr/bin/env bash
set -e

## We need FPM
gem install fpm

## Grab the version
VERSION=`grep -r version: package.yaml | cut -d "'" -f2`

## Create the Apt package
fpm -s dir -t deb -n kitlang -v $VERSION-$CC-latest \
  --description "Kit is a programming language designed for creating concise, high performance cross-platform applications." \
  --license "LGPLv3.0" \
  --vendor "kitlang.org" \
  --maintainer "Ben Morris <ben@bendmorris.com>" \
  --url "https://kitlang.org" \
  -d "$CC" \
  ~/.local/bin/kitc=/usr/bin/kitc std/=/usr/lib/kit

## Create the RPM package
fpm -s dir -t rpm -n kitlang -v $VERSION-$CC-latest --iteration $TRAVIS_BUILD_NUMBER \
  --description "Kit is a programming language designed for creating concise, high performance cross-platform applications." \
  --license "LGPLv3.0" \
  --vendor "kitlang.org" \
  --maintainer "Ben Morris <ben@bendmorris.com>" \
  --url "https://kitlang.org" \
  -d "$CC" \
  ~/.local/bin/kitc=/usr/bin/kitc std/=/usr/lib/kit

## Deploy the Apt package
curl -T kitlang_$VERSION-$CC-latest_amd64.deb -ukitplummer:$BINTRAY_API_KEY \
  -H "X-Bintray-Publish: 1" -H "X-Bintray-Override: 1" \
  -H "X-Bintray-Debian-Distribution: trusty,xenial,bionic" \
  -H "X-Bintray-Debian-Component: universe" \
  -H "X-Bintray-Debian-Architecture: amd64" \
  https://api.bintray.com/content/kitplummer/kit/kitlang/0.1.0/kitlang_$VERSION-$CC-latest_amd64.deb

## Deploy the RPM package
curl -vvv -T kitlang-$VERSION_$CC_latest-$TRAVIS_BUILD_NUMBER.x86_64.rpm \
  -H "X-Bintray-Publish: 1" -H "X-Bintray-Override: 1" \
  -ukitplummer:$BINTRAY_API_KEY https://api.bintray.com/content/kitplummer/kit-rpm/kitlang/$VERSION/kitlang-$VERSION_$CC_latest-$TRAVIS_BUILD_NUMBER.x86_64.rpm
