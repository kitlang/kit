#!/usr/bin/env bash
set -e

## Only deploy gcc matrix item
if [ "$CC" == "gcc" ]; then

  # We need FPM
  gem install fpm --no-ri --no-rdoc

  ## Grab the version from package.yml
  VERSION=`grep -r version: package.yaml | cut -d "'" -f2`

  if [ "$TRAVIS_BRANCH" == "dev" ]; then
    FILE_VERSION=$VERSION~prerelease
  fi

  if [ "$TRAVIS_BRANCH" == "master" ]; then
    FILE_VERSION=$VERSION
  fi

  ## Create the Apt package
  fpm -s dir -t deb -n kitlang -v $FILE_VERSION-$TRAVIS_BUILD_NUMBER \
    --description "Kit is a programming language designed for creating concise, high performance cross-platform applications." \
    --license "LGPLv3.0" \
    --vendor "kitlang.org" \
    --maintainer "Ben Morris <ben@bendmorris.com>" \
    --url "https://kitlang.org" \
    -d "gcc | clang" \
    ~/.local/bin/kitc=/usr/bin/kitc std/=/usr/lib/kit

  ## Create the RPM package
  fpm -s dir -t rpm -n kitlang -v $FILE_VERSION --iteration $TRAVIS_BUILD_NUMBER \
    --description "Kit is a programming language designed for creating concise, high performance cross-platform applications." \
    --license "LGPLv3.0" \
    --vendor "kitlang.org" \
    --maintainer "Ben Morris <ben@bendmorris.com>" \
    --url "https://kitlang.org" \
    -d "gcc" \
    ~/.local/bin/kitc=/usr/bin/kitc std/=/usr/lib/kit

  ## Deploy the Apt package
  curl -T kitlang_${FILE_VERSION}-${TRAVIS_BUILD_NUMBER}_amd64.deb -ukitplummer:$BINTRAY_API_KEY \
    -H "X-Bintray-Publish: 1" \
    -H "X-Bintray-Debian-Distribution: trusty,xenial,bionic" \
    -H "X-Bintray-Debian-Component: universe" \
    -H "X-Bintray-Debian-Architecture: amd64" \
    https://api.bintray.com/content/kitplummer/kit/kitlang/$VERSION/kitlang_${FILE_VERSION}-${TRAVIS_BUILD_NUMBER}_amd64.deb

  ## Deploy the RPM package
  curl -vvv -T kitlang-${FILE_VERSION}-$TRAVIS_BUILD_NUMBER.x86_64.rpm \
    -H "X-Bintray-Publish: 1" \
    -ukitplummer:$BINTRAY_API_KEY https://api.bintray.com/content/kitplummer/kit-rpm/kitlang/$VERSION/kitlang-${FILE_VERSION}-$TRAVIS_BUILD_NUMBER.x86_64.rpm

fi
