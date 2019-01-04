#!/usr/bin/env bash
set -e

## Only deploy gcc matrix item
if [ "$CC" == "gcc" ]; then
  if [ "$TRAVIS_BRANCH" == "dev" ] || [ "$TRAVIS_BRANCH" == "master" ]; then
    gem install fpm

    VERSION=`grep -r version: package.yaml | cut -d "'" -f2`

    if [ "$TRAVIS_BRANCH" == "dev" ]
    then
      FILE_VERSION=$VERSION-prerelease-$TRAVIS_BUILD_NUMBER
    fi

    if [ "$TRAVIS_BRANCH" == "master" ]
    then
      FILE_VERSION=$VERSION-$TRAVIS_BUILD_NUMBER
    fi

    fpm -s dir -t osxpkg -n kitlang -v $FILE_VERSION \
      --description "Kit is a programming language designed for creating concise, high performance cross-platform applications." \
      --license "LGPLv3.0" \
      --vendor "kitlang.org" \
      --maintainer "Ben Morris <ben@bendmorris.com>" \
      --url "https://kitlang.org" \
      ~/.local/bin/kitc=/usr/local/bin/kitc std/=/usr/local/lib/kit

    curl --show-error --fail -vvv -T kitlang-$FILE_VERSION.pkg -ubendmorris:$BINTRAY_API_KEY \
      -H "X-Bintray-Publish: 1" -H "X-Bintray-Override: 1" \
      https://api.bintray.com/content/kitlang/kitlang-macos/kitlang/$VERSION/kitlang-$FILE_VERSION.pkg
  fi
fi
