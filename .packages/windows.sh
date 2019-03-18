#!/usr/bin/env bash
set -e

## Only deploy gcc matrix item
if [ "$CC" == "gcc" ]; then

#   if [ "$TRAVIS_BRANCH" == "dev" ] || [ "$TRAVIS_BRANCH" == "master" ]; then
  if [ "$TRAVIS_BRANCH" == "dev" ]; then
    ## Grab the version from package.yml
    VERSION=`grep -r version: package.yaml | cut -d "'" -f2`

    if [ "$TRAVIS_BRANCH" == "dev" ]; then
      FILE_VERSION=$VERSION-prerelease
      REPO_NAME=kitlang-prerelease
    fi

    if [ "$TRAVIS_BRANCH" == "master" ]; then
      FILE_VERSION=$VERSION
      REPO_NAME=kitlang-stable
    fi

    ## Create the Windows zip
    stack install --local-bin-path . kitlang:kitc
    zip -r kitlang_${FILE_VERSION}-${TRAVIS_BUILD_NUMBER}_x86_64.zip kitc.exe std toolchains README.md LICENSE.md LICENSE-RUNTIME.md helloworld.kit

    ## Deploy the zip file
    curl --show-error --fail -T kitlang_${FILE_VERSION}-${TRAVIS_BUILD_NUMBER}_x86_64.zip -ubendmorris:$BINTRAY_API_KEY \
      -H "X-Bintray-Publish: 1" \
      https://api.bintray.com/content/kitlang/$REPO_NAME-windows/kitlang/$VERSION/kitlang_${FILE_VERSION}-${TRAVIS_BUILD_NUMBER}_x86_64.zip
  fi
fi
