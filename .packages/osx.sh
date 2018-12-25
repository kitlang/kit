#!/usr/bin/env bash
set -e

gem install fpm

VERSION=`grep -r version: package.yaml | cut -d "'" -f2`

fpm -s dir -t osxpkg -n kitlang -v $VERSION-latest \
  --description "Kit is a programming language designed for creating concise, high performance cross-platform applications." \
  --license "LGPLv3.0" \
  --vendor "kitlang.org" \
  --maintainer "Ben Morris <ben@bendmorris.com>" \
  --url "https://kitlang.org" \
  ~/.local/bin/kitc=/usr/local/bin/kitc std/=/usr/local/lib/kit

curl -vvv -T kitlang_$VERSION-latest.pkg -ukitplummer:$BINTRAY_API_KEY \
  -H "X-Bintray-Publish: 1" -H "X-Bintray-Override: 1" \
  https://api.bintray.com/content/kitplummer/kit-macos/kitlang/0.1.0/kitlang_$VERSION-latest.pkg
