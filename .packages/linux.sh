#!/bin/#!/usr/bin/env bash
set -e

gem install fpm

fpm -s dir -t deb -n kitlang -v 0.1.0-$CC-latest \
  --description "Kit is a programming language designed for creating concise, high performance cross-platform applications." \
  -d "$CC" \
  ~/.local/bin/kitc=/usr/bin/kitc std/=/usr/lib/kit

curl -vvv -T kitlang_0.1.0-$CC-latest.deb -ukitplummer:$BINTRAY_API_KEY \
  -H "X-Bintray-Publish: 1" -H "X-Bintray-Override: 1" \
  -H "X-Bintray-Debian-Distribution: trusty, xenial, bionic" \
  -H "X-Bintray-Debian-Component: universe" \
  -H "X-Bintray-Debian-Architecture: amd64" \
  https://api.bintray.com/content/kitplummer/kit/kitlang/0.1.0/kitlang_0.1.0-$CC-latest_amd64.deb
