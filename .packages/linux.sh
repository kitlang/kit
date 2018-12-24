#!/bin/#!/usr/bin/env bash
set -e

gem install fpm

fpm -s dir -t deb -n kitlang -v 0.1.0-$CC-latest \
  -d "$CC" \
  ~/.local/bin/kitc=/usr/bin/kitc std/=/usr/lib/kit

curl -T kitlang_0.1.0-$CC-latest_amd64.deb -ukitplummer:$BINTRAY_API_KEY https://api.bintray.com/content/kitplummer/kit/kitlang/0.1.0/kitlang_0.1.0-$CC-latest_amd64.deb;deb_component=universe;deb_architecture=amd64
