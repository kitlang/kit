#!/usr/bin/env bash

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 379CE192D401AB61
echo "deb https://dl.bintray.com/kitplummer/kit bionic universe" | sudo tee -a /etc/apt/sources.list
sudo apt update
apt search kitlang
sudo apt install kitlang
kitc --version