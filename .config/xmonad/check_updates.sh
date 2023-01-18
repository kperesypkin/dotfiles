#!/usr/bin/env bash

cupd=$(apt-get -q -y --ignore-hold --allow-change-held-packages --allow-unauthenticated -s dist-upgrade | grep  ^Inst | wc -l)
echo "$cupd updates "
