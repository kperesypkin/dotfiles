#!/usr/bin/env bash

if [[ $EUID -ne 0 ]]; then
    echo "This script must be run as root"
    exit 1

else
    # Update & upgrade
    echo "Updating and Upgrading"
    sudo apt update && sudo apt upgrade -y

    # Install packages
    echo "Installing packages"
    sudo apt install -y \
         vim \
         vifm \
         emacs \
         git \
         rxvt-unicode \
         fonts-hack \
         fonts-firacode \
         virtualenvwrapper \
         # firefox-esr \
         # firefox-esr-locale-ru

fi

