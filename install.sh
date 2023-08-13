#!/bin/sh

VS_CODE_DIR=""

case $(uname) in
    'Darwin')
    VS_CODE_DIR="${HOME}/Library/Application Support/Code/User"
    ;;
    'Linux')
    VS_CODE_DIR="${HOME}/.config/Code/User/"
    ;;
    *)
    echo "Possibly unsupported operating system < $(uname) >. Aborting."
    exit
    ;;
esac

stow --ignore="VSCode" --ignore="install.sh" -v .
stow -d VSCode/ -t "${VS_CODE_DIR}" -S .
