#!/bin/sh

VS_CODE_DIR=""

if command -v code
then
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
    stow -v -d VSCode/ -t "${VS_CODE_DIR}" --ignore="txt" -S .

    while read -r extension; do
        code --install-extension "${extension}" --force
    done < "${VS_CODE_DIR}/extensions.txt"
fi

stow -v --ignore="VSCode" --ignore="install.sh" -v .
