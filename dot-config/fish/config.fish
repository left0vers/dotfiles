if status is-interactive
    # Commands to run in interactive sessions can go here
end

# `fish_add_path` is a provided function that takes care of adding the provided
# path if they are not already included.
if [ (uname) = Darwin ]
    fish_add_path /opt/homebrew/bin
end

fish_add_path ~/.local/bin
fish_add_path ~/.cargo/bin

# Load the `starship` shell
starship init fish | source

if [ -f ~/.docker/init-fish.sh ]
    source ~/.docker/init-fish.sh || true # Added by Docker Desktop
end

# pyenv setup
# https://github.com/pyenv/pyenv
set -U PYENV_ROOT $HOME/.pyenv
fish_add_path $PYENV_ROOT/bin
pyenv init - | source
# pyenv virtualenv plugin, documentation can be found here:
# https://github.com/pyenv/pyenv-virtualenv
status --is-interactive; and pyenv virtualenv-init - | source
