function ll
    if command -v exa >/dev/null
        exa --long --header --icons --tree --level=1 -a $argv
    else
        ls -alh --color=auto $argv
    end
end
