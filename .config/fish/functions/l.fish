function l
    if command -v exa >/dev/null
        exa --long --header --icons --tree --level=1 $argv
    else
        ls -lh --color=auto $argv
    end
end
