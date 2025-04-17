function fish_prompt

    # interactive user name @ host name, date/time in YYYY-mm-dd format and path
    echo (whoami)@(hostname) (basename (prompt_pwd)) "% "

end
