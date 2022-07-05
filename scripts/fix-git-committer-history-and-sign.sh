#!/bin/sh

# Snippet from: https://stackoverflow.com/questions/750172/how-to-change-the-author-and-committer-name-and-e-mail-of-multiple-commits-in-gi
git filter-branch --env-filter '
OLD_EMAIL="test@test.com"
CORRECT_NAME="Áquila Freitas"
CORRECT_EMAIL="aquilacf@protonmail.com"
if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags

# Snippet from: https://superuser.com/questions/397149/can-you-gpg-sign-old-commits
git rebase --exec 'git commit --amend --no-edit -n -S' -i --root
