#!/bin/sh

# A script that will change any commits that previously had the old email
# address in its author or committer fields to use the correct name and
# email address.
#
# Source: https://help.github.com/articles/changing-author-info/

git filter-branch --env-filter '

OLD_EMAIL="christian.brommer@jpl.nasa.gov"
CORRECT_NAME="Danylo Malyuta"
CORRECT_EMAIL="danylo.malyuta@gmail.com"

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