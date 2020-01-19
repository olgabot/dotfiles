#!/usr/bin/env bash

# make sure we have pulled in and updated any submodules
git submodule init
git submodule update

# what directories should be installable by all users including the root user
base=(
    bash
    bin
)

# folders that should, or only need to be installed for a local user
useronly=(
    git
    orgmode
)

specialapps=(
    "${HOME}/Library/Preferences/::iterm2"
)



# run the stow command for the passed in directory ($2) in location $1
stowit() {
    usr=$1
    app=$2
    # -v verbose
    # -R recursive
    # -t target
    stow -v -R -t ${usr} ${app}
}

echo ""
echo "Stowing apps for user: ${whoami}"

# install apps available to local users and root
for app in ${base[@]}; do
    stowit "${HOME}" $app 
done



# install only user space folders
for app in ${useronly[@]}; do
    if [[ ! "$(whoami)" = *"root"* ]]; then
        stowit "${HOME}" $app 
    fi
done

#OLDIFS=$IFS; IFS=','
# install apps with special locations
for index in "${specialapps[@]}"; do
    if [[ ! "$(whoami)" = *"root"* ]]; then
	location="${index%%::*}"
	app="${index##*::}"
	echo $location and $app
        stowit $location $app
    fi
done
#IFS=$OLDIFS

echo ""
echo "##### ALL DONE"
