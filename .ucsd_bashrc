# .bash_profile

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

#PATH=/home/obotvinnik/bin/jre1.7.0_07/bin:/home/obotvinnik/packages/java-genomics-toolkit:/home/obotvinnik/packages/epd-7.3-2-rh3-x86_64/bin:$HOME/bin:$PATH:$HOME/packages/weblogo

#PATH=$PATH:/home/obotvinnik/packages/homer/.//bin/

#export PATH

alias 'ucsd-git-pull'='env GIT_SSL_NO_VERIFY=true git pull origin master'
alias 'ucsd-git-push'='env GIT_SSL_NO_VERIFY=true git push origin master'

#export PATH

eval "`dircolors -b ~/.dircolors_test`"
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -lha' 
alias l='ls -CF' # .bashrc

# Ipython alias with static port. `&` indicates that the process should run in the background
alias ipy='ipython notebook --port 8988 &'

# install python modules cleanly
alias pycleanbuildinstall='python setup.py clean ; python setup.py build ; python setup.py install'

alias rotation='cd ~/rotations/q3-spring-pevzner'
alias qstato='qstat -u obotvinnik'
alias showqo='showq -u obotvinnik'
alias conf='./configure --prefix /home/obotvinnik ; make ; make install'

export EMAIL=obotvinn@ucsd.edu




alias vi='emacs' # LOL :)
alias vim='emacs'

# Add my personal pythonpath for python packages
#export PYTHONPATH=$PYTHONPATH
export EDITOR=/usr/bin/emacs

# make sure we have the right samtools
#export PATH=/home/yeo-lab/software/bin/samtools:$PATH
#export PATH=$PATH:/home/yeo-lab/software/apache-maven-3.1.0/bin
#export CLASSPATH=/home/yeo-lab/software/colt/lib/colt.jar:/home/yeo-lab/software/colt/lib/concurrent.jar:$CLASSPATH 

# Activate my virtual environment
#source ~/obot_virtualenv/bin/activate

# Force usage of the virtual environment packages
#export PYTHONPATH=~/obot_virtualenv/lib/python2.7/site-packages/:$PYTHONPATH


alias chmodpy='chgrp -R yeo-group /home/yeo-lab/software/lib/python2.7/site-packages ; chmod -R 775 /home/yeo-lab/software/lib/python2.7/site-packages'


source ~/gscripts/bashrc/tscc_bash_settings_current
source activate olga
#source ~/virtualenvs/envy/bin/activate
#export PATH=~/virtualenvs/envy/bin:$HOME/bin:/projects/ps-yeolab/software/bin:$PATH

# make sure all virtualenv packages get imported and we never use anything from /opt, and use yeolab stuff as a last resort.
#export PYTHONPATH=$(python -c 'import os; print ":".join(set(p for p in os.environ["PYTHONPATH"].split(":") if p and ("opt" not in p and "yeo" not in p)))')
#export PYTHONPATH=$PYTHONPATH:/projects/ps-yeolab/software/lib/python2.7/site-packages

#OLD_LD_LIBRARY_PATH=$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=
#source /opt/intel/composer_xe_2013.1.117/bin/compilervars.sh intel64
#source /opt/intel/composer_xe_2013.1.117/mkl/bin/mklvars.sh intel64
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$OLD_LD_LIBRARY_PATH


alias dropbox="python $(which dropbox.py)"

export IPYNB_PORT=8812
export IPYNB_MINUTES=12
alias sshcluster="ssh obotvinnik@tscc-login1.sdsc.edu -L $IPYNB_PORT:localhost:$IPYNB_PORT"
# added by Anaconda 1.9.1 installer
#export PATH="/projects/ps-yeolab/software/anaconda/bin:~/bin:$PATH"
#export PYTHONPATH=

alias ipynb="ipython notebook --no-browser --port 7700 --matplotlib inline --debug &"
alias sshtscc="ssh -NR 7700:localhost:7700 tscc-login2 &"
