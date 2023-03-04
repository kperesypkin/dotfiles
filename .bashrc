# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Append to the history file, don't overwrite it
shopt -s histappend

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# ~/.bash_variables
if [[ -f ~/.bash_variables ]]; then
	. ~/.bash_variables
fi

# ~/.bash_aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# ~/.bash_profile
if [ -f ~/.bash_profile ]; then
    . ~/.bash_profile
fi

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

if [[ -z "${FORCE_COLOR_PS1}" ]] && [[ "${TERM}" == "linux" ]]; then
	PS1='${debian_chroot:+($debian_chroot)}\u@\H:\w\$ '
else
	set_prompt() {
        local EXIT=$?
        local PSCHAR="▶"
        local OK="✔"
        local ERROR="✘"
		local reset='\[\033[0m\]'
		local red='\[\033[38;5;160m\]'
		local green='\[\033[38;5;148m\]'
		local yellow='\[\033[38;5;221m\]'
		local blue='\[\033[1;38;5;102m\]'
		local magenta='\[\033[38;5;197m\]'
		local cyan='\[\033[38;5;44m\]'
        local dark_cyan='\[\033[38;5;m146\]'
		local white='\[\033[38;5;255m\]'
        local orange='\[\033[38;5;214m\]'
		local bblue='\[\033[38;5;75m\]'

		# first line
		PS1="\[\033[0m\]\n"
        # exit code
		if [ $EXIT != 0 ]; then
			PS1+="\[\033[1;48;5;234;38;5;160m\] ${ERROR}${EXIT}\\033[0m\]"
        else    
            PS1+="\[\033[1;48;5;234;38;5;148m\] ${OK}\\033[0m\]"
		fi
        # datetime 
		PS1+="\[\033[0;48;5;234;38;5;255m\] `date +%H:%M:%S`\[\033[0m\]"
        # user@host
		if [ $(id -u) -eq 0 ]; then
			PS1+="\[\033[1;48;5;234;38;5;160m\] \\u \[\033[0;48;5;234;38;5;255m\]at \[\033[1;48;5;234;38;5;44m\]\\h\[\033[0m\]"
            PSCHAR="\[\033[1;38;5;160m\] #\033[0m\] "
		else
			PS1+="\[\033[1;48;5;234;38;5;148m\] \\u \[\033[0;48;5;234;38;5;255m\]at \[\033[1;48;5;234;38;5;44m\]\\h\[\033[0m\]"
		fi
		# working directory
		PS1+="\[\033[0;48;5;234;38;5;255m\] in \[\033[1;48;5;234;38;5;75m\]\\w\[\033[0m\]"
		# git
		if [[ -n "$(command -v __git_ps1)" ]]; then
			local fstype="$(df --output=fstype . | tail -n +2)"
			if [[ "${fstype}" != *"fuse.sshfs"* ]]; then
				GIT_PS1_SHOWDIRTYSTATE=true
				GIT_PS1_SHOWUNTRACKEDFILES=true
                GIT_PS1_SHOWSTASHSTATE=true
                GIT_PS1_SHOWCOLORHINTS=true
                GIT_PS1_SHOWUPSTREAM="auto"
				PS1+=$(__git_ps1 "\[\033[1;38;5;197m\] (%s)\[\033[0m\]")
			fi
		fi
		# line two
		PS1+="\n${PSCHAR} "

		# ssh
		if [[ -n "${SSH_CLIENT}" ]]; then
			PS1+="(\[\033[1;48;5;234;38;5;148m\]SSH\[\033[0m\])- "
		fi
		# chroot
		if [[ -n "${debian_chroot}" ]]; then
			PS1+="('\[\033[38;5;255m\]'${debian_chroot}\[\033[0m\])- "
		fi
		# venv
		if [[ -n "$VIRTUAL_ENV" ]]; then
			PS1+="(\[\033[38;5;214m\]${VIRTUAL_ENV##*/}\[\033[0m\])─ "
			VIRTUAL_ENV_DISABLE_PROMPT=1
		fi
		# $ for user, # for root
		# PS1+="${white}\\\$${reset} "
	}

	if [[ -n "${PROMPT_COMMAND}" ]]; then
		PROMPT_COMMAND="set_prompt; ${PROMPT_COMMAND}"
	else
		PROMPT_COMMAND="set_prompt"
	fi
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
    else
    color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
