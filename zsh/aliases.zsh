## Show hidden files ##
alias l.='ls -d .* --color=auto'

## Colorize the grep and ls command output for ease of use (good for log files)##
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias diff='colordiff'

# ls aliases
alias ll='ls -lah'
alias la='ls -A'
alias l='ls'

#confirmation
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias ln='ln -i'

# Update dotfiles
function dfu() {
    (
        cd ~/.dotfiles && git pullff && ./install -q
    )
}


# Create a directory and cd into it
function mcd() {
    mkdir "${1}" && cd "${1}"
}


# Jump to directory containing file
function jump() {
    cd "$(dirname ${1})"
}

# Go up [n] directories
function up()
{
    local cdir="$(pwd)"
    if [[ "${1}" == "" ]]; then
        cdir="$(dirname "${cdir}")"
    elif ! [[ "${1}" =~ ^[0-9]+$ ]]; then
        echo "Error: argument must be a number"
    elif ! [[ "${1}" -gt "0" ]]; then
        echo "Error: argument must be positive"
    else
        for i in {1..${1}}; do
            local ncdir="$(dirname "${cdir}")"
            if [[ "${cdir}" == "${ncdir}" ]]; then
                break
            else
                cdir="${ncdir}"
            fi
        done
    fi
    cd "${cdir}"
}

#cd aliases
alias ..='cd ..'
alias ...='cd ../..'

# Execute a command in a specific directory
function in() {
    (
        cd ${1} && shift && ${@}
    )
}


# if user is not root, pass all commands via sudo #
if [ $UID -ne 0 ]; then
    alias reboot='sudo reboot'
    alias update='sudo apt-get update && sudo apt-get upgrade'
fi

alias home='cd ${HOME}'

# This is GOLD for finding out what is taking so much space on your drives!
alias diskspace="du -S | sort -n -r |more"

alias bc='bc -l'


alias mount='mount |column -t'

alias ports='netstat -tulanp'

alias wget='wget -c'

alias pid='ps -aux | grep'

alias mterm='/opt/madis/src/mterm.py' 

alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""'
