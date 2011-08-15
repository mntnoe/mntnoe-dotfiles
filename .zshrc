#!/bin/zsh
#  File:     .zshrc
#  Author:   Mads N Noe <mail (@) madsnoe.dk>
#  Modified: 2011-03-12

# NON-INTERACTIVE SHELLS {{{
_source () { [[ -f $1 ]] && source $1 }
_source ~/.zsh/non-interactive

[[ $HOSTNAME == mntnoe-server && $UID != 0 ]] &&
    umask 027 ||
    umask 022

# Test for an interactive shell.
[[ -t 1 ]] || return
# }}}

# SETTINGS {{{
WORDCHARS="${WORDCHARS:s#/#}"
KEYTIMEOUT=5

# flow control
stty -ixon -ixoff

# history
HISTSIZE=4000
HISTFILE=$HOME/.zsh_history
SAVEHIST=4000
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt incappendhistory

# cd
setopt autocd
setopt autopushd
setopt pushdsilent

# prompt
setopt extendedglob
setopt nolistambiguous
setopt prompt_subst
setopt nobeep

# job
setopt nohup
setopt nocheckjobs
setopt nobgnice
# }}}

# IMPORTS {{{
_source ~/.zsh/aliases
_source ~/.zsh/local
_source ~/.zsh/bookmarks
_source ~/.zsh/spectrum
_source ~/.zsh/meta
_source ~/.zsh/screen
[[ "$UID" != "0" ]] && _source ~/.zsh/subversion
# }}}

# COMPLETION {{{
autoload -U compinit && compinit

zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' menu select

# case-insensitive -> partial-word -> substring completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

eval $(dircolors)
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# }}}

# PROMPT {{{
_if_viins () echo '${${KEYMAP/vicmd/ }/(main|viins)/'"$1"'}'

setprompt () {
    local host cwd_color cwd char char2 char_fmt char2_fmt
    local laptop_color char_fx

    if [[ $XTERM_THEME == "light" ]] {
        laptop_color=$FG[166]
        char_fx=$FX[bold]
    } else {
        laptop_color=$FG[215]
        char_fx=
    }

    # HOST NAME
    case "$HOSTNAME" in
        mntnoe-server)  host="%{$fg[magenta]%}server" ;;
        mntnoe-laptop)  host="%{$laptop_color%}laptop"   ;;
        mntnoe-netbook) host="%{$fg[yellow]%}netbook" ;;
        *)              host="%{$NORM%}$HOSTNAME" ;;
    esac

    # WORKING DIR
    case $USER in
        root)   cwd_color="%{$fg[red]%}"   ;;
        mntnoe) cwd_color="%{$fg[green]%}" ;;
        *)      cwd_color="%{$fg[white]%}" ;;
    esac

    # CHAR
    case $USER in
        root) char='#'  ;;
        *)    char='%%' ;;
    esac

    char_fmt="%{$char_fx$fg[blue]%}`_if_viins $char`"
    char2_fmt="%{$char_fx$fg[blue]%}`_if_viins \>`"

    PROMPT="$host $cwd_color%3d $char_fmt %{$NORM%}"
    PROMPT2="$char2_fmt %{$NORM%}"
}; setprompt

zle-keymap-select () zle reset-prompt
zle -N zle-keymap-select
# }}}

# TITLE {{{

# screen
precmd() {
    # Reset prompt colors.  zle-line-init can't be used, as it is  executed
    # only after the first keypress.
    KEYMAP=main

    local PREFIX=
    [[ "$UID" == 0 ]] && PREFIX="root: "

    case $TERM in
        *xterm*|rxvt*|screen*) print -Pn "\e]2;$PREFIX%~ ($TERM)\a"
        ;;
    esac

    _screen-precmd
}

preexec () _screen-preexec $*
# }}}

# ZLE {{{
go-up () {
    cd ..
    zle reset-prompt
}; zle -N go-up

go-back () {
    popd
    zle reset-prompt
}; zle -N go-back

list-dir () {
    echo
    ls
    zle reset-prompt
}; zle -N list-dir

list-dir-long () {
    echo
    ls -l
    zle reset-prompt
}; zle -N list-dir-long

last-cmd () {
    zle up-line-or-history
    zle accept-line
}; zle -N last-cmd

last-cmd2 () {
    zle up-line-or-history
    zle up-line-or-history
    zle accept-line
}; zle -N last-cmd2

go-to-bookmark () {
    cd $bookmark[$KEYS[-1]]
    zle reset-prompt
}; zle -N go-to-bookmark

vi-change-whole-line () { 
    zle kill-whole-line
    zle vi-insert 
}; zle -N vi-change-whole-line

spawn-xterm () (
    xterm &
); zle -N spawn-xterm

# }}}

# BINDKEY {{{

bindkey -N mntnoe emacs

bindkey -M mntnoe    "^["          vi-cmd-mode
bindkey -M vicmd  -s ""          ""

# movement
bindkey -M mntnoe    "$m_[h]"      backward-char
bindkey -M mntnoe    "$m_[l]"      forward-char
bindkey -M mntnoe    "$m_[b]"      backward-word
bindkey -M mntnoe    "$m_[w]"      forward-word
bindkey -M vicmd     "n"           vi-add-eol
bindkey -M vicmd  -s "t"           "0cw"
bindkey -M vicmd  -s "o"           "0f-l"
bindkey -M vicmd  -s "O"           "0f-dW"

# editing
bindkey -M mntnoe    "ˆ"          backward-kill-word
bindkey -M vicmd     "ˆ"          backward-kill-word
bindkey -M mntnoe    "$m_[-]"      vi-backward-kill-word
bindkey -M vicmd     "$m_[-]"      vi-backward-kill-word
bindkey -M vicmd     ""          vi-backward-delete-char

bindkey -M vicmd     "$m_[u]"      vi-change-whole-line
bindkey -M mntnoe    "$m_[u]"      kill-whole-line
bindkey -M mntnoe    "^U"          kill-whole-line

bindkey -M vicmd     "$m_[\']"     self-insert

bindkey -M mntnoe    "$m_[,]"      copy-prev-shell-word
bindkey -M mntnoe    "$m_[.]"      insert-last-word

bindkey -M mntnoe    "$m_[k]"      up-line-or-history
bindkey -M mntnoe    "$m_[j]"      down-line-or-history

bindkey -M mntnoe    "$m_[\<]"     beginning-of-buffer-or-history
bindkey -M mntnoe    "$m_[\>]"     end-of-buffer-or-history

bindkey -M mntnoe    "^R"          history-incremental-search-backward
bindkey -M mntnoe    "^S"          history-incremental-search-forward

bindkey -M mntnoe    "^[0"         go-to-bookmark
bindkey -M mntnoe    "^[1"         go-to-bookmark
bindkey -M mntnoe    "^[2"         go-to-bookmark
bindkey -M mntnoe    "^[3"         go-to-bookmark
bindkey -M mntnoe    "^[4"         go-to-bookmark
bindkey -M mntnoe    "^[5"         go-to-bookmark
bindkey -M mntnoe    "^[6"         go-to-bookmark
bindkey -M mntnoe    "^[7"         go-to-bookmark
bindkey -M mntnoe    "^[8"         go-to-bookmark
bindkey -M mntnoe    "^[9"         go-to-bookmark

bindkey -M mntnoe -s "$m_[0]"      "^[0"
bindkey -M mntnoe -s "$m_[1]"      "^[1"
bindkey -M mntnoe -s "$m_[2]"      "^[2"
bindkey -M mntnoe -s "$m_[3]"      "^[3"
bindkey -M mntnoe -s "$m_[4]"      "^[4"
bindkey -M mntnoe -s "$m_[5]"      "^[5"
bindkey -M mntnoe -s "$m_[6]"      "^[6"
bindkey -M mntnoe -s "$m_[7]"      "^[7"
bindkey -M mntnoe -s "$m_[8]"      "^[8"
bindkey -M mntnoe -s "$m_[9]"      "^[9"

# selection
bindkey -M mntnoe    "^@"          set-mark-command
bindkey -M mntnoe    "^Y"          yank
bindkey -M mntnoe    "$m_[y]"      yank-pop
bindkey -M mntnoe    "^X^W"        copy-region-as-kill

# accepting
bindkey -M mntnoe    "$m_[n]"      accept-line
bindkey -M mntnoe    "^M"          accept-line
bindkey -M mntnoe    "[27;2;13~" accept-line
bindkey -M mntnoe    "^J"          accept-line
bindkey -M mntnoe    "^X^J"        accept-line-and-down-history
bindkey -M mntnoe    "^X^M"        accept-and-hold
bindkey -M mntnoe    "$m_[f]"      push-line

bindkey -M mntnoe -s "$m_[o]"      "&"
bindkey -M mntnoe -s "$m_[m]"      "&exit"

bindkey -M mntnoe    "$m_[i]"      list-dir
bindkey -M mntnoe    "$m_[I]"      list-dir-long

# misc
bindkey -M mntnoe    "$m_[e]"      complete-word
bindkey -M mntnoe    "^I"          complete-word
bindkey -M mntnoe    "^X^E"        expand-word

bindkey -M mntnoe    "^_"          undo
bindkey -M vicmd     "u"           undo

bindkey -M mntnoe    "^L"          clear-screen

bindkey -M vicmd     ":"           execute-named-cmd

bindkey -M vicmd     "^X^K"        run-help

bindkey -M mntnoe -s "$m_[q]"      ""
bindkey -M vicmd  -s "q"           "i"
bindkey -M vicmd  -s "$m_[q]"      "i"

bindkey -M mntnoe    "$m_[p]"      go-up
bindkey -M mntnoe    "$m_[t]"      go-back
bindkey -M mntnoe    "$m_[r]"      last-cmd
bindkey -M mntnoe    "$m_[R]"      last-cmd2

bindkey -M mntnoe    "[24~"      spawn-xterm

case "$TERM" in
    (*xterm*|rxvt*|screen*) 
        bindkey -A mntnoe main
    ;;
esac

for map in mntnoe vicmd; do
    case "$TERM" in
        (*xterm*)
            bindkey -M $map "^[[7~"     beginning-of-line
            bindkey -M $map "^[[4~"     end-of-line
            bindkey -M $map "^[[3~"     delete-char
            bindkey -M $map "[5~"     backward-word
            bindkey -M $map "[6~"     forward-word
        ;;
        (rxvt*) 
            bindkey -M $map "^[[7~"     beginning-of-line
            bindkey -M $map "^[[8~"     end-of-line
            bindkey -M $map "^[[3~"     delete-char
        ;;
        (screen*) 
            bindkey -M $map "^[[7~"     beginning-of-line
            bindkey -M $map "^[[4~"     end-of-line
            bindkey -M $map "^[[3~"     delete-char
            bindkey -M $map "[5~"     backward-word
            bindkey -M $map "[6~"     forward-word
            bindkey -M $map "[3;2~"   delete-word
        ;;
    esac
done
# }}}

#  vim: set ft=zsh fdm=marker fdl=1 fdc=2:
