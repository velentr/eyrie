# SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

autoload -U colors && colors
autoload -U promptinit && promptinit

# custom prompt
function cur_dir_path {
    CURRENT=`dirname ${PWD}`
    if [[ $CURRENT = / ]]; then
        echo ""
    elif [[ $CURRENT = "/home" ]]; then
        echo ""
    else
        CURRENT=$(print -P %~)
        echo "${CURRENT%/*}/"
    fi
}
function dir_name {
    echo "%{$reset_color%}$(cur_dir_path)%{$fg_no_bold[red]%}%1~%{$reset_color%}"
}
function git_stat {
    GIT_STATUS=$(git status --short 2>/dev/null)
    if [ ! -z ${GIT_STATUS} ]; then
        echo " !!"
    fi
}
function git_branch {
    GIT_BRANCH=$(git branch 2>/dev/null | grep '\*' | sed 's/* //')
    if [ ${GIT_BRANCH} ]; then
        echo " %{$reset_color%}on %{$fg_no_bold[red]%}${GIT_BRANCH}%{$reset_color%}$(git_stat)"
    fi
}
function check_return {
    if [ $? != 0 ]; then
        echo "-(%{$fg_no_bold[red]%}%?%{$reset_color%})"
    fi
}
function window_title {
    print -Pn '\e]0;%~\a'
}

PROMPT='
(%{$fg_no_bold[red]%}%m%{$reset_color%})$(check_return) > '
RPROMPT='($(dir_name)$(git_branch)%{$reset_color%})'

setopt prompt_subst
