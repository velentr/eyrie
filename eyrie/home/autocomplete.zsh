# SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

fpath[1,0]=~/.guix-home/profile/share/zsh/site-functions

autoload -U compinit && compinit

setopt completealiases

eval `dircolors -b`
zstyle ':completion:*' list-colors '${(s.:.)LS_COLORS}'
zstyle ':completion:*' menu select eval "$(dircolors -b)"
# guix's completion for git is broken
zstyle ':completion:*:*:git:*' script ~/.guix-home/profile/etc/bash_completion.d/git

if [ -f "${HOME}/aircam/build/python3_venv/bin/register-python-argcomplete" ]; then
	eval "$(${HOME}/aircam/build/python3_venv/bin/register-python-argcomplete --shell zsh skyrun)"
fi
