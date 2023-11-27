# SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

# Find the real path to an executable in $PATH.
function realwhich() {
	realpath $(which $1)
}
