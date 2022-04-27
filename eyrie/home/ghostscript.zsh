# SPDX-FileCopyrightText: 2022 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

gsgetpages()
{
    # @arg1 : first page of the range to extract
    # @arg2 : last page of the range to extract
    # @arg3 : input file
    gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=${1} -dLastPage=${2} -sOutputFile=temp.pdf ${3}
}
