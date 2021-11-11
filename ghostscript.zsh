gsgetpages()
{
    # @arg1 : first page of the range to extract
    # @arg2 : last page of the range to extract
    # @arg3 : input file
    gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=${1} -dLastPage=${2} -sOutputFile=temp.pdf ${3}
}
