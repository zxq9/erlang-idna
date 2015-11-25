#!/bin/sh
set -e

SCRIPT=$(readlink $0 || true)
if [ -z $SCRIPT ]; then
    SCRIPT=$0
fi;
SCRIPT_DIR="$(cd `dirname "$SCRIPT"` && pwd -P)"


CURL_BIN=`which curl`
if ! test -n "CURLBIN"; then
    echo "Error: curl is required. Add it to 'PATH'"
    exit 1
fi


DATA_FILE=UnicodeData.txt
DATA_URL=http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
DATA1_OUT=src/idna_unicode_data1.erl
DATA2_OUT=src/idna_unicode_data2.erl

# fetch data file
#$CURL_BIN -o $DATA_FILE $DATA_URL

# create the first module
cat <<EOF > $DATA1_OUT
-module(idna_unicode_data1).
-export([decomposition/1]).

EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($6!=""){ printf("decomposition(\"%s\") -> \"%s\";\n", $6, $1)}}' \
    | sort \
    | uniq -w 25 \
    >> $DATA1_OUT
echo "decomposition(_) -> false." >> $DATA1_OUT
echo "" >> $DATA1_OUT


# create the second module
cat <<EOF > $DATA2_OUT
-module(idna_unicode_data2).
-export([lookup1/1]).

EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($1!=""){printf("lookup1(\"%s\") -> {\"%s\",\"%s\",\"%s\"};\n", $1, $4, $6, $14)}}' \
    | sort \
    | uniq -w 25 \
	>> $DATA2_OUT
echo "lookup1(_) -> false." >> $DATA2_OUT
echo "" >> $DATA2_OUT


