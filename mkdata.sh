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
DATA_OUT=src/idna_unicode_data.erl
# fetch data file

if [ ! -e "$DATA_FILE" ]; then
    $CURL_BIN -o $DATA_FILE $DATA_URL
fi

cat <<EOF > $DATA_OUT
-module(idna_unicode_data).
-export([lookup/1, decomposition/1]).

lookup("") -> false;
EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($1!=""){ printf("lookup(\"%s\") -> {\"%s\",\"%s\",\"%s\"};\n", $1, $4, $6, $14) }};' \
    | sort \
    | uniq -w 25 \
    >> $DATA_OUT
echo "lookup(_) -> false." >> $DATA_OUT


cat <<EOF >> $DATA_OUT
decomposition("") -> false;
EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($6!=""){ printf("decomposition(\"%s\") -> \"%s\";\n", $6, $1) }};' \
    | sort \
    | uniq -w 25 \
    >> $DATA_OUT
echo "decomposition(_) -> false." >> $DATA_OUT
