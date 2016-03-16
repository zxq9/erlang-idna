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
DATA_OUT=src/idna_unicode_data.hrl
# fetch data file

if [ ! -e "$DATA_FILE" ]; then
    $CURL_BIN -o $DATA_FILE $DATA_URL
fi

cat <<EOF > $DATA_OUT
-define(BY_CODE, #{
EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($1!=""){ printf("\"%s\" => { \"%s\", \"%s\", \"%s\"}\n", $1, $4, $6, $14) }};' \
    | sort \
    | uniq -w 25 \
    | awk '{print t $0;}; {t = ","} ' \
    >> $DATA_OUT
echo "})." >> $DATA_OUT


cat <<EOF >> $DATA_OUT
-define(BY_KEY, #{
EOF
cat $DATA_FILE \
    | awk 'BEGIN{FS=";"}{if($6!=""){ printf("\"%s\" => \"%s\"\n", $6, $1) }};' \
    | sort \
    | uniq -w 25 \
    | awk '{print t $0;}; {t = ","} ' \
    >> $DATA_OUT
echo "})." >> $DATA_OUT
