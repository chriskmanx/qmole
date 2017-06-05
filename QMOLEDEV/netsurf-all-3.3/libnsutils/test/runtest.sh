#!/bin/sh 
TEST_PATH=$1

b64enctst()
{
    ENC=$(echo -n "${1}" | ${TEST_PATH}/test_base64 )
    if [ "${ENC}" != "${2}" ];then
        echo "Base64 encode error ${ENC} != ${2}"
	exit 2
    fi 
}

b64dectst()
{
    DEC=$(echo -n "$1" | ${TEST_PATH}/test_base64 -d )
    if [ "${DEC}" != "$2" ];then
        echo "Base64 decode error ${DEC} != $2"
	exit 3
    fi 
}

b64enctst 'f' 'Zg=='
b64enctst 'fo' 'Zm8='
b64enctst 'foo' 'Zm9v'
b64enctst 'foob' 'Zm9vYg=='
b64enctst 'fooba' 'Zm9vYmE='
b64enctst 'foobar' 'Zm9vYmFy'

b64dectst 'Zg==' 'f'
b64dectst 'Zm8=' 'fo'
b64dectst 'Zm9v' 'foo'
b64dectst 'Zm9vYg==' 'foob'
b64dectst 'Zm9vYmE=' 'fooba'
b64dectst 'Zm9vYmFy' 'foobar'
