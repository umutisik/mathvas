#!/bin/sh
# usage: enter the file name without extension for the first argument
#                  the folder the file is in including the / sign!
# e.g. stopcontainer.sh helloworld /home/username/
#docker stop $CNAME > /dev/null

FILENAME="$1"
FILEFOLDER="$2"
CNAME=$FILENAME

eval "$(docker-machine env default)"

docker stop $CNAME
docker rm $CNAME 
rm $FILEFOLDER/outputimages/$FILENAME.jpeg




