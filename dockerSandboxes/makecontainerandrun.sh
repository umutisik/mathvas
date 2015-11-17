#!/bin/sh
# usage: enter the file name without extension for the first argument
#                  the folder the file is in including the / sign!
# e.g. makecontainerandrun.sh helloworld /home/username/

FILENAME="$1"
FILEFOLDER="$2"
HASIMAGES="$3"
CNAME=$FILENAME

eval "$(docker-machine env default)"

docker run -d -it --name $CNAME umutcoderunner/haskell bash > /dev/null
docker cp $FILEFOLDER/hsfiles/$FILENAME.hs $CNAME:/home/umutcoderunner/
docker exec $CNAME runghc /home/umutcoderunner/$FILENAME.hs $FILENAME

#special for images
if [ "$HASIMAGES" == "True" ]
then
  docker exec $CNAME mogrify -format jpeg /home/umutcoderunner/$FILENAME.bmp
  docker cp $CNAME:/home/umutcoderunner/$FILENAME.jpeg $FILEFOLDER/outputimages/
  docker exec $CNAME rm /home/umutcoderunner/$FILENAME.bmp
fi

docker stop $CNAME > /dev/null
docker rm $CNAME > /dev/null

rm $FILEFOLDER/hsfiles/$FILENAME.hs 


