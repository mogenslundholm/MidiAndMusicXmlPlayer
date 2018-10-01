#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling midimusicxml
/usr/bin/as -o MidiMusicXml.o MidiMusicXml.s -arch i386
if [ $? != 0 ]; then DoExitAsm midimusicxml; fi
rm MidiMusicXml.s
echo Linking MidiMusicXml
OFS=$IFS
IFS="
"
/usr/bin/ld -framework Carbon -framework OpenGL -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib     -multiply_defined suppress -L. -o MidiMusicXml `cat link.res` -pagezero_size 0x10000
if [ $? != 0 ]; then DoExitLink MidiMusicXml; fi
IFS=$OFS
