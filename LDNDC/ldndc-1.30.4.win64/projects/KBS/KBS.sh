#!/bin/bash

if [ -e $HOME/.ldndc ]; 
then
    #LandscapeDNDC program
    ldndc="../../bin/ldndc"

    #Target project
    project="./KBS.ldndc"

    #Run target project
    $ldndc $project
else
printf "Directory \"~/.ldndc\" missing!\n\
Did you install LandscapeDNDC via \"install.sh\"?\n"
fi
    
