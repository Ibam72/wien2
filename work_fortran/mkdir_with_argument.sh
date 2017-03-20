#!/bin/bash

CHK_DIR=$1

if [ ! -d ${CHK_DIR} ] ;then
	mkdir ${CHK_DIR}
fi