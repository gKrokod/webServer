#!/bin/bash
#
# DON'T EDIT THIS!
#
# CodeCrafters uses this file to test your code. Don't make any changes here!
#
# DON'T EDIT THIS!
# exec stack run --silent -- "$@"
curl -i 127.0.0.1:4221/ # if path = / then 200ok
curl -i 127.0.0.1:4221/index200.html # 404 error
curl -v 127.0.0.1:4221/echo/MSGtoYOU # 200ok+ MSGtoYOU in Body+ content headers
