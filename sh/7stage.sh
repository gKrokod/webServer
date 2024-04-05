#!/bin/bash
#
# DON'T EDIT THIS!
#
# CodeCrafters uses this file to test your code. Don't make any changes here!
#
# DON'T EDIT THIS!
curl -i 127.0.0.1:4221/ # if path = / then 200ok
curl -i 127.0.0.1:4221/index200.html # 404 error
curl -i 127.0.0.1:4221/echo/MSGtoYOU_Stage4 # 200ok+ MSGtoYOU in Body+ content headers
curl -i --user-agent "Warp browser_Stage5" 127.0.0.1:4221/user-agent # 200ok + user-agent header in body
curl -v 127.0.0.1:4221/files/README.me # if exist then 200ok + cType + contain on body, else 404 
curl -v 127.0.0.1:4221/files/noREADME.me # if exist then 200ok + cType + contain on body, else 404 
