#!/bin/bash
#
curl -v -X POST login1:qpass1@127.0.0.1:4221/users/create -H "Content-Type: application/json" -d '{"isAdmin":true,"isPublisher":true,"login":"Дагер","name":"Петр","password":"qwerty"}'
