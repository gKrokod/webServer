#!/bin/bash
#
curl -v -X POST 127.0.0.1:4221/users/create/votvot -H "Content-Type: application/json" -d '{"isAdmin":true,"isPublisher":true,"login":"Дагер","name":"Петр","password":"qwerty", "hoh":"hoho"}'
