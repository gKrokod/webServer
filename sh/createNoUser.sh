#!/bin/bash
#
curl -v -X POST 127.0.0.1:4221/users/create -H "Content-Type: application/json" -d '{"isAdmin":true,"isPublisher":true,"lin":"Дагер","name":"Петр","passw":"qwerty"}'
