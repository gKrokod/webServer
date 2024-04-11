#!/bin/bash
#
# DON'T EDIT THIS!
#
# CodeCrafters uses this file to test your code. Don't make any changes here!
#
# DON'T EDIT THIS!
# exec stack run --silent -- "$@"
curl -v -X POST 127.0.0.1:4221/users/create -H "Content-Type: application/json" -d '{"isAdmin":true,"isPublisher":true,"lin":"Дагер","name":"Петр","passw":"qwerty"}'
