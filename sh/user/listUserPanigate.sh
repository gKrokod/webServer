#!/bin/bash
#
# curl -v 127.0.0.1:4221/users?limit=10&offset=1
curl '127.0.0.1:4221/users?limit=10&offset=1&panigate={"offset"%3A0%2C"limit"%3A10}'


# curl -v -X POST 127.0.0.1:4221/users/create -H "Content-Type: application/json" -d '{"isAdmin":true,"isPublisher":true,"login":"Дагер","name":"Петр","pas1sword":"qwerty", "hoh":"hoho"}'
