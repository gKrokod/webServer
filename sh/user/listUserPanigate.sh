#!/bin/bash
#
# curl -v 127.0.0.1:4221/users?limit=10&offset=1
curl '127.0.0.1:4221/users?limit=10&offset=1&panigate=%7B"offset"%3A1%2C"limit"%3A7%7D'


# {  %7B
# }  %7D
# ,  %2C
# :  %3A
# =  %3D


