#!/bin/bash
#
curl -v '127.0.0.1:4221/news?panigate=%7B"offset"%3A2%2C"limit"%3A7%7D&sort=%7B"columnType"%3A"CategoryName"%2C"sortOrder"%3A"Ascending"%7D'

# {  %7B
# }  %7D
# ,  %2C
# :  %3A
# =  %3D
