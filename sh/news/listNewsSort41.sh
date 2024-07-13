#!/bin/bash
#
curl -v '127.0.0.1:4221/news?panigate=%7B"offset"%3A1%2C"limit"%3A17%7D&sort=%7B"columnType"%3A"QuantityImages"%2C"sortOrder"%3A"Descending"%7D'

# {  %7B
# }  %7D
# ,  %2C
# :  %3A
# =  %3D
