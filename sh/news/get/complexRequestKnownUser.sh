#!/bin/bash
#
curl -v 'login1:qpass1@127.0.0.1:4221/news?panigate=%7B"offset"%3A1%2C"limit"%3A7%7D&filter=%5B%7B"contents"%3A"2023-01-01"%2C"tag"%3A"FilterDataSince"%7D%2C%7B"contents"%3A"user"%2C"tag"%3A"FilterTitleFind"%7D%5D&sort=%7B"columnType"%3A"QuantityImages"%2C"sortOrder"%3A"Ascending"%7D&find=%7B"subString"%3A"and"%7D'


# {  %7B
# }  %7D
# ,  %2C
# :  %3A
# =  %3D
# [  %5B
# ]  %5D
