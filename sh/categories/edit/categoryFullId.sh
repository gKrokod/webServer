#!/bin/bash
#
curl -v -X POST login1:qpass1@127.0.0.1:4221/categories/AdditionalTask/edit -H "Content-Type: application/json" -d '{"uid":2,"newlabel":"NewMan","newparent":"Woman"}'
