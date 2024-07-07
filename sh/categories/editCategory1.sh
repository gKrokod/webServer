#!/bin/bash
#
curl -v -X POST 127.0.0.1:4221/categories/edit -H "Content-Type: application/json" -d '{"label":"Man","newlabel":"NewMan","newparent":"Woman"}'
# curl -v -X POST 127.0.0.1:4221/categories/edit -H "Content-Type: application/json" -d '{"label":"Man"}'
# curl -v -X POST 127.0.0.1:4221/categories/edit -H "Content-Type: application/json" -d '{"label":"Man","newlabel":"NewMan"}'
# curl -v -X POST 127.0.0.1:4221/categories/edit -H "Content-Type: application/json" -d '{"label":"Man","newparent":"Woman"}'
