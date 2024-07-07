#!/bin/bash
#
curl -v -X POST 127.0.0.1:4221/categories/create -H "Content-Type: application/json" -d '{"label":"NewAbstract","parent":null}'
