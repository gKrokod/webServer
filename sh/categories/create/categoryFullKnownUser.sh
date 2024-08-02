#!/bin/bash
#
curl -v -X POST login1:qpass1@127.0.0.1:4221/categories/create -H "Content-Type: application/json" -d '{"label":"Angel","parent":"Abstract"}'
