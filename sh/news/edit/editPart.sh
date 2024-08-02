#!/bin/bash
#
curl -v -X POST login1:qpass1@127.0.0.1:4221/news/edit -H "Content-Type: application/json" -d '{"title":"News 1 about Witch from user 1", "newContent":"","images":[]}'

