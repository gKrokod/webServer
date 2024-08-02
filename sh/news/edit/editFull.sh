#!/bin/bash
#
curl -v -X POST login1:qpass1@127.0.0.1:4221/news/edit -H "Content-Type: application/json" -d '{"title":"News 4 about Evil from user 1", "newTitle":"Edit EDIT EDIT EDIT EDIT News 4", "newIsPublish":true,"newLogin":"login2","newLabel":"Good","newContent":"Edit Text about man now","images":[{"imageHeader":"edit image","imageBase64":"edit kartinka for news sh"}]}'

