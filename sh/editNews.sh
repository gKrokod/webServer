#!/bin/bash
#
curl -v -X POST 127.0.0.1:4221/news/edit?title=GOod+MOrning! -H "Content-Type: application/json" -d '{"category":"Witch","dataCreated":"21.14.1000","isPublish":true,"textBody":"WOman eat bread","title":"GOod Evening!"}'
