#!/bin/bash
#
curl -v -X POST 127.0.0.1:4221/news/edit?title=GOod+MOrning! -H "Content-Type: application/json" -d '{"category":"Warrior","dataCreated":"17.04.2024","isPublish":false,"textBody":"new text body Warrior Peter drink water","title":"GOod Evening!"}'
