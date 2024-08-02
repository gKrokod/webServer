#!/bin/bash
#
curl -v -X POST login2:qpass1@127.0.0.1:4221/news/create -H "Content-Type: application/json" -d '{"title":"1News from SH script","isPublish":true,"login":"login2","label":"Witch","content":"New text about news from sh","images":[{"imageHeader":"image","imageBase64":"kartinka for news sh"},{"imageHeader":"image2 sh","imageBase64":"kartinka for news sh"}]}'
