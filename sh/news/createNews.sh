#!/bin/bash
#
curl -v -X POST 127.0.0.1:4221/news/create -H "Content-Type: application/json" -d '{"title":"News from SH script","isPublish":false,"login":"login1","label":"Witch","content":"New text about news from sh","images":[{"imageHeader":"image","imageBase64":"kartinka for news sh"},{"imageHeader":"image2 sh","imageBase64":"kartinka for news sh"}]}'

# data NewsFromWeb = NewsFromWeb {title :: T.Text, login :: T.Text, label :: T.Text, content :: T.Text,
#                                 images :: [Image], isPublish :: Bool }
#  Image sql=images
#   header T.Text
#   base64 T.Text
