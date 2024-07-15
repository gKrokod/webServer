#!/bin/bash
#
curl -v -X POST login2:qpass2@127.0.0.1:4221/news/edit -H "Content-Type: application/json" -d '{"title":"News from SH script", "newTitle":"Edit News from SH script", "newIsPublish":true,"newLogin":"login3","newLabel":"Man","newContent":"Edit Text about man now","images":[{"imageHeader":"edit image","imageBase64":"edit kartinka for news sh"}]}'

#  Image sql=images
#   header T.Text
#   base64 T.Text
# data EditNewsFromWeb = EditNewsFromWeb {title :: T.Text, newTitle :: Maybe T.Text, 
#   newLogin :: Maybe T.Text, newLabel :: Maybe T.Text, 
#   newContent :: Maybe T.Text,
#   images :: Maybe [Image], newIsPublish :: Maybe Bool }
#   deriving stock (Show, Generic)
#   deriving anyclass (FromJSON)
