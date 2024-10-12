module Database.Api (editNews, putUser, makeAndFillTables, findUserByLogin, validPassword, validCopyRight, pullAllUsers, findCategoryByLabel, putCategory, editCategory, pullAllCategories, pullImage, putNews, findNewsByTitle, pullAllNews) where

import Database.Authorization (validCopyRight, validPassword)
import Database.Queries.Category (editCategory, findCategoryByLabel, pullAllCategories, putCategory)
import Database.Queries.Image (pullImage)
import Database.Queries.News (editNews, findNewsByTitle, pullAllNews, putNews)
import Database.Queries.User (findUserByLogin, pullAllUsers, putUser)
import Database.Verb (makeAndFillTables)
