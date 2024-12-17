module Database.Api (editNews, putUser, makeHashPassword, migrationEngine, findUserByLogin, validPassword, validCopyRight, pullAllUsers, findCategoryByLabel, putCategory, editCategory, pullAllCategories, pullImage, putNews, findNewsByTitle, pullAllNews, pullOneNews) where

import Database.Authorization (validCopyRight, validPassword)
import Database.Crypto (makeHashPassword)
import Database.Migrations.Migration (migrationEngine)
import Database.Queries.Category (editCategory, findCategoryByLabel, pullAllCategories, putCategory)
import Database.Queries.Image (pullImage)
import Database.Queries.News (editNews, findNewsByTitle, pullAllNews, putNews, pullOneNews)
import Database.Queries.User (findUserByLogin, pullAllUsers, putUser)
