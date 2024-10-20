module Handlers.Database.Api (createNewsBase, getCopyRight, getImage, getPrivilege, getResultValid, updateCategoryBase, getAllNews, createCategoryBase, updateNewsBase, getAllCategories, getAllUsers, createUserBase) where

import Handlers.Database.Authorization (getCopyRight, getPrivilege, getResultValid)
import Handlers.Database.Category.Create (createCategoryBase)
import Handlers.Database.Category.Get (getAllCategories)
import Handlers.Database.Category.Update (updateCategoryBase)
import Handlers.Database.Image.Get (getImage)
import Handlers.Database.News.Create (createNewsBase)
import Handlers.Database.News.Get (getAllNews)
import Handlers.Database.News.Update (updateNewsBase)
import Handlers.Database.User.Create (createUserBase)
import Handlers.Database.User.Get (getAllUsers)
