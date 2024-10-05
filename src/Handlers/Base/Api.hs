module Handlers.Base.Api (createNewsBase, getCopyRight, getImage, getPrivilege, getResultValid, updateCategoryBase, getAllNews, createCategoryBase, updateNewsBase, getAllCategories, getAllUsers, createUserBase) where

import Handlers.Base.Authorization (getCopyRight, getPrivilege, getResultValid)
import Handlers.Base.Category.Create.Api (createCategoryBase)
import Handlers.Base.Category.Get.Api (getAllCategories)
import Handlers.Base.Category.Update.Api (updateCategoryBase)
import Handlers.Base.Image.Get.Api (getImage)
import Handlers.Base.News.Create.Api (createNewsBase)
import Handlers.Base.News.Get.Api (getAllNews)
import Handlers.Base.News.Update.Api (updateNewsBase)
import Handlers.Base.User.Create.Api (createUserBase)
import Handlers.Base.User.Get.Api (getAllUsers)
