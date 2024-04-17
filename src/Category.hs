module Category where
-- Категории:
-- создание только для админов
-- получение списка для всех
-- редактирование только для админов (редактирование названия и смена родительской категории)
-- сделай вывод списка существующих в базе категорий
-- сделай переименование категории
-- сделай смену родительской категорий , выбранной категории, на выбранную родительскую.
--
-- data Tree a = Node	 { 
--     rootLabel :: a
--     subForest :: [Tree a] }
--
--   Tree a =  Node a [Tree a]
import Data.Tree (Tree(..), drawTree )
import qualified Data.Text as T
import qualified Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Bool
--
-- for Categore Dictionary

-- data Category = Category { categoryName :: T.Text } deriving Show
data CategoryDictionary = CategoryDictionary { categoryDictionaryTree :: Rose} deriving Show

type Rose = Tree T.Text
-- instance PersistField Rose where
--   toPersistValue = PersistByteString . BC.toStrict . encode
--   fromPersistValue (PersistByteString t) = maybe (Left "Can't Decode PersistByteString to Tree") Right $ decode $ BC.fromStrict t
--   fromPersistValue x = Left $ "\n" <> T.pack (show x)
-- instance PS.PersistFieldSql Rose where
--   sqlType _ = SqlBlob
type OldName = T.Text
type NewName = T.Text
type Parent = T.Text
type NewParent = T.Text
type OldParent = T.Text
type NameCategory = T.Text
type OldRose = Rose
type NewRose = Rose
type AddRose = Rose

--
--переименовать категорию
renameRose :: OldName -> NewName -> Rose -> NewRose 
renameRose oldName newName = fmap (\x -> if x == oldName then newName else x)
--
--найди куст с конкретным заголовком и всех его последователей
findRose :: Parent -> OldRose -> Maybe NewRose 
findRose = (listToMaybe .) . helper
  where
    helper :: Parent -> OldRose -> [NewRose]
    helper name (Node x xs) | name == x = pure $ Node x xs
                            | otherwise = concatMap (helper name) xs
                          --
--удалить ветку с заданным узлом и всех ее последователей
deleteRose :: Parent -> OldRose -> Maybe NewRose 
deleteRose = (listToMaybe .) . helper
  where
    helper :: Parent -> OldRose -> [NewRose]
    helper name (Node x xs) | name == x = []
                            | otherwise = [Node x (concatMap (helper name) xs)]
                          --
-- вставить ветку к заданному родителю
insertRose :: Parent -> AddRose -> OldRose -> NewRose
insertRose name addrose (Node x xs) | name == x = Node x (addrose : xs)  
                                    | otherwise = Node x (map (insertRose name addrose) xs)
                                  --
-- сменить родительскую категорию у выбранной категорий на  новую выбранную
changeRose :: NameCategory -> NewParent -> OldRose -> NewRose
changeRose name parent rose = case newRose of
  Just newrose -> newrose -- (Node "d" []) --newrose
  Nothing -> rose
  where selectTree = findRose name rose
        roseWithOutSelectTree = deleteRose name rose
        newRose = insertRose <$> ruleName <*> selectTree <*> roseWithOutSelectTree
        ruleName = if name == parent then Nothing
                   else bool Nothing (Just parent) (parent `elem` rose)
-- сделать из дерева список и отсортировать

-- treeToList :: Rose -> [Category] -- [T.Text]
-- treeToList = map Category . Data.List.sort . flatten 
-- вывести структуру категорий (дерева)

showCategory :: Rose -> IO ()
showCategory = putStrLn . drawTree . fmap show

-- cat1, cat2 :: Category
-- cat1 = Category { categoryName = "Man" }
-- cat2 = Category { categoryName = "Woman" }
-- cat3 = Category { categoryName = "Abstract" }
-- cat4 = Category { categoryName = "Warrior" }
-- cat5 = Category { categoryName = "Archer" }
-- cat6 = Category { categoryName = "Witch" }
-- cat7 = Category { categoryName = "Evil" }
-- cat8 = Category { categoryName = "Good" }
-- cat9 = Category { categoryName = "Neutral" }

catTr1 :: CategoryDictionary
catTr1 = CategoryDictionary { categoryDictionaryTree = Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]] }

testTree :: Rose
testTree =  Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]] 
