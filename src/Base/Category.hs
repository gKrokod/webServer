module Base.Category where
-- Категории:
-- создание только для админов
-- получение списка для всех
-- редактирование только для админов (редактирование названия и смена родительской категории)
--
-- сделай вывод списка существующих в базе категорий
--
-- сделай переименование категории
--
-- сделай смену родительской категорий , выбранной категории, на выбранную родительскую.
-- Node	 
--     rootLabel :: a
--     subForest :: [Tree a]
--
--   Tree a =  Node a [Tree a]
--   impor
import Base.BasicSchema (Rose(..))
import Data.Bool (bool)
import Base.TestEntity (catTr1)
import Data.Tree
import qualified Data.Text as T
import Data.Foldable
import Data.List
import Data.Maybe (listToMaybe)
type OldName = T.Text
type NewName = T.Text
type Parent = T.Text
type NewParent = T.Text
type OldParent = T.Text
type NameCategory = T.Text
type OldRose = Rose
type NewRose = Rose
type AddRose = Rose
--переименовать категорию
renameRose :: OldName -> NewName -> Rose -> NewRose 
renameRose oldName newName = fmap (\x -> if x == oldName then newName else x)
--найди куст с конкретным заголовком и всех его последователей
findRose :: Parent -> OldRose -> Maybe NewRose 
findRose = (listToMaybe .) . helper
  where
    helper :: Parent -> OldRose -> [NewRose]
    helper name (Node x xs) | name == x = pure $ Node x xs
                            | otherwise = concatMap (helper name) xs
--удалить ветку с заданным узлом и всех ее последователей
deleteRose :: Parent -> OldRose -> Maybe NewRose 
deleteRose = (listToMaybe .) . helper
  where
    helper :: Parent -> OldRose -> [NewRose]
    helper name (Node x xs) | name == x = []
                            | otherwise = [Node x (concatMap (helper name) xs)]
-- вставить ветку к заданному родителю
insertRose :: Parent -> AddRose -> OldRose -> NewRose
insertRose name addrose (Node x xs) | name == x = Node x (addrose : xs)  
                                    | otherwise = Node x (map (insertRose name addrose) xs)
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
                  

testTree :: Rose
testTree =  Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]] 

showcat :: Rose -> IO ()
showcat t = putStr $ drawTree $ fmap show $ t

-- -- tt :: Tree String
-- tt =  Node (Just "Abstract") [Node (Just "Man") [Node (Just "Warrior") [Node (Just "Evil") [], Node (Just "Good") [], Node (Just "Neutral") []], Node (Just "Archer") []], Node (Just "Woman") [Node (Just "Witch") []]] 
-- t2, t3 :: Tree (Maybe T.Text)
-- t2 =  Node (Just "Abstract") [Node (Just "Man") [Node (Just "Warrior") [Node (Just "Evil") [], Node (Just "Good") [], Node (Just "Neutral") []], Node (Just "Archer") []], Node (Nothing) [Node (Just "Witch") []]] 
-- t3 =  Node (Just "Abstract") [Node (Nothing ) [Node (Just "Warrior") [Node (Just "Evil") [], Node (Just "Good") [], Node (Just "Neutral") []], Node (Just "Archer") []], Node (Just "woman") [Node (Just "Witch") []]] 
-- -- type Rose = Tree T.Text
--
