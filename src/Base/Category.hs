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
import Base.BasicSchema (Rose(..))
import Base.TestEntity (catTr1)
import Data.Tree
import qualified Data.Text as T
import Data.Foldable

tt :: Tree String
tt =  Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]] 
-- type Rose = Tree T.Text
testTree :: Rose
testTree =  Node "Abstract" [Node "Man" [Node "Warrior" [Node "Evil" [], Node "Good" [], Node "Neutral" []], Node "Archer" []], Node "Woman" [Node "Witch" []]] 
showcat :: Rose -> IO ()
showcat t = putStr $ drawTree $ fmap show $ t

type OldName = T.Text
type NewName = T.Text
rename :: OldName -> NewName -> Rose -> Rose 
rename oldName newName = fmap (\x -> if x == oldName then newName else x)
