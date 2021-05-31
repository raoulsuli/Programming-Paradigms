{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node { state :: s,
                       action :: Maybe a,
                       parentNode :: Maybe (Node s a),
                       depthNode :: Int,
                       children :: [Node s a] }
    deriving (Eq, Ord)
{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s 
nodeState node = (state node)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = parentNode node

nodeDepth :: Node s a -> Int
nodeDepth node = depthNode node

nodeAction :: Node s a -> Maybe a
nodeAction node = action node

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = children node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
createChildren :: (ProblemState s a) => s -> a -> Node s a -> Int -> Node s a
createChildren s a parent depth = node
    where node = Node s (Just a) (Just parent) depth succs
          succs = foldl (\x (act,str) -> x ++ [createChildren str act node (depth + 1)]) [] (successors s)

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace stateV = node
    where node = (Node stateV Nothing Nothing 0 succs) 
          succs = foldl (\x (act, str) -> x ++ [createChildren str act node 1]) [] (successors stateV)

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = [([node], [node])] ++ recbfs [node] [node]
    where recbfs visited queue
            | null queue = []
            | otherwise = [(listOfNotVisited, ((tail queue) ++ listOfNotVisited))] ++ recbfs (visited ++ listOfNotVisited) ((tail queue) ++ listOfNotVisited)
                where 
                      current = (head queue)
                      listOfNotVisited = filter (\x -> (not (elem (nodeState x) visitedList))) (nodeChildren current)
                      visitedList = foldl (\x y -> x ++ [(nodeState y)]) [] visited


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}


bidirBFS :: Ord s => Eq a => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS initialNode finalNode = checkIntersection (bfs initialNode) (bfs finalNode) 1

checkIntersection :: Eq a => Eq s => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> Int -> (Node s a, Node s a)
checkIntersection initialList finalList index
    | (element /= Nothing) = (fromJust element)
    | otherwise = checkIntersection initialList finalList (index + 1)
        where element = checkElem (snd (last (take index initialList))) (snd (last (take index finalList)))

checkElem :: Eq a => Eq s => [Node s a] -> [Node s a] -> Maybe (Node s a, Node s a)
checkElem list1 list2
    | ((length list1) == 0) = Nothing
    | (check /= Nothing) = check
    | otherwise = checkElem (tail list1) list2
        where check = checkList (head list1) list2

checkList :: Eq s => Node s a -> [Node s a] -> Maybe (Node s a, Node s a)
checkList item list
    | length list == 0 = Nothing
    | (not (elem (nodeState item) statesList)) = Nothing
    | (nodeState item) == (nodeState (head list)) = Just (item, (head list))
    | otherwise = checkList item (tail list) 
        where statesList = foldl (\x y -> x ++ [(nodeState y)]) [] list


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Eq a => Eq s => Node s a -> [(Maybe a, s)]
extractPath node = reverse list
    where list = createPath node

createPath :: Eq a => Eq s => Node s a -> [(Maybe a, s)]
createPath node 
    | (nodeParent node) == Nothing = [(Nothing, (nodeState node))]
    | otherwise = [((nodeAction node), (nodeState node))] ++ extractPath (fromJust (nodeParent node))


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
